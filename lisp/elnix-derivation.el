;;; elnix-derivation.el --- derivationStrict -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `derivationStrict' turns a set of attributes into a derivation: it
;; coerces each attribute to a string by the derivation rules, collects
;; the inputs from the string contexts, serialises the ATerm, hashes it
;; modulo its fixed-output inputs and computes the output paths and the
;; path of the `.drv' file.  Every rule follows prim_derivationStrict in
;; libexpr/primops.cc and libstore/derivations.cc of Nix.
;;
;; Every derivation an evaluation produces is kept in
;; `elnix-derivation--drvs' by its `.drv' path, because hashing a
;; derivation needs the hash of each input derivation.
;;
;; A derivation is a plist:
;;   :name NAME
;;   :outputs  alist (OUTPUT . [PATH HASH-ALGO HASH]) sorted by output
;;   :input-drvs alist (DRV-PATH . OUTPUT-NAMES) sorted
;;   :input-srcs sorted list of paths
;;   :platform :builder :args (list of strings)
;;   :env alist (NAME . VALUE) sorted
;;   :fixed non-nil for a fixed-output derivation

;;; Code:

(require 'cl-lib)
(require 'elnix-runtime)
(require 'elnix-store)

(declare-function elnix-to-json-with-context "elnix-json" (v))

(defvar elnix-derivation--drvs (make-hash-table :test 'equal)
  "The derivations of the evaluation by `.drv' path.")

(defvar elnix-derivation--hash-cache (make-hash-table :test 'equal)
  "The hashes modulo of derivations by `.drv' path: alist output to bytes.")

(defvar elnix-derivation--text (make-hash-table :test 'equal)
  "The ATerm text of each derivation by `.drv' path.")

;;;; ATerm

(defun elnix-derivation--quote (s)
  "S as an ATerm string literal."
  ;; S is unibyte; copying runs with `substring' keeps every byte as it
  ;; is, where `string' would read a byte above 127 as a Latin-1
  ;; character and encode it again.
  (let* ((s (elnix--unibyte s))
         (out (list "\"")) (start 0) (n (length s)))
    (dotimes (i n)
      (let ((esc (pcase (aref s i)
                   (?\" "\\\"") (?\\ "\\\\") (?\n "\\n") (?\r "\\r") (?\t "\\t"))))
        (when esc
          (when (> i start) (push (substring s start i) out))
          (push esc out)
          (setq start (1+ i)))))
    (when (< start n) (push (substring s start) out))
    (push "\"" out)
    (apply #'concat (nreverse out))))

(defun elnix-derivation--list (items)
  (concat "[" (mapconcat #'identity items ",") "]"))

(defun elnix-derivation-unparse (drv mask-outputs &optional replace input-drvs)
  "The ATerm of DRV.
With MASK-OUTPUTS the output paths are blank.  With REPLACE, INPUT-DRVS
replaces the input derivations: an alist of (KEY . OUTPUT-NAMES),
sorted."
  (let ((outputs (plist-get drv :outputs))
        (q #'elnix-derivation--quote))
    (concat
     "Derive("
     (elnix-derivation--list
      (mapcar (lambda (o)
                (let ((spec (cdr o)))
                  (concat "(" (funcall q (car o)) ","
                          (funcall q (if mask-outputs "" (aref spec 0))) ","
                          (funcall q (aref spec 1)) ","
                          (funcall q (aref spec 2)) ")")))
              outputs))
     ","
     (elnix-derivation--list
      (mapcar (lambda (i)
                (concat "(" (funcall q (car i)) ","
                        (elnix-derivation--list (mapcar q (cdr i))) ")"))
              (if replace input-drvs (plist-get drv :input-drvs))))
     ","
     (elnix-derivation--list (mapcar q (plist-get drv :input-srcs)))
     "," (funcall q (plist-get drv :platform))
     "," (funcall q (plist-get drv :builder))
     "," (elnix-derivation--list (mapcar q (plist-get drv :args)))
     ","
     (elnix-derivation--list
      (mapcar (lambda (e)
                (concat "(" (funcall q (car e)) ","
                        (funcall q (if (and mask-outputs (assoc (car e) outputs))
                                       ""
                                     (cdr e)))
                        ")"))
              (plist-get drv :env)))
     ")")))

;;;; Hashing modulo

(defun elnix-derivation-hash-modulo (drv-path)
  "The hashes modulo of the derivation at DRV-PATH: alist output to bytes."
  (or (gethash drv-path elnix-derivation--hash-cache)
      (let ((drv (gethash drv-path elnix-derivation--drvs)))
        (unless drv
          (elnix-fail "derivation '%s' is not known to this evaluation" drv-path))
        (puthash drv-path (elnix-derivation--hash-modulo drv nil)
                 elnix-derivation--hash-cache))))

(defun elnix-derivation--hash-modulo (drv mask-outputs)
  (if (plist-get drv :fixed)
      (mapcar
       (lambda (o)
         (let ((spec (cdr o)))
           (cons (car o)
                 (elnix-store-sha256
                  (concat "fixed:out:" (aref spec 1) ":" (aref spec 2) ":"
                          (aref spec 0))))))
       (plist-get drv :outputs))
    (let ((inputs nil))
      (dolist (i (plist-get drv :input-drvs))
        (let ((hashes (elnix-derivation-hash-modulo (car i))))
          (dolist (out (cdr i))
            (let ((h (cdr (assoc out hashes))))
              (unless h
                (elnix-fail "no hash for output '%s' of derivation '%s'"
                            out (plist-get drv :name)))
              (let* ((key (elnix-store-hex h))
                     (cell (assoc key inputs)))
                (if cell
                    (setcdr cell (cons out (cdr cell)))
                  (push (list key out) inputs)))))))
      (setq inputs
            (sort (mapcar (lambda (c) (cons (car c) (sort (delete-dups (cdr c))
                                                          #'string<)))
                          inputs)
                  (lambda (a b) (string< (car a) (car b)))))
      (let ((h (elnix-store-sha256
                (elnix-derivation-unparse drv mask-outputs t inputs))))
        (mapcar (lambda (o) (cons (car o) h)) (plist-get drv :outputs))))))

;;;; derivationStrict

(defun elnix-derivation--closure (path)
  "The store paths PATH refers to, transitively, PATH included."
  (let ((seen (make-hash-table :test 'equal))
        (work (list path)))
    (while work
      (let ((p (pop work)))
        (unless (gethash p seen)
          (puthash p t seen)
          (let ((drv (gethash p elnix-derivation--drvs)))
            (if drv
                (progn
                  (dolist (s (plist-get drv :input-srcs)) (push s work))
                  (dolist (i (plist-get drv :input-drvs)) (push (car i) work)))
              (dolist (r (gethash p elnix-store--references)) (push r work)))))))
    (hash-table-keys seen)))

(defun elnix-derivation--outputs-from (names)
  "Validate the output NAMES and return them."
  (when (null names)
    (elnix-fail "derivation cannot have an empty set of outputs"))
  (let ((seen nil))
    (dolist (n names)
      (when (member n seen)
        (elnix-fail "duplicate derivation output '%s'" n))
      (push n seen)))
  names)

(defun elnix-derivation-strict (attrs)
  "The result of `builtins.derivationStrict' on the set ATTRS."
  (let* ((tbl (aref attrs 1))
         (get (lambda (k) (let ((x (gethash k tbl 'elnix--missing)))
                            (if (eq x 'elnix--missing) nil x))))
         (name-v (funcall get 'name)))
    (unless name-v
      (elnix-fail "required attribute 'name' missing"))
    (let* ((drv-name (elnix-force-string-no-ctx name-v))
           (ignore-nulls (let ((x (funcall get '__ignoreNulls)))
                           (and x (elnix-force-bool x))))
           (structured (let ((x (funcall get '__structuredAttrs)))
                         (and x (elnix-force-bool x))))
           (json nil)
           (env nil) (ctxs nil) (args nil)
           (builder nil) (platform nil)
           (output-hash nil) (output-hash-algo nil) (output-hash-mode nil)
           (outputs (list "out")))
      (when (string-suffix-p ".drv" drv-name)
        (elnix-fail "derivation names are not allowed to end in '.drv'"))
      (dolist (key (append (elnix-attr-keys attrs) nil))
        (let ((kname (symbol-name key)))
          (unless (member kname '("__ignoreNulls"))
            (let ((v (elnix-force (gethash key tbl))))
              (unless (and ignore-nulls (eq v :null))
                (cond
                 ((string= kname "__contentAddressed")
                  (when (elnix--bool v)
                    (elnix-fail "content-addressed derivations are not supported by elnix")))
                 ((string= kname "__impure")
                  (when (elnix--bool v)
                    (elnix-fail "impure derivations are not supported by elnix")))
                 ((string= kname "args")
                  (mapc (lambda (a)
                          (let ((s (elnix-coerce-to-string (elnix-force a) t t)))
                            (push (elnix--sctx s) ctxs)
                            (push (elnix--sval s) args)))
                        (elnix-force-list v)))
                 ;; Only a true __structuredAttrs stays out of the
                 ;; environment; false is coerced like any attribute.
                 ((and (string= kname "__structuredAttrs") structured))
                 (structured
                  (let ((j (elnix-to-json-with-context v)))
                    (push (elnix--sctx j) ctxs)
                    (push (cons kname (elnix--sval j)) json))
                  (pcase kname
                    ("builder" (setq builder (elnix-force-string-text v)))
                    ("system" (setq platform (elnix-force-string-text v)))
                    ("outputHash" (setq output-hash (elnix-force-string-text v)))
                    ("outputHashAlgo" (setq output-hash-algo (elnix-force-string-text v)))
                    ("outputHashMode" (setq output-hash-mode (elnix-force-string-text v)))
                    ("outputs"
                     (setq outputs (elnix-derivation--outputs-from
                                    (mapcar #'elnix-force-string-text
                                            (elnix-force-list v)))))))
                 (t
                  (let* ((s (elnix-coerce-to-string v t t))
                         (text (elnix--sval s)))
                    (push (elnix--sctx s) ctxs)
                    (push (cons kname text) env)
                    (pcase kname
                      ("builder" (setq builder text))
                      ("system" (setq platform text))
                      ("outputHash" (setq output-hash text))
                      ("outputHashAlgo" (setq output-hash-algo text))
                      ("outputHashMode" (setq output-hash-mode text))
                      ("outputs"
                       (setq outputs (elnix-derivation--outputs-from
                                      (split-string text "[ \t\n\r]+" t)))))))))))))
      (when structured
        (push (cons "__json" (elnix-derivation--json-object (nreverse json))) env))
      (unless builder (elnix-fail "required attribute 'builder' missing"))
      (unless platform (elnix-fail "required attribute 'system' missing"))
      ;; Inputs from the contexts.
      (let ((input-srcs (make-hash-table :test 'equal))
            (input-drvs (make-hash-table :test 'equal)))
        (dolist (c (apply #'append ctxs))
          (cond
           ((eq (aref c 0) ?=)
            (dolist (p (elnix-derivation--closure (substring c 1)))
              (puthash p t input-srcs)
              (when (string-suffix-p ".drv" p)
                (let ((d (gethash p elnix-derivation--drvs)))
                  (dolist (o (plist-get d :outputs))
                    (puthash p (cons (car o) (gethash p input-drvs)) input-drvs))))))
           ((eq (aref c 0) ?!)
            (let* ((i (cl-position ?! c :start 1))
                   (out (substring c 1 i))
                   (p (substring c (1+ i))))
              (puthash p (cons out (gethash p input-drvs)) input-drvs)))
           (t (puthash c t input-srcs))))
        (let* ((srcs (sort (hash-table-keys input-srcs) #'string<))
               (drvs (sort (mapcar (lambda (k)
                                     (cons k (sort (delete-dups
                                                    (copy-sequence
                                                     (gethash k input-drvs)))
                                                   #'string<)))
                                   (hash-table-keys input-drvs))
                           (lambda (a b) (string< (car a) (car b)))))
               (sorted-outputs (sort (copy-sequence outputs) #'string<))
               (drv (list :name drv-name :input-drvs drvs :input-srcs srcs
                          :platform platform :builder builder
                          :args (nreverse args))))
          (if output-hash
              (progn
                (unless (equal outputs '("out"))
                  (elnix-fail "multiple outputs are not supported in fixed-output derivations"))
                (let* ((method (pcase (or output-hash-mode "flat")
                                 ("flat" 'flat)
                                 ("recursive" 'recursive)
                                 ("nar" 'recursive)
                                 (m (elnix-fail "invalid value '%s' for 'outputHashMode' attribute" m))))
                       (parsed (elnix-store-parse-hash
                                output-hash
                                (and output-hash-algo (> (length output-hash-algo) 0)
                                     output-hash-algo)))
                       (algo (car parsed))
                       (bytes (cdr parsed))
                       (out-path (elnix-store-fixed-path drv-name method algo bytes)))
                  (push (cons "out" out-path) env)
                  (setq drv (plist-put drv :fixed t))
                  (setq drv (plist-put drv :outputs
                                       (list (cons "out"
                                                   (vector out-path
                                                           (concat (if (eq method 'recursive) "r:" "")
                                                                   algo)
                                                           (elnix-store-hex bytes))))))
                  (setq drv (plist-put drv :env (elnix-derivation--sort-env env)))))
            ;; Input-addressed: hash with the outputs blank, then fill them.
            (dolist (o outputs) (push (cons o "") env))
            (setq drv (plist-put drv :outputs
                                 (mapcar (lambda (o) (cons o (vector "" "" "")))
                                         sorted-outputs)))
            (setq drv (plist-put drv :env (elnix-derivation--sort-env env)))
            (let* ((h (cdar (elnix-derivation--hash-modulo drv t)))
                   (paths (mapcar (lambda (o)
                                    (cons o (elnix-store-output-path o h drv-name)))
                                  sorted-outputs)))
              (setq drv (plist-put drv :outputs
                                   (mapcar (lambda (p) (cons (car p) (vector (cdr p) "" "")))
                                           paths)))
              (setq drv (plist-put drv :env
                                   (mapcar (lambda (e)
                                             (let ((p (assoc (car e) paths)))
                                               (if p (cons (car e) (cdr p)) e)))
                                           (plist-get drv :env))))))
          ;; Write the derivation.
          (let* ((text (elnix-derivation-unparse drv nil))
                 (refs (append srcs (mapcar #'car drvs)))
                 (drv-path (elnix-store-text-path (concat drv-name ".drv") text refs)))
            (puthash drv-path drv elnix-derivation--drvs)
            (puthash drv-path text elnix-derivation--text)
            (puthash drv-path refs elnix-store--references)
            (let ((result (list (cons 'drvPath
                                      (elnix--mkstr drv-path
                                                    (list (concat "=" drv-path)))))))
              (dolist (o (plist-get drv :outputs))
                (push (cons (intern (car o))
                            (elnix--mkstr (aref (cdr o) 0)
                                          (list (concat "!" (car o) "!" drv-path))))
                      result))
              (elnix-attrs-from-alist result))))))))

(defun elnix-derivation--sort-env (env)
  "ENV with one entry per name, the last pushed winning, sorted."
  (let ((seen (make-hash-table :test 'equal)) (out nil))
    (dolist (e env)
      (unless (gethash (car e) seen)
        (puthash (car e) t seen)
        (push e out)))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun elnix-derivation--json-object (pairs)
  "The JSON text of an object of PAIRS of a key and the JSON of a value."
  (concat "{"
          (mapconcat (lambda (p)
                       (concat (elnix-json-quote (car p)) ":" (cdr p)))
                     (sort pairs (lambda (a b) (string< (car a) (car b))))
                     ",")
          "}"))

(declare-function elnix-json-quote "elnix-json" (s))

(provide 'elnix-derivation)

;;; elnix-derivation.el ends here
