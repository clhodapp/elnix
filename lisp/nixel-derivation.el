;;; nixel-derivation.el --- derivationStrict -*- lexical-binding: t; -*-

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
;; `nixel-derivation--drvs' by its `.drv' path, because hashing a
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
(require 'nixel-runtime)
(require 'nixel-store)

(declare-function nixel-to-json-with-context "nixel-json" (v))

(defvar nixel-derivation--drvs (make-hash-table :test 'equal)
  "The derivations of the evaluation by `.drv' path.")

(defvar nixel-derivation--hash-cache (make-hash-table :test 'equal)
  "The hashes modulo of derivations by `.drv' path: alist output to bytes.")

(defvar nixel-derivation--text (make-hash-table :test 'equal)
  "The ATerm text of each derivation by `.drv' path.")

;;;; ATerm

(defun nixel-derivation--quote (s)
  "S as an ATerm string literal."
  (let ((out (list "\"")))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (push (pcase c
                (?\" "\\\"")
                (?\\ "\\\\")
                (?\n "\\n")
                (?\r "\\r")
                (?\t "\\t")
                (_ (unibyte-string c)))
              out)))
    (push "\"" out)
    (apply #'concat (nreverse out))))

(defun nixel-derivation--list (items)
  (concat "[" (mapconcat #'identity items ",") "]"))

(defun nixel-derivation-unparse (drv mask-outputs &optional replace input-drvs)
  "The ATerm of DRV.
With MASK-OUTPUTS the output paths are blank.  With REPLACE, INPUT-DRVS
replaces the input derivations: an alist of (KEY . OUTPUT-NAMES),
sorted."
  (let ((outputs (plist-get drv :outputs))
        (q #'nixel-derivation--quote))
    (concat
     "Derive("
     (nixel-derivation--list
      (mapcar (lambda (o)
                (let ((spec (cdr o)))
                  (concat "(" (funcall q (car o)) ","
                          (funcall q (if mask-outputs "" (aref spec 0))) ","
                          (funcall q (aref spec 1)) ","
                          (funcall q (aref spec 2)) ")")))
              outputs))
     ","
     (nixel-derivation--list
      (mapcar (lambda (i)
                (concat "(" (funcall q (car i)) ","
                        (nixel-derivation--list (mapcar q (cdr i))) ")"))
              (if replace input-drvs (plist-get drv :input-drvs))))
     ","
     (nixel-derivation--list (mapcar q (plist-get drv :input-srcs)))
     "," (funcall q (plist-get drv :platform))
     "," (funcall q (plist-get drv :builder))
     "," (nixel-derivation--list (mapcar q (plist-get drv :args)))
     ","
     (nixel-derivation--list
      (mapcar (lambda (e)
                (concat "(" (funcall q (car e)) ","
                        (funcall q (if (and mask-outputs (assoc (car e) outputs))
                                       ""
                                     (cdr e)))
                        ")"))
              (plist-get drv :env)))
     ")")))

;;;; Hashing modulo

(defun nixel-derivation-hash-modulo (drv-path)
  "The hashes modulo of the derivation at DRV-PATH: alist output to bytes."
  (or (gethash drv-path nixel-derivation--hash-cache)
      (let ((drv (gethash drv-path nixel-derivation--drvs)))
        (unless drv
          (nixel-fail "derivation '%s' is not known to this evaluation" drv-path))
        (puthash drv-path (nixel-derivation--hash-modulo drv nil)
                 nixel-derivation--hash-cache))))

(defun nixel-derivation--hash-modulo (drv mask-outputs)
  (if (plist-get drv :fixed)
      (mapcar
       (lambda (o)
         (let ((spec (cdr o)))
           (cons (car o)
                 (nixel-store-sha256
                  (concat "fixed:out:" (aref spec 1) ":" (aref spec 2) ":"
                          (aref spec 0))))))
       (plist-get drv :outputs))
    (let ((inputs nil))
      (dolist (i (plist-get drv :input-drvs))
        (let ((hashes (nixel-derivation-hash-modulo (car i))))
          (dolist (out (cdr i))
            (let ((h (cdr (assoc out hashes))))
              (unless h
                (nixel-fail "no hash for output '%s' of derivation '%s'"
                            out (plist-get drv :name)))
              (let* ((key (nixel-store-hex h))
                     (cell (assoc key inputs)))
                (if cell
                    (setcdr cell (cons out (cdr cell)))
                  (push (list key out) inputs)))))))
      (setq inputs
            (sort (mapcar (lambda (c) (cons (car c) (sort (delete-dups (cdr c))
                                                          #'string<)))
                          inputs)
                  (lambda (a b) (string< (car a) (car b)))))
      (let ((h (nixel-store-sha256
                (nixel-derivation-unparse drv mask-outputs t inputs))))
        (mapcar (lambda (o) (cons (car o) h)) (plist-get drv :outputs))))))

;;;; derivationStrict

(defun nixel-derivation--closure (path)
  "The store paths PATH refers to, transitively, PATH included."
  (let ((seen (make-hash-table :test 'equal))
        (work (list path)))
    (while work
      (let ((p (pop work)))
        (unless (gethash p seen)
          (puthash p t seen)
          (let ((drv (gethash p nixel-derivation--drvs)))
            (if drv
                (progn
                  (dolist (s (plist-get drv :input-srcs)) (push s work))
                  (dolist (i (plist-get drv :input-drvs)) (push (car i) work)))
              (dolist (r (gethash p nixel-store--references)) (push r work)))))))
    (hash-table-keys seen)))

(defun nixel-derivation--outputs-from (names)
  "Validate the output NAMES and return them."
  (when (null names)
    (nixel-fail "derivation cannot have an empty set of outputs"))
  (let ((seen nil))
    (dolist (n names)
      (when (member n seen)
        (nixel-fail "duplicate derivation output '%s'" n))
      (when (string= n "drv")
        (nixel-fail "invalid derivation output name 'drv'"))
      (push n seen)))
  names)

(defun nixel-derivation-strict (attrs)
  "The result of `builtins.derivationStrict' on the set ATTRS."
  (let* ((tbl (aref attrs 1))
         (get (lambda (k) (let ((x (gethash k tbl 'nixel--missing)))
                            (if (eq x 'nixel--missing) nil x))))
         (name-v (funcall get 'name)))
    (unless name-v
      (nixel-fail "required attribute 'name' missing"))
    (let* ((drv-name (nixel-force-string-no-ctx name-v))
           (ignore-nulls (let ((x (funcall get '__ignoreNulls)))
                           (and x (nixel-force-bool x))))
           (structured (let ((x (funcall get '__structuredAttrs)))
                         (and x (nixel-force-bool x))))
           (json nil)
           (env nil) (ctxs nil) (args nil)
           (builder nil) (platform nil)
           (output-hash nil) (output-hash-algo nil) (output-hash-mode nil)
           (outputs (list "out")))
      (when (string-suffix-p ".drv" drv-name)
        (nixel-fail "derivation names are not allowed to end in '.drv'"))
      (dolist (key (append (nixel-attr-keys attrs) nil))
        (let ((kname (symbol-name key)))
          (unless (member kname '("__ignoreNulls"))
            (let ((v (nixel-force (gethash key tbl))))
              (unless (and ignore-nulls (eq v :null))
                (cond
                 ((string= kname "__contentAddressed")
                  (when (nixel--bool v)
                    (nixel-fail "content-addressed derivations are not supported by nixel")))
                 ((string= kname "__impure")
                  (when (nixel--bool v)
                    (nixel-fail "impure derivations are not supported by nixel")))
                 ((string= kname "args")
                  (mapc (lambda (a)
                          (let ((s (nixel-coerce-to-string (nixel-force a) t t)))
                            (push (nixel--sctx s) ctxs)
                            (push (nixel--sval s) args)))
                        (nixel-force-list v)))
                 ((string= kname "__structuredAttrs"))
                 (structured
                  (let ((j (nixel-to-json-with-context v)))
                    (push (nixel--sctx j) ctxs)
                    (push (cons kname (nixel--sval j)) json))
                  (pcase kname
                    ("builder" (setq builder (nixel-force-string-text v)))
                    ("system" (setq platform (nixel-force-string-text v)))
                    ("outputHash" (setq output-hash (nixel-force-string-text v)))
                    ("outputHashAlgo" (setq output-hash-algo (nixel-force-string-text v)))
                    ("outputHashMode" (setq output-hash-mode (nixel-force-string-text v)))
                    ("outputs"
                     (setq outputs (nixel-derivation--outputs-from
                                    (mapcar #'nixel-force-string-text
                                            (nixel-force-list v)))))))
                 (t
                  (let* ((s (nixel-coerce-to-string v t t))
                         (text (nixel--sval s)))
                    (push (nixel--sctx s) ctxs)
                    (push (cons kname text) env)
                    (pcase kname
                      ("builder" (setq builder text))
                      ("system" (setq platform text))
                      ("outputHash" (setq output-hash text))
                      ("outputHashAlgo" (setq output-hash-algo text))
                      ("outputHashMode" (setq output-hash-mode text))
                      ("outputs"
                       (setq outputs (nixel-derivation--outputs-from
                                      (split-string text "[ \t\n\r]+" t)))))))))))))
      (when structured
        (push (cons "__json" (nixel-derivation--json-object (nreverse json))) env))
      (unless builder (nixel-fail "required attribute 'builder' missing"))
      (unless platform (nixel-fail "required attribute 'system' missing"))
      ;; Inputs from the contexts.
      (let ((input-srcs (make-hash-table :test 'equal))
            (input-drvs (make-hash-table :test 'equal)))
        (dolist (c (apply #'append ctxs))
          (cond
           ((eq (aref c 0) ?=)
            (dolist (p (nixel-derivation--closure (substring c 1)))
              (puthash p t input-srcs)
              (when (string-suffix-p ".drv" p)
                (let ((d (gethash p nixel-derivation--drvs)))
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
                  (nixel-fail "multiple outputs are not supported in fixed-output derivations"))
                (let* ((method (pcase (or output-hash-mode "flat")
                                 ("flat" 'flat)
                                 ("recursive" 'recursive)
                                 ("nar" 'recursive)
                                 (m (nixel-fail "invalid value '%s' for 'outputHashMode' attribute" m))))
                       (parsed (nixel-store-parse-hash
                                output-hash
                                (and output-hash-algo (> (length output-hash-algo) 0)
                                     output-hash-algo)))
                       (algo (car parsed))
                       (bytes (cdr parsed))
                       (out-path (nixel-store-fixed-path drv-name method algo bytes)))
                  (push (cons "out" out-path) env)
                  (setq drv (plist-put drv :fixed t))
                  (setq drv (plist-put drv :outputs
                                       (list (cons "out"
                                                   (vector out-path
                                                           (concat (if (eq method 'recursive) "r:" "")
                                                                   algo)
                                                           (nixel-store-hex bytes))))))
                  (setq drv (plist-put drv :env (nixel-derivation--sort-env env)))))
            ;; Input-addressed: hash with the outputs blank, then fill them.
            (dolist (o outputs) (push (cons o "") env))
            (setq drv (plist-put drv :outputs
                                 (mapcar (lambda (o) (cons o (vector "" "" "")))
                                         sorted-outputs)))
            (setq drv (plist-put drv :env (nixel-derivation--sort-env env)))
            (let* ((h (cdar (nixel-derivation--hash-modulo drv t)))
                   (paths (mapcar (lambda (o)
                                    (cons o (nixel-store-output-path o h drv-name)))
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
          (let* ((text (nixel-derivation-unparse drv nil))
                 (refs (append srcs (mapcar #'car drvs)))
                 (drv-path (nixel-store-text-path (concat drv-name ".drv") text refs)))
            (puthash drv-path drv nixel-derivation--drvs)
            (puthash drv-path text nixel-derivation--text)
            (puthash drv-path refs nixel-store--references)
            (let ((result (list (cons 'drvPath
                                      (nixel--mkstr drv-path
                                                    (list (concat "=" drv-path)))))))
              (dolist (o (plist-get drv :outputs))
                (push (cons (intern (car o))
                            (nixel--mkstr (aref (cdr o) 0)
                                          (list (concat "!" (car o) "!" drv-path))))
                      result))
              (nixel-attrs-from-alist result))))))))

(defun nixel-derivation--sort-env (env)
  "ENV with one entry per name, the last pushed winning, sorted."
  (let ((seen (make-hash-table :test 'equal)) (out nil))
    (dolist (e env)
      (unless (gethash (car e) seen)
        (puthash (car e) t seen)
        (push e out)))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun nixel-derivation--json-object (pairs)
  "The JSON text of an object of PAIRS of a key and the JSON of a value."
  (concat "{"
          (mapconcat (lambda (p)
                       (concat (nixel-json-quote (car p)) ":" (cdr p)))
                     (sort pairs (lambda (a b) (string< (car a) (car b))))
                     ",")
          "}"))

(declare-function nixel-json-quote "nixel-json" (s))

(provide 'nixel-derivation)

;;; nixel-derivation.el ends here
