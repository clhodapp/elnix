;;; elnix-json.el --- toJSON and fromJSON -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Nix serialises with nlohmann::json and parses with its SAX parser;
;; the bytes here follow both.  `toJSON' keeps object keys in byte
;; order, escapes only the quote, the backslash and control
;; characters, and lays floats out the way nlohmann's dump does: the
;; shortest digits that read back, in fixed notation for decimal
;; exponents in (-4, 15] and scientific notation otherwise, with `.0'
;; on integral values.

;;; Code:

(require 'cl-lib)
(require 'elnix-runtime)

;;;; Serialising

(defun elnix-json-quote (s)
  "S as a JSON string literal."
  (let ((out (list "\"")) (start 0) (n (length s)))
    (dotimes (i n)
      (let ((c (aref s i)))
        (when (or (< c 32) (eq c ?\") (eq c ?\\))
          (when (> i start) (push (substring s start i) out))
          (push (pcase c
                  (?\" "\\\"")
                  (?\\ "\\\\")
                  (?\b "\\b")
                  (?\f "\\f")
                  (?\n "\\n")
                  (?\r "\\r")
                  (?\t "\\t")
                  (_ (format "\\u%04x" c)))
                out)
          (setq start (1+ i)))))
    (when (< start n) (push (substring s start) out))
    (push "\"" out)
    (apply #'concat (nreverse out))))

(defun elnix-json--shortest (x)
  "The shortest decimal digits of the positive float X that read back.
Return (DIGITS . EXPONENT) with X = 0.DIGITS * 10^EXPONENT."
  (let ((p 1) (result nil))
    (while (not result)
      (let ((s (format (format "%%.%de" (1- p)) x)))
        (when (or (= (string-to-number s) x) (>= p 17))
          (string-match "\\`\\([0-9]\\)\\(?:\\.\\([0-9]*\\)\\)?e\\([-+][0-9]+\\)\\'" s)
          (let* ((digits (concat (match-string 1 s) (or (match-string 2 s) "")))
                 (e (string-to-number (match-string 3 s))))
            ;; Drop trailing zeros from the digits.
            (setq digits (replace-regexp-in-string "0+\\'" "" digits))
            (when (string= digits "") (setq digits "0"))
            (setq result (cons digits (1+ e))))))
      (setq p (1+ p)))
    result))

(defun elnix-json--exponent (e)
  (format "e%s%02d" (if (< e 0) "-" "+") (abs e)))

(defun elnix-json-format-float (x)
  "The float X as nlohmann's dump writes it."
  (cond
   ((or (/= x x) (= x 1.0e+INF) (= x -1.0e+INF)) "null")
   ((= x 0.0) (if (< (copysign 1.0 x) 0) "-0.0" "0.0"))
   (t
    (let* ((neg (< x 0))
           (sd (elnix-json--shortest (abs x)))
           (digits (car sd))
           (k (length digits))
           (n (cdr sd))
           (body
            (cond
             ((and (<= k n) (<= n 15))
              (concat digits (make-string (- n k) ?0) ".0"))
             ((and (< 0 n) (<= n 15))
              (concat (substring digits 0 n) "." (substring digits n)))
             ((and (< -4 n) (<= n 0))
              (concat "0." (make-string (- n) ?0) digits))
             ((= k 1)
              (concat digits (elnix-json--exponent (1- n))))
             (t
              (concat (substring digits 0 1) "." (substring digits 1)
                      (elnix-json--exponent (1- n)))))))
      (if neg (concat "-" body) body)))))

(defun elnix-to-json-with-context (v)
  "The JSON of the lazy value V, as a string value carrying its context."
  (let ((ctxs nil))
    (cl-labels
        ((walk (x)
           (let ((val (elnix-force x)))
             (cond
              ((eq val :null) "null")
              ((eq val t) "true")
              ((null val) "false")
              ((integerp val) (number-to-string val))
              ((floatp val) (elnix-json-format-float val))
              ((stringp val) (elnix-json-quote val))
              ((elnix--cstr-p val)
               (push (aref val 2) ctxs)
               (elnix-json-quote (aref val 1)))
              ((elnix--path-p val)
               (let ((s (elnix-coerce-to-string val t)))
                 (push (elnix--sctx s) ctxs)
                 (elnix-json-quote (elnix--sval s))))
              ((vectorp val)
               (concat "[" (mapconcat #'walk val ",") "]"))
              ((elnix--attrs-p val)
               (let* ((tbl (aref val 1))
                      (ts (gethash '__toString tbl 'elnix--missing)))
                 (cond
                  ((not (eq ts 'elnix--missing))
                   (let ((s (elnix-coerce-to-string val nil)))
                     (push (elnix--sctx s) ctxs)
                     (elnix-json-quote (elnix--sval s))))
                  ((not (eq (gethash 'outPath tbl 'elnix--missing) 'elnix--missing))
                   (walk (gethash 'outPath tbl)))
                  (t
                   (concat "{"
                           (mapconcat (lambda (k)
                                        (concat (elnix-json-quote (symbol-name k)) ":"
                                                (walk (gethash k tbl))))
                                      (elnix-attr-keys val) ",")
                           "}")))))
              ((elnix-function-p val)
               (elnix-fail "cannot convert a function to JSON"))
              (t (elnix-fail "cannot convert %s to JSON" (elnix-show-type val)))))))
      (let ((s (walk v)))
        (elnix--mkstr s (elnix--ctx-union ctxs))))))

(defun elnix-to-json (v)
  "`builtins.toJSON' of V."
  (elnix-to-json-with-context v))

;;;; Parsing

(defvar elnix-json--s nil)
(defvar elnix-json--i 0)

(defun elnix-json--fail (what)
  (elnix-fail "JSON parse error at byte %d: %s" elnix-json--i what))

(defsubst elnix-json--skip-ws ()
  (let ((n (length elnix-json--s)))
    (while (and (< elnix-json--i n)
                (memq (aref elnix-json--s elnix-json--i) '(?\s ?\t ?\n ?\r)))
      (setq elnix-json--i (1+ elnix-json--i)))))

(defun elnix-json--peek ()
  (and (< elnix-json--i (length elnix-json--s)) (aref elnix-json--s elnix-json--i)))

(defun elnix-json--expect-word (w value)
  (if (eq t (compare-strings elnix-json--s elnix-json--i (+ elnix-json--i (length w))
                             w nil nil))
      (progn (setq elnix-json--i (+ elnix-json--i (length w))) value)
    (elnix-json--fail "unexpected literal")))

(defun elnix-json--utf8 (cp)
  "The UTF-8 bytes of the code point CP."
  (cond
   ((< cp #x80) (unibyte-string cp))
   ((< cp #x800) (unibyte-string (logior #xc0 (ash cp -6))
                                 (logior #x80 (logand cp #x3f))))
   ((< cp #x10000) (unibyte-string (logior #xe0 (ash cp -12))
                                   (logior #x80 (logand (ash cp -6) #x3f))
                                   (logior #x80 (logand cp #x3f))))
   (t (unibyte-string (logior #xf0 (ash cp -18))
                      (logior #x80 (logand (ash cp -12) #x3f))
                      (logior #x80 (logand (ash cp -6) #x3f))
                      (logior #x80 (logand cp #x3f))))))

(defun elnix-json--hex4 ()
  (let ((h (substring elnix-json--s elnix-json--i (min (length elnix-json--s)
                                                       (+ 4 elnix-json--i)))))
    (unless (string-match-p "\\`[0-9a-fA-F]\\{4\\}\\'" h)
      (elnix-json--fail "invalid \\u escape"))
    (setq elnix-json--i (+ 4 elnix-json--i))
    (string-to-number h 16)))

(defun elnix-json--string ()
  ;; At the opening quote.
  (setq elnix-json--i (1+ elnix-json--i))
  (let ((out nil) (n (length elnix-json--s)) (done nil))
    (while (not done)
      (let ((start elnix-json--i))
        (while (and (< elnix-json--i n)
                    (let ((c (aref elnix-json--s elnix-json--i)))
                      (and (not (eq c ?\")) (not (eq c ?\\)) (>= c 32))))
          (setq elnix-json--i (1+ elnix-json--i)))
        (when (> elnix-json--i start)
          (push (substring elnix-json--s start elnix-json--i) out)))
      (let ((c (elnix-json--peek)))
        (cond
         ((null c) (elnix-json--fail "unterminated string"))
         ((eq c ?\") (setq elnix-json--i (1+ elnix-json--i)) (setq done t))
         ((< c 32) (elnix-json--fail "control character in string"))
         (t
          (setq elnix-json--i (1+ elnix-json--i))
          (let ((e (elnix-json--peek)))
            (setq elnix-json--i (1+ elnix-json--i))
            (push (pcase e
                    (?\" "\"") (?\\ "\\") (?/ "/") (?b "\b") (?f "\f")
                    (?n "\n") (?r "\r") (?t "\t")
                    (?u (let ((cp (elnix-json--hex4)))
                          (when (and (>= cp #xd800) (<= cp #xdbff))
                            (unless (and (eq (elnix-json--peek) ?\\)
                                         (eq (aref elnix-json--s (1+ elnix-json--i)) ?u))
                              (elnix-json--fail "missing low surrogate"))
                            (setq elnix-json--i (+ 2 elnix-json--i))
                            (let ((lo (elnix-json--hex4)))
                              (unless (and (>= lo #xdc00) (<= lo #xdfff))
                                (elnix-json--fail "invalid low surrogate"))
                              (setq cp (+ #x10000 (ash (- cp #xd800) 10) (- lo #xdc00)))))
                          (elnix-json--utf8 cp)))
                    (_ (elnix-json--fail "invalid escape")))
                  out))))))
    (apply #'concat (nreverse out))))

(defun elnix-json--number ()
  (let ((start elnix-json--i))
    (unless (string-match "-?\\(?:0\\|[1-9][0-9]*\\)\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?"
                          elnix-json--s elnix-json--i)
      (elnix-json--fail "invalid number"))
    (unless (= (match-beginning 0) start) (elnix-json--fail "invalid number"))
    (setq elnix-json--i (match-end 0))
    (let ((text (match-string 0 elnix-json--s)))
      (if (or (match-beginning 1) (match-beginning 2))
          (string-to-number text)
        (let ((n (string-to-number text)))
          (cond
           ((and (<= n elnix--int-max) (>= n elnix--int-min)) n)
           ((and (> n 0) (<= n 18446744073709551615))
            (elnix-fail "unsigned json number %s outside of Nix integer range" text))
           (t (float n))))))))

(defun elnix-json--key ()
  "Parse an object key and the colon after it."
  (elnix-json--skip-ws)
  (unless (eq (elnix-json--peek) ?\") (elnix-json--fail "expected a key"))
  (let ((k (elnix-json--string)))
    (elnix-json--skip-ws)
    (unless (eq (elnix-json--peek) ?:) (elnix-json--fail "expected ':'"))
    (setq elnix-json--i (1+ elnix-json--i))
    (intern k)))

(defun elnix-json--value ()
  "Parse one JSON value.
Nesting is kept on an explicit stack rather than the Lisp stack, since
Nix accepts documents nested far deeper than Lisp recursion allows.  A
frame is [array ITEMS] or [object TABLE KEY]."
  (let ((stack nil) (result nil) (done nil))
    (while (not done)
      (elnix-json--skip-ws)
      (let ((c (elnix-json--peek)) (val 'elnix--none))
        (cond
         ((null c) (elnix-json--fail "unexpected end of input"))
         ((eq c ?\") (setq val (elnix-json--string)))
         ((eq c ?\[)
          (setq elnix-json--i (1+ elnix-json--i))
          (elnix-json--skip-ws)
          (if (eq (elnix-json--peek) ?\])
              (progn (setq elnix-json--i (1+ elnix-json--i)) (setq val []))
            (push (vector 'array nil) stack)))
         ((eq c ?\{)
          (setq elnix-json--i (1+ elnix-json--i))
          (elnix-json--skip-ws)
          (if (eq (elnix-json--peek) ?\})
              (progn (setq elnix-json--i (1+ elnix-json--i))
                     (setq val (elnix--mkattrs (elnix-make-attrs))))
            (push (vector 'object (elnix-make-attrs) (elnix-json--key)) stack)))
         ((eq c ?t) (setq val (elnix-json--expect-word "true" t)))
         ((eq c ?f) (setq val (elnix-json--expect-word "false" nil)))
         ((eq c ?n) (setq val (elnix-json--expect-word "null" :null)))
         (t (setq val (elnix-json--number))))
        ;; Deliver a finished value to the innermost open container,
        ;; closing every container the value completes.
        (while (not (eq val 'elnix--none))
          (if (null stack)
              (setq result val done t val 'elnix--none)
            (let ((fr (car stack)))
              (if (eq (aref fr 0) 'array)
                  (aset fr 1 (cons val (aref fr 1)))
                (puthash (aref fr 2) val (aref fr 1)))
              (elnix-json--skip-ws)
              (let ((d (elnix-json--peek)))
                (cond
                 ((eq d ?,)
                  (setq elnix-json--i (1+ elnix-json--i))
                  (when (eq (aref fr 0) 'object)
                    (aset fr 2 (elnix-json--key)))
                  (setq val 'elnix--none))
                 ((and (eq d ?\]) (eq (aref fr 0) 'array))
                  (setq elnix-json--i (1+ elnix-json--i))
                  (pop stack)
                  (setq val (vconcat (nreverse (aref fr 1)))))
                 ((and (eq d ?\}) (eq (aref fr 0) 'object))
                  (setq elnix-json--i (1+ elnix-json--i))
                  (pop stack)
                  (setq val (elnix--mkattrs (aref fr 1))))
                 (t (elnix-json--fail "expected ',' or the end of a container")))))))))
    result))

(defun elnix-from-json (s)
  "`builtins.fromJSON' of the unibyte string S."
  (let ((elnix-json--s (elnix--unibyte s))
        (elnix-json--i 0)
        (case-fold-search nil))
    (let ((v (elnix-json--value)))
      (elnix-json--skip-ws)
      (when (< elnix-json--i (length elnix-json--s))
        (elnix-json--fail "trailing data"))
      v)))

(provide 'elnix-json)

;;; elnix-json.el ends here
