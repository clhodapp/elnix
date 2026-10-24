;;; nixel-json.el --- toJSON and fromJSON -*- lexical-binding: t; -*-

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
(require 'nixel-runtime)

;;;; Serialising

(defun nixel-json-quote (s)
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

(defun nixel-json--shortest (x)
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

(defun nixel-json--exponent (e)
  (format "e%s%02d" (if (< e 0) "-" "+") (abs e)))

(defun nixel-json-format-float (x)
  "The float X as nlohmann's dump writes it."
  (cond
   ((or (/= x x) (= x 1.0e+INF) (= x -1.0e+INF)) "null")
   ((= x 0.0) (if (< (copysign 1.0 x) 0) "-0.0" "0.0"))
   (t
    (let* ((neg (< x 0))
           (sd (nixel-json--shortest (abs x)))
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
              (concat digits (nixel-json--exponent (1- n))))
             (t
              (concat (substring digits 0 1) "." (substring digits 1)
                      (nixel-json--exponent (1- n)))))))
      (if neg (concat "-" body) body)))))

(defun nixel-to-json-with-context (v)
  "The JSON of the lazy value V, as a string value carrying its context."
  (let ((ctxs nil))
    (cl-labels
        ((walk (x)
           (let ((val (nixel-force x)))
             (cond
              ((eq val :null) "null")
              ((eq val t) "true")
              ((null val) "false")
              ((integerp val) (number-to-string val))
              ((floatp val) (nixel-json-format-float val))
              ((stringp val) (nixel-json-quote val))
              ((nixel--cstr-p val)
               (push (aref val 2) ctxs)
               (nixel-json-quote (aref val 1)))
              ((nixel--path-p val)
               (let ((s (nixel-coerce-to-string val t)))
                 (push (nixel--sctx s) ctxs)
                 (nixel-json-quote (nixel--sval s))))
              ((vectorp val)
               (concat "[" (mapconcat #'walk val ",") "]"))
              ((nixel--attrs-p val)
               (let* ((tbl (aref val 1))
                      (ts (gethash '__toString tbl 'nixel--missing)))
                 (cond
                  ((not (eq ts 'nixel--missing))
                   (let ((s (nixel-coerce-to-string val nil)))
                     (push (nixel--sctx s) ctxs)
                     (nixel-json-quote (nixel--sval s))))
                  ((not (eq (gethash 'outPath tbl 'nixel--missing) 'nixel--missing))
                   (walk (gethash 'outPath tbl)))
                  (t
                   (concat "{"
                           (mapconcat (lambda (k)
                                        (concat (nixel-json-quote (symbol-name k)) ":"
                                                (walk (gethash k tbl))))
                                      (nixel-attr-keys val) ",")
                           "}")))))
              ((nixel-function-p val)
               (nixel-fail "cannot convert a function to JSON"))
              (t (nixel-fail "cannot convert %s to JSON" (nixel-show-type val)))))))
      (let ((s (walk v)))
        (nixel--mkstr s (nixel--ctx-union ctxs))))))

(defun nixel-to-json (v)
  "`builtins.toJSON' of V."
  (nixel-to-json-with-context v))

;;;; Parsing

(defvar nixel-json--s nil)
(defvar nixel-json--i 0)

(defun nixel-json--fail (what)
  (nixel-fail "JSON parse error at byte %d: %s" nixel-json--i what))

(defsubst nixel-json--skip-ws ()
  (let ((n (length nixel-json--s)))
    (while (and (< nixel-json--i n)
                (memq (aref nixel-json--s nixel-json--i) '(?\s ?\t ?\n ?\r)))
      (setq nixel-json--i (1+ nixel-json--i)))))

(defun nixel-json--peek ()
  (and (< nixel-json--i (length nixel-json--s)) (aref nixel-json--s nixel-json--i)))

(defun nixel-json--expect-word (w value)
  (if (eq t (compare-strings nixel-json--s nixel-json--i (+ nixel-json--i (length w))
                             w nil nil))
      (progn (setq nixel-json--i (+ nixel-json--i (length w))) value)
    (nixel-json--fail "unexpected literal")))

(defun nixel-json--utf8 (cp)
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

(defun nixel-json--hex4 ()
  (let ((h (substring nixel-json--s nixel-json--i (min (length nixel-json--s)
                                                       (+ 4 nixel-json--i)))))
    (unless (string-match-p "\\`[0-9a-fA-F]\\{4\\}\\'" h)
      (nixel-json--fail "invalid \\u escape"))
    (setq nixel-json--i (+ 4 nixel-json--i))
    (string-to-number h 16)))

(defun nixel-json--string ()
  ;; At the opening quote.
  (setq nixel-json--i (1+ nixel-json--i))
  (let ((out nil) (n (length nixel-json--s)) (done nil))
    (while (not done)
      (let ((start nixel-json--i))
        (while (and (< nixel-json--i n)
                    (let ((c (aref nixel-json--s nixel-json--i)))
                      (and (not (eq c ?\")) (not (eq c ?\\)) (>= c 32))))
          (setq nixel-json--i (1+ nixel-json--i)))
        (when (> nixel-json--i start)
          (push (substring nixel-json--s start nixel-json--i) out)))
      (let ((c (nixel-json--peek)))
        (cond
         ((null c) (nixel-json--fail "unterminated string"))
         ((eq c ?\") (setq nixel-json--i (1+ nixel-json--i)) (setq done t))
         ((< c 32) (nixel-json--fail "control character in string"))
         (t
          (setq nixel-json--i (1+ nixel-json--i))
          (let ((e (nixel-json--peek)))
            (setq nixel-json--i (1+ nixel-json--i))
            (push (pcase e
                    (?\" "\"") (?\\ "\\") (?/ "/") (?b "\b") (?f "\f")
                    (?n "\n") (?r "\r") (?t "\t")
                    (?u (let ((cp (nixel-json--hex4)))
                          (when (and (>= cp #xd800) (<= cp #xdbff))
                            (unless (and (eq (nixel-json--peek) ?\\)
                                         (eq (aref nixel-json--s (1+ nixel-json--i)) ?u))
                              (nixel-json--fail "missing low surrogate"))
                            (setq nixel-json--i (+ 2 nixel-json--i))
                            (let ((lo (nixel-json--hex4)))
                              (unless (and (>= lo #xdc00) (<= lo #xdfff))
                                (nixel-json--fail "invalid low surrogate"))
                              (setq cp (+ #x10000 (ash (- cp #xd800) 10) (- lo #xdc00)))))
                          (nixel-json--utf8 cp)))
                    (_ (nixel-json--fail "invalid escape")))
                  out))))))
    (apply #'concat (nreverse out))))

(defun nixel-json--number ()
  (let ((start nixel-json--i))
    (unless (string-match "-?\\(?:0\\|[1-9][0-9]*\\)\\(\\.[0-9]+\\)?\\([eE][-+]?[0-9]+\\)?"
                          nixel-json--s nixel-json--i)
      (nixel-json--fail "invalid number"))
    (unless (= (match-beginning 0) start) (nixel-json--fail "invalid number"))
    (setq nixel-json--i (match-end 0))
    (let ((text (match-string 0 nixel-json--s)))
      (if (or (match-beginning 1) (match-beginning 2))
          (string-to-number text)
        (let ((n (string-to-number text)))
          (cond
           ((and (<= n nixel--int-max) (>= n nixel--int-min)) n)
           ((and (> n 0) (<= n 18446744073709551615))
            (nixel-fail "unsigned json number %s outside of Nix integer range" text))
           (t (float n))))))))

(defun nixel-json--key ()
  "Parse an object key and the colon after it."
  (nixel-json--skip-ws)
  (unless (eq (nixel-json--peek) ?\") (nixel-json--fail "expected a key"))
  (let ((k (nixel-json--string)))
    (nixel-json--skip-ws)
    (unless (eq (nixel-json--peek) ?:) (nixel-json--fail "expected ':'"))
    (setq nixel-json--i (1+ nixel-json--i))
    (intern k)))

(defun nixel-json--value ()
  "Parse one JSON value.
Nesting is kept on an explicit stack rather than the Lisp stack, since
Nix accepts documents nested far deeper than Lisp recursion allows.  A
frame is [array ITEMS] or [object TABLE KEY]."
  (let ((stack nil) (result nil) (done nil))
    (while (not done)
      (nixel-json--skip-ws)
      (let ((c (nixel-json--peek)) (val 'nixel--none))
        (cond
         ((null c) (nixel-json--fail "unexpected end of input"))
         ((eq c ?\") (setq val (nixel-json--string)))
         ((eq c ?\[)
          (setq nixel-json--i (1+ nixel-json--i))
          (nixel-json--skip-ws)
          (if (eq (nixel-json--peek) ?\])
              (progn (setq nixel-json--i (1+ nixel-json--i)) (setq val []))
            (push (vector 'array nil) stack)))
         ((eq c ?\{)
          (setq nixel-json--i (1+ nixel-json--i))
          (nixel-json--skip-ws)
          (if (eq (nixel-json--peek) ?\})
              (progn (setq nixel-json--i (1+ nixel-json--i))
                     (setq val (nixel--mkattrs (nixel-make-attrs))))
            (push (vector 'object (nixel-make-attrs) (nixel-json--key)) stack)))
         ((eq c ?t) (setq val (nixel-json--expect-word "true" t)))
         ((eq c ?f) (setq val (nixel-json--expect-word "false" nil)))
         ((eq c ?n) (setq val (nixel-json--expect-word "null" :null)))
         (t (setq val (nixel-json--number))))
        ;; Deliver a finished value to the innermost open container,
        ;; closing every container the value completes.
        (while (not (eq val 'nixel--none))
          (if (null stack)
              (setq result val done t val 'nixel--none)
            (let ((fr (car stack)))
              (if (eq (aref fr 0) 'array)
                  (aset fr 1 (cons val (aref fr 1)))
                (puthash (aref fr 2) val (aref fr 1)))
              (nixel-json--skip-ws)
              (let ((d (nixel-json--peek)))
                (cond
                 ((eq d ?,)
                  (setq nixel-json--i (1+ nixel-json--i))
                  (when (eq (aref fr 0) 'object)
                    (aset fr 2 (nixel-json--key)))
                  (setq val 'nixel--none))
                 ((and (eq d ?\]) (eq (aref fr 0) 'array))
                  (setq nixel-json--i (1+ nixel-json--i))
                  (pop stack)
                  (setq val (vconcat (nreverse (aref fr 1)))))
                 ((and (eq d ?\}) (eq (aref fr 0) 'object))
                  (setq nixel-json--i (1+ nixel-json--i))
                  (pop stack)
                  (setq val (nixel--mkattrs (aref fr 1))))
                 (t (nixel-json--fail "expected ',' or the end of a container")))))))))
    result))

(defun nixel-from-json (s)
  "`builtins.fromJSON' of the unibyte string S."
  (let ((nixel-json--s (nixel--unibyte s))
        (nixel-json--i 0)
        (case-fold-search nil))
    (let ((v (nixel-json--value)))
      (nixel-json--skip-ws)
      (when (< nixel-json--i (length nixel-json--s))
        (nixel-json--fail "trailing data"))
      v)))

(provide 'nixel-json)

;;; nixel-json.el ends here
