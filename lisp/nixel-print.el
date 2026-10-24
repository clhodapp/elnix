;;; nixel-print.el --- Print Nix values as nix-instantiate does -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The output format of `nix-instantiate --eval': floats in printf's
;; %g, strings with `"', `\', newline, return, tab and `${' escaped,
;; functions as <LAMBDA>, <PRIMOP> or <PRIMOP-APP>, and a non-empty set
;; or list that was already printed as «repeated».  With STRICT every
;; value is forced; otherwise an unforced thunk prints as <CODE>.

;;; Code:

(require 'cl-lib)
(require 'nixel-runtime)

(defconst nixel-print--reserved
  '("if" "then" "else" "assert" "with" "let" "in" "rec" "inherit"))

(defun nixel-print-string-literal (s)
  "S as a Nix string literal."
  (let ((out (list "\"")) (n (length s)) (i 0))
    (while (< i n)
      (let ((c (aref s i)))
        (push (pcase c
                (?\" "\\\"")
                (?\\ "\\\\")
                (?\n "\\n")
                (?\r "\\r")
                (?\t "\\t")
                (?$ (if (and (< (1+ i) n) (eq (aref s (1+ i)) ?{)) "\\$" "$"))
                (_ (unibyte-string c)))
              out))
      (setq i (1+ i)))
    (push "\"" out)
    (apply #'concat (nreverse out))))

(defun nixel-print--var-name-p (s)
  (and (> (length s) 0)
       (not (member s nixel-print--reserved))
       (not (memq (aref s 0) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?')))
       (string-match-p "\\`[a-zA-Z0-9_'-]+\\'" s)))

(defun nixel-print-attr-name (s)
  (if (nixel-print--var-name-p s) s (nixel-print-string-literal s)))

(defun nixel-print-float (f)
  "F as printf's %g writes it, as Nix prints floats."
  (cond
   ((/= f f) (if (< (copysign 1.0 f) 0) "-nan" "nan"))
   ((= f 1.0e+INF) "inf")
   ((= f -1.0e+INF) "-inf")
   (t (format "%g" f))))

(defun nixel-print-value (v &optional strict)
  "The text of the value V as `nix-instantiate --eval' prints it.
With STRICT, force everything inside V."
  (let ((seen (make-hash-table :test 'eq))
        (out nil))
    (cl-labels
        ((emit (s) (push s out))
         (walk (x)
           (let ((val (if (and (consp x) (not strict)
                               (not (null (car x))))
                          'nixel--unforced
                        (nixel-force x))))
             (cond
              ((eq val 'nixel--unforced) (emit "<CODE>"))
              ((integerp val) (emit (number-to-string val)))
              ((floatp val) (emit (nixel-print-float val)))
              ((eq val t) (emit "true"))
              ((null val) (emit "false"))
              ((eq val :null) (emit "null"))
              ((stringp val) (emit (nixel-print-string-literal val)))
              ((nixel--cstr-p val) (emit (nixel-print-string-literal (aref val 1))))
              ((nixel--path-p val) (emit (aref val 1)))
              ((vectorp val)
               (cond
                ((= (length val) 0) (emit "[ ]"))
                ((gethash val seen) (emit "«repeated»"))
                (t
                 (puthash val t seen)
                 (emit "[ ")
                 (mapc (lambda (e) (walk e) (emit " ")) val)
                 (emit "]"))))
              ((nixel--attrs-p val)
               (cond
                ((= (nixel-attr-count val) 0) (emit "{ }"))
                ((gethash val seen) (emit "«repeated»"))
                (t
                 (puthash val t seen)
                 (emit "{ ")
                 (mapc (lambda (k)
                         (emit (nixel-print-attr-name (symbol-name k)))
                         (emit " = ")
                         (walk (gethash k (aref val 1)))
                         (emit "; "))
                       (nixel-attr-keys val))
                 (emit "}"))))
              ((nixel--primop-p val)
               (emit (if (aref val 3) "<PRIMOP-APP>" "<PRIMOP>")))
              ((nixel-function-p val) (emit "<LAMBDA>"))
              (t (emit (format "<unknown %S>" val)))))))
      (walk v))
    (apply #'concat (nreverse out))))

(provide 'nixel-print)

;;; nixel-print.el ends here
