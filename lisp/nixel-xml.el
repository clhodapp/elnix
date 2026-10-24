;;; nixel-xml.el --- builtins.toXML -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The XML that `builtins.toXML' writes: a header line, a root <expr>
;; element, two spaces of indentation per level, element attributes in
;; name order, and attribute values with `"', `<', `>', `&' and newline
;; escaped.  A derivation is written once; later appearances of the
;; same `.drv' path are <repeated />.

;;; Code:

(require 'cl-lib)
(require 'nixel-runtime)

(declare-function nixel-print-float "nixel-print" (f))

(defun nixel-xml--escape (s)
  (let ((out nil))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (push (pcase c
                (?\" "&quot;") (?< "&lt;") (?> "&gt;") (?& "&amp;")
                (?\n "&#xA;")
                (_ (unibyte-string c)))
              out)))
    (apply #'concat (nreverse out))))

(defun nixel-to-xml (v)
  "`builtins.toXML' of the value V, as a string value with its context."
  (let ((out nil) (depth 0) (ctxs nil)
        (drvs-seen (make-hash-table :test 'equal)))
    (cl-labels
        ((attrs-text (attrs)
           (mapconcat (lambda (a) (format " %s=\"%s\"" (car a) (nixel-xml--escape (cdr a))))
                      (sort attrs (lambda (x y) (string< (car x) (car y)))) ""))
         (indent () (push (make-string (* 2 depth) ?\s) out))
         (open (name &optional attrs)
           (indent)
           (push (format "<%s%s>\n" name (attrs-text attrs)) out)
           (setq depth (1+ depth)))
         (close (name)
           (setq depth (1- depth))
           (indent)
           (push (format "</%s>\n" name) out))
         (empty (name &optional attrs)
           (indent)
           (push (format "<%s%s />\n" name (attrs-text attrs)) out))
         (show-attrs (a)
           (dolist (k (append (nixel-attr-keys a) nil))
             (open "attr" (list (cons "name" (symbol-name k))))
             (walk (gethash k (aref a 1)))
             (close "attr")))
         (walk (x)
           (let ((val (nixel-force x)))
             (cond
              ((stringp val) (empty "string" (list (cons "value" val))))
              ((nixel--cstr-p val)
               (push (aref val 2) ctxs)
               (empty "string" (list (cons "value" (aref val 1)))))
              ((integerp val) (empty "int" (list (cons "value" (number-to-string val)))))
              ((floatp val) (empty "float" (list (cons "value" (nixel-print-float val)))))
              ((eq val t) (empty "bool" (list (cons "value" "true"))))
              ((null val) (empty "bool" (list (cons "value" "false"))))
              ((eq val :null) (empty "null"))
              ((nixel--path-p val) (empty "path" (list (cons "value" (aref val 1)))))
              ((vectorp val)
               (open "list")
               (mapc #'walk val)
               (close "list"))
              ((nixel--attrs-p val)
               (if (nixel--is-derivation val)
                   (let* ((tbl (aref val 1))
                          (attrs nil)
                          (drv-path nil))
                     (let ((d (gethash 'drvPath tbl 'nixel--missing)))
                       (unless (eq d 'nixel--missing)
                         (let ((dv (nixel-force d)))
                           (when (nixel-string-p dv)
                             (setq drv-path (nixel--sval dv))
                             (push (cons "drvPath" drv-path) attrs)))))
                     (let ((o (gethash 'outPath tbl 'nixel--missing)))
                       (unless (eq o 'nixel--missing)
                         (let ((ov (nixel-force o)))
                           (when (nixel-string-p ov)
                             (push (cons "outPath" (nixel--sval ov)) attrs)))))
                     (open "derivation" attrs)
                     (if (and drv-path (not (gethash drv-path drvs-seen)))
                         (progn
                           (puthash drv-path t drvs-seen)
                           (show-attrs val))
                       (empty "repeated"))
                     (close "derivation"))
                 (open "attrs")
                 (show-attrs val)
                 (close "attrs")))
              ((nixel--lam-p val)
               (open "function")
               (let ((attrs (and (aref val 3) (list (cons "ellipsis" "1")))))
                 (open "attrspat" attrs)
                 (dolist (f (sort (mapcar (lambda (f) (symbol-name (car f))) (aref val 2))
                                  #'string<))
                   (empty "attr" (list (cons "name" f))))
                 (close "attrspat"))
               (close "function"))
              ((nixel-function-p val) (empty "unevaluated"))))))
      (push "<?xml version='1.0' encoding='utf-8'?>\n" out)
      (open "expr")
      (walk v)
      (close "expr"))
    (nixel--mkstr (apply #'concat (nreverse out)) (nixel--ctx-union ctxs))))

(provide 'nixel-xml)

;;; nixel-xml.el ends here
