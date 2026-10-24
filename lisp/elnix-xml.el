;;; elnix-xml.el --- builtins.toXML -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The XML that `builtins.toXML' writes: a header line, a root <expr>
;; element, two spaces of indentation per level, element attributes in
;; name order, and attribute values with `"', `<', `>', `&' and newline
;; escaped.  A derivation is written once; later appearances of the
;; same `.drv' path are <repeated />.

;;; Code:

(require 'cl-lib)
(require 'elnix-runtime)

(declare-function elnix-print-float "elnix-print" (f))

(defun elnix-xml--escape (s)
  (let ((out nil))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (push (pcase c
                (?\" "&quot;") (?< "&lt;") (?> "&gt;") (?& "&amp;")
                (?\n "&#xA;")
                (_ (unibyte-string c)))
              out)))
    (apply #'concat (nreverse out))))

(defun elnix-to-xml (v)
  "`builtins.toXML' of the value V, as a string value with its context."
  (let ((out nil) (depth 0) (ctxs nil)
        (drvs-seen (make-hash-table :test 'equal)))
    (cl-labels
        ((attrs-text (attrs)
           (mapconcat (lambda (a) (format " %s=\"%s\"" (car a) (elnix-xml--escape (cdr a))))
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
           (dolist (k (append (elnix-attr-keys a) nil))
             (open "attr" (list (cons "name" (symbol-name k))))
             (walk (gethash k (aref a 1)))
             (close "attr")))
         (walk (x)
           (let ((val (elnix-force x)))
             (cond
              ((stringp val) (empty "string" (list (cons "value" val))))
              ((elnix--cstr-p val)
               (push (aref val 2) ctxs)
               (empty "string" (list (cons "value" (aref val 1)))))
              ((integerp val) (empty "int" (list (cons "value" (number-to-string val)))))
              ((floatp val) (empty "float" (list (cons "value" (elnix-print-float val)))))
              ((eq val t) (empty "bool" (list (cons "value" "true"))))
              ((null val) (empty "bool" (list (cons "value" "false"))))
              ((eq val :null) (empty "null"))
              ((elnix--path-p val) (empty "path" (list (cons "value" (aref val 1)))))
              ((vectorp val)
               (open "list")
               (mapc #'walk val)
               (close "list"))
              ((elnix--attrs-p val)
               (if (elnix--is-derivation val)
                   (let* ((tbl (aref val 1))
                          (attrs nil)
                          (drv-path nil))
                     (let ((d (gethash 'drvPath tbl 'elnix--missing)))
                       (unless (eq d 'elnix--missing)
                         (let ((dv (elnix-force d)))
                           (when (elnix-string-p dv)
                             (setq drv-path (elnix--sval dv))
                             (push (cons "drvPath" drv-path) attrs)))))
                     (let ((o (gethash 'outPath tbl 'elnix--missing)))
                       (unless (eq o 'elnix--missing)
                         (let ((ov (elnix-force o)))
                           (when (elnix-string-p ov)
                             (push (cons "outPath" (elnix--sval ov)) attrs)))))
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
              ((elnix--lam-p val)
               (open "function")
               (let ((attrs (and (aref val 3) (list (cons "ellipsis" "1")))))
                 (open "attrspat" attrs)
                 (dolist (f (sort (mapcar (lambda (f) (symbol-name (car f))) (aref val 2))
                                  #'string<))
                   (empty "attr" (list (cons "name" f))))
                 (close "attrspat"))
               (close "function"))
              ((elnix-function-p val) (empty "unevaluated"))))))
      (push "<?xml version='1.0' encoding='utf-8'?>\n" out)
      (open "expr")
      (walk v)
      (close "expr"))
    (elnix--mkstr (apply #'concat (nreverse out)) (elnix--ctx-union ctxs))))

(provide 'elnix-xml)

;;; elnix-xml.el ends here
