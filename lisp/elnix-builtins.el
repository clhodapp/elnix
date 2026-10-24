;;; elnix-builtins.el --- The Nix builtins -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Every builtin of Nix 2.34.  A builtin of arity N is an Emacs Lisp
;; function `elnix-b--NAME' of N lazy arguments, which compiled code
;; calls directly when a call site supplies all N, and a curried value
;; registered under NAME for every other use.  A builtin of arity 0 is
;; a constant.
;;
;; The store, derivations, JSON, TOML, XML and regular expressions live
;; in their own libraries; the builtins here call into them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'elnix-runtime)
(require 'elnix-compile)

(declare-function elnix-import-path "elnix" (path))
(declare-function elnix-print-value "elnix-print" (v &optional depth))
(declare-function elnix-store-add-text "elnix-store" (name text refs))
(declare-function elnix-store-add-filtered "elnix-store"
                  (path name filter recursive expected))
(declare-function elnix-store-placeholder "elnix-store" (output))
(declare-function elnix-hash-string "elnix-store" (algo s))
(declare-function elnix-convert-hash "elnix-store" (hash algo to-format))
(declare-function elnix-derivation-strict "elnix-derivation" (attrs))
(declare-function elnix-to-json "elnix-json" (v))
(declare-function elnix-from-json "elnix-json" (s))
(declare-function elnix-from-toml "elnix-toml" (s))
(declare-function elnix-to-xml "elnix-xml" (v))
(declare-function elnix-regex-match "elnix-regex" (re s))
(declare-function elnix-regex-split "elnix-regex" (re s))

;;;; Configuration

(defvar elnix-current-system "x86_64-linux"
  "The value of `builtins.currentSystem'.")

(defvar elnix-nix-version "2.34.9"
  "The value of `builtins.nixVersion'.")

(defvar elnix-store-dir "/nix/store"
  "The store directory, `builtins.storeDir'.")

(defvar elnix-nix-path nil
  "The search path: a list of (PREFIX . PATH), searched in order.")

(defvar elnix-impure nil
  "Non-nil to read the environment as `nix-instantiate --impure' does.
Otherwise `getEnv' returns the empty string, as under pure evaluation.")

(defvar elnix-corepkgs-dir nil
  "The directory holding elnix's own versions of Nix's corepkgs.
`<nix/NAME>' resolves to a file there.")

(defvar elnix-derivation-function nil
  "A function of no arguments returning the value of `builtins.derivation'.
The derivation builtin is written in Nix, so the library that can
import Nix files sets this.")

;;;; Registry

(defvar elnix--builtins (make-hash-table :test 'eq)
  "The builtins by name symbol: their values.")

(defvar elnix--builtin-arities (make-hash-table :test 'equal)
  "The arities of the builtins by name string.")

(defconst elnix--global-names
  '("abort" "baseNameOf" "break" "builtins" "derivation" "derivationStrict"
    "dirOf" "false" "fetchGit" "fetchMercurial" "fetchTarball" "fetchTree"
    "fromTOML" "import" "isNull" "map" "null" "placeholder" "removeAttrs"
    "scopedImport" "throw" "toString" "true")
  "The builtins that are global variables without the `__' prefix.")

(defun elnix--register-builtin (name arity fn)
  "Register FN as the builtin NAME (a string) of ARITY."
  (puthash name arity elnix--builtin-arities)
  (puthash (intern name) (record 'elnix--primop fn arity nil name)
           elnix--builtins))

(defun elnix--register-constant (name value)
  (puthash name 0 elnix--builtin-arities)
  (puthash (intern name) value elnix--builtins))

(defmacro elnix-defbuiltin (name args &rest body)
  "Define the builtin NAME taking the lazy ARGS, with BODY."
  (declare (indent defun) (doc-string 3))
  (let ((fname (intern (format "elnix-b--%s" name))))
    `(progn
       (defun ,fname ,args ,@body)
       (elnix--register-builtin ,(symbol-name name) ,(length args) #',fname))))

(defun elnix-builtin (sym)
  "The value of the global builtin SYM."
  (let ((v (gethash sym elnix--builtins 'elnix--missing)))
    (if (eq v 'elnix--missing)
        (let ((name (symbol-name sym)))
          (if (string-prefix-p "__" name)
              (elnix-builtin (intern (substring name 2)))
            (elnix-fail "undefined variable '%s'" name)))
      (if (elnix--primop-p v)
          v
        ;; Constants computed from the configuration.
        (pcase sym
          ('currentSystem elnix-current-system)
          ('nixVersion elnix-nix-version)
          ('storeDir elnix-store-dir)
          ('nixPath (elnix--nix-path-value))
          ('builtins (elnix--builtins-attrs))
          ('derivation (funcall elnix-derivation-function))
          (_ v))))))

(defun elnix--global-p (name)
  (or (member name elnix--global-names)
      (member name '("__curPos" "__nixPath"))
      (and (string-prefix-p "__" name)
           (gethash (substring name 2) elnix--builtin-arities))))

(setq elnix-compile-global-p #'elnix--global-p)
(setq elnix-compile-builtin-arity
      (lambda (name)
        ;; Builtins defined in Nix rather than as functions here have no
        ;; direct form.
        (and (not (member name '("derivation")))
             (fboundp (intern (concat "elnix-b--" name)))
             (gethash name elnix--builtin-arities))))

(defvar elnix--builtins-attrs nil)

(defun elnix--builtins-attrs ()
  "The `builtins' set."
  (or elnix--builtins-attrs
      (let ((tbl (elnix-make-attrs (hash-table-count elnix--builtins))))
        (maphash (lambda (k _v)
                   (puthash k (if (eq k 'builtins)
                                  (elnix--delay (elnix--builtins-attrs))
                                (elnix--delay (elnix-builtin k)))
                            tbl))
                 elnix--builtins)
        (setq elnix--builtins-attrs (elnix--mkattrs tbl)))))

(defun elnix-reset-builtins-cache ()
  "Forget the cached `builtins' set, after the configuration changes."
  (setq elnix--builtins-attrs nil))

(elnix--register-constant "true" t)
(elnix--register-constant "false" nil)
(elnix--register-constant "null" :null)
(elnix--register-constant "langVersion" 6)
(elnix--register-constant "currentSystem" 'config)
(elnix--register-constant "nixVersion" 'config)
(elnix--register-constant "storeDir" 'config)
(elnix--register-constant "nixPath" 'config)
(elnix--register-constant "builtins" 'config)
(elnix--register-constant "derivation" 'config)

(defun elnix--nix-path-value ()
  (vconcat
   (mapcar (lambda (e)
             (elnix-attrs-from-alist
              (list (cons 'prefix (car e)) (cons 'path (cdr e)))))
           elnix-nix-path)))

;;;; Helpers

(defun elnix--list-elems (v)
  "The vector of the list V, forced."
  (elnix-force-list v))

(defun elnix--call2 (f a b)
  (elnix-app (elnix-app f a) b))

(defun elnix--coerce (v &optional copy more)
  "Force V and coerce it to a string value."
  (elnix-coerce-to-string (elnix-force v) copy more))

(defun elnix--mkbool (x) (if x t nil))

;;;; Types

(elnix-defbuiltin typeOf (x) (elnix-type-of (elnix-force x)))
(elnix-defbuiltin isNull (x) (eq (elnix-force x) :null))
(elnix-defbuiltin isInt (x) (integerp (elnix-force x)))
(elnix-defbuiltin isFloat (x) (floatp (elnix-force x)))
(elnix-defbuiltin isBool (x) (elnix-bool-p (elnix-force x)))
(elnix-defbuiltin isString (x) (elnix-string-p (elnix-force x)))
(elnix-defbuiltin isList (x) (vectorp (elnix-force x)))
(elnix-defbuiltin isAttrs (x) (elnix--attrs-p (elnix-force x)))
(elnix-defbuiltin isPath (x) (elnix--path-p (elnix-force x)))
(elnix-defbuiltin isFunction (x)
  (let ((v (elnix-force x)))
    (elnix--mkbool (elnix-function-p v))))

;;;; Arithmetic

(elnix-defbuiltin add (a b) (elnix-add-num (elnix-force a) (elnix-force b)))
(elnix-defbuiltin sub (a b) (elnix-sub (elnix-force a) (elnix-force b)))
(elnix-defbuiltin mul (a b) (elnix-mul (elnix-force a) (elnix-force b)))
(elnix-defbuiltin div (a b) (elnix-div (elnix-force a) (elnix-force b)))
(elnix-defbuiltin lessThan (a b)
  (elnix-less-than (elnix-force a) (elnix-force b)))
(elnix-defbuiltin bitAnd (a b) (logand (elnix-force-int a) (elnix-force-int b)))
(elnix-defbuiltin bitOr (a b) (logior (elnix-force-int a) (elnix-force-int b)))
(elnix-defbuiltin bitXor (a b) (logxor (elnix-force-int a) (elnix-force-int b)))

(defun elnix--round (x fn what)
  (let ((v (elnix-force x)))
    (cond
     ((integerp v) v)
     ((floatp v)
      (if (or (/= v v) (> (abs v) 1.0e19))
          (elnix-fail "%s: float out of range" what)
        (elnix--check-int (funcall fn v) what)))
     (t (elnix--type-error "a float" v)))))

(elnix-defbuiltin ceil (x) (elnix--round x #'ceiling "builtins.ceil"))
(elnix-defbuiltin floor (x) (elnix--round x #'floor "builtins.floor"))

;;;; Control

(elnix-defbuiltin throw (msg)
  (signal 'elnix-throw (list (elnix--sval (elnix--coerce msg)))))

(elnix-defbuiltin abort (msg)
  (signal 'elnix-abort
          (list (format "evaluation aborted with the following error message: '%s'"
                        (elnix--sval (elnix--coerce msg))))))

(elnix-defbuiltin seq (a b) (elnix-force a) (elnix-force b))

(defun elnix-deep-force (v)
  "Force the lazy value V and everything it contains."
  (let ((seen (make-hash-table :test 'eq)))
    (cl-labels ((walk (x)
                  (let ((val (elnix-force x)))
                    (unless (gethash val seen)
                      (cond
                       ((vectorp val)
                        (puthash val t seen)
                        (mapc #'walk val))
                       ((elnix--attrs-p val)
                        (puthash val t seen)
                        (maphash (lambda (_k y) (walk y)) (aref val 1)))))
                    val)))
      (walk v))))

(elnix-defbuiltin deepSeq (a b) (elnix-deep-force a) (elnix-force b))

(elnix-defbuiltin tryEval (e)
  (elnix--catching
   (((elnix-throw elnix-assertion) _err
     (elnix-attrs-from-alist (list (cons 'success nil) (cons 'value nil)))))
   (let ((v (elnix-force e)))
     (elnix-attrs-from-alist (list (cons 'success t) (cons 'value v))))))

(defvar elnix-trace-function
  (lambda (msg) (princ (concat "trace: " msg "\n") #'external-debugging-output))
  "The function that prints a `builtins.trace' message.")

(defun elnix--trace-text (v)
  (let ((x (elnix-force v)))
    (if (elnix-string-p x)
        (elnix--sval x)
      (elnix-print-value x))))

(elnix-defbuiltin trace (msg e)
  (funcall elnix-trace-function (elnix--trace-text msg))
  (elnix-force e))

(elnix-defbuiltin traceVerbose (_msg e) (elnix-force e))

(elnix-defbuiltin warn (msg e)
  (let ((m (elnix-force msg)))
    (unless (elnix-string-p m)
      (elnix--type-error "a string" m))
    (princ (concat "evaluation warning: " (elnix--sval m) "\n")
           #'external-debugging-output)
    (elnix-force e)))

(elnix-defbuiltin addErrorContext (_ctx e) (elnix-force e))
(elnix-defbuiltin break (e) (elnix-force e))

;;;; Lists

(elnix-defbuiltin length (l) (length (elnix--list-elems l)))

(elnix-defbuiltin head (l)
  (let ((v (elnix--list-elems l)))
    (when (= (length v) 0)
      (elnix-fail "'builtins.head' called on an empty list"))
    (elnix-force (aref v 0))))

(elnix-defbuiltin tail (l)
  (let ((v (elnix--list-elems l)))
    (when (= (length v) 0)
      (elnix-fail "'builtins.tail' called on an empty list"))
    (substring v 1)))

(elnix-defbuiltin elemAt (l n)
  (let ((v (elnix--list-elems l))
        (i (elnix-force-int n)))
    (when (or (< i 0) (>= i (length v)))
      (elnix-fail "'builtins.elemAt' called with index %d on a list of size %d"
                  i (length v)))
    (elnix-force (aref v i))))

(elnix-defbuiltin map (f l)
  (let ((v (elnix--list-elems l)))
    (if (= (length v) 0)
        []
      (let ((out (make-vector (length v) nil)))
        (dotimes (i (length v))
          (let ((x (aref v i)))
            (aset out i (elnix--delay (elnix-app f x)))))
        out))))

(elnix-defbuiltin filter (f l)
  (let ((v (elnix--list-elems l)) (out nil) (same t))
    (dotimes (i (length v))
      (let ((x (aref v i)))
        (if (elnix-force-bool (elnix-app f x))
            (push x out)
          (setq same nil))))
    (if same v (vconcat (nreverse out)))))

(elnix-defbuiltin foldl\' (op nul l)
  (let ((v (elnix--list-elems l)))
    (if (= (length v) 0)
        (elnix-force nul)
      (let ((acc nul))
        (dotimes (i (length v))
          (setq acc (elnix--call2 op acc (aref v i))))
        (elnix-force acc)))))

(elnix-defbuiltin concatLists (l)
  (let ((v (elnix--list-elems l)))
    (apply #'vconcat (mapcar #'elnix-force-list v))))

(elnix-defbuiltin concatMap (f l)
  (let ((v (elnix--list-elems l)))
    (apply #'vconcat (mapcar (lambda (x) (elnix-force-list (elnix-app f x))) v))))

(elnix-defbuiltin genList (f n)
  (let ((len (elnix-force-int n)))
    (when (< len 0)
      (elnix-fail "cannot create list of size %d" len))
    (let ((out (make-vector len nil)))
      (dotimes (i len)
        (let ((j i))
          (aset out i (elnix--delay (elnix-app f j)))))
      out)))

(elnix-defbuiltin elem (x l)
  (let ((v (elnix--list-elems l)))
    (elnix--mkbool (cl-some (lambda (y) (elnix-eq-lazy x y)) v))))

(elnix-defbuiltin all (f l)
  (elnix--mkbool
   (cl-every (lambda (x) (elnix-force-bool (elnix-app f x))) (elnix--list-elems l))))

(elnix-defbuiltin any (f l)
  (elnix--mkbool
   (cl-some (lambda (x) (elnix-force-bool (elnix-app f x))) (elnix--list-elems l))))

(elnix-defbuiltin partition (f l)
  (let ((right nil) (wrong nil))
    (mapc (lambda (x)
            (if (elnix-force-bool (elnix-app f x)) (push x right) (push x wrong)))
          (elnix--list-elems l))
    (elnix-attrs-from-alist
     (list (cons 'right (vconcat (nreverse right)))
           (cons 'wrong (vconcat (nreverse wrong)))))))

(elnix-defbuiltin groupBy (f l)
  (let ((groups (make-hash-table :test 'eq)) (order nil))
    (mapc (lambda (x)
            (let ((k (intern (elnix--unibyte
                              (elnix-force-string-no-ctx (elnix-app f x))))))
              (unless (gethash k groups) (push k order))
              (puthash k (cons x (gethash k groups)) groups)))
          (elnix--list-elems l))
    (let ((tbl (elnix-make-attrs (length order))))
      (dolist (k order)
        (puthash k (vconcat (nreverse (gethash k groups))) tbl))
      (elnix--mkattrs tbl))))

(elnix-defbuiltin sort (f l)
  (let ((v (elnix--list-elems l)))
    (if (< (length v) 2)
        v
      (sort (copy-sequence v)
            :lessp (lambda (a b) (elnix-force-bool (elnix--call2 f a b)))))))

;;;; Attribute sets

(elnix-defbuiltin attrNames (s)
  (let ((a (elnix-force-attrs s)))
    (vconcat (mapcar #'symbol-name (elnix-attr-keys a)))))

(elnix-defbuiltin attrValues (s)
  (let* ((a (elnix-force-attrs s))
         (tbl (aref a 1)))
    (vconcat (mapcar (lambda (k) (gethash k tbl)) (elnix-attr-keys a)))))

(elnix-defbuiltin getAttr (name s)
  (elnix-sel (elnix-force-attrs s)
             (intern (elnix--unibyte (elnix-force-string-no-ctx name)))))

(elnix-defbuiltin hasAttr (name s)
  (let ((sym (intern (elnix--unibyte (elnix-force-string-no-ctx name))))
        (a (elnix-force-attrs s)))
    (not (eq (gethash sym (aref a 1) 'elnix--missing) 'elnix--missing))))

(elnix-defbuiltin removeAttrs (s names)
  (let* ((a (elnix-force-attrs s))
         (ns (elnix--list-elems names))
         (syms (mapcar (lambda (n) (intern (elnix--unibyte
                                            (elnix-force-string-no-ctx n))))
                       ns))
         (tbl (aref a 1)))
    (if (cl-notany (lambda (k) (not (eq (gethash k tbl 'elnix--missing)
                                        'elnix--missing)))
                   syms)
        a
      (let ((new (copy-hash-table tbl)))
        (dolist (k syms) (remhash k new))
        (elnix--mkattrs new (aref a 2))))))

(elnix-defbuiltin listToAttrs (l)
  (let* ((v (elnix--list-elems l))
         (tbl (elnix-make-attrs (length v))))
    (mapc (lambda (e)
            (let* ((es (elnix-force-attrs e))
                   (n (gethash 'name (aref es 1) 'elnix--missing)))
              (when (eq n 'elnix--missing)
                (elnix-fail "attribute 'name' missing in a call to 'listToAttrs'"))
              (let ((sym (intern (elnix--unibyte (elnix-force-string-no-ctx n)))))
                (when (eq (gethash sym tbl 'elnix--missing) 'elnix--missing)
                  (let ((val (gethash 'value (aref es 1) 'elnix--missing)))
                    (when (eq val 'elnix--missing)
                      (elnix-fail "attribute 'value' missing in a call to 'listToAttrs'"))
                    (puthash sym val tbl))))))
          v)
    (elnix--mkattrs tbl)))

(elnix-defbuiltin mapAttrs (f s)
  (let* ((a (elnix-force-attrs s))
         (tbl (make-hash-table :test 'eq :size (hash-table-count (aref a 1)))))
    (maphash (lambda (k x)
               (let ((name (symbol-name k)))
                 (puthash k (elnix--delay (elnix--call2 f name x)) tbl)))
             (aref a 1))
    (elnix--mkattrs tbl (aref a 2))))

(elnix-defbuiltin intersectAttrs (e1 e2)
  (let* ((a (elnix-force-attrs e1))
         (b (elnix-force-attrs e2))
         (ta (aref a 1)) (tb (aref b 1))
         (tbl (elnix-make-attrs)))
    (if (< (hash-table-count ta) (hash-table-count tb))
        (maphash (lambda (k _v)
                   (let ((y (gethash k tb 'elnix--missing)))
                     (unless (eq y 'elnix--missing) (puthash k y tbl))))
                 ta)
      (maphash (lambda (k y)
                 (unless (eq (gethash k ta 'elnix--missing) 'elnix--missing)
                   (puthash k y tbl)))
               tb))
    (elnix--mkattrs tbl (aref b 2))))

(elnix-defbuiltin catAttrs (name l)
  (let ((sym (intern (elnix--unibyte (elnix-force-string-no-ctx name))))
        (out nil))
    (mapc (lambda (e)
            (let* ((es (elnix-force-attrs e))
                   (x (gethash sym (aref es 1) 'elnix--missing)))
              (unless (eq x 'elnix--missing) (push x out))))
          (elnix--list-elems l))
    (vconcat (nreverse out))))

(elnix-defbuiltin zipAttrsWith (f l)
  (let ((groups (make-hash-table :test 'eq)))
    (mapc (lambda (e)
            (let ((es (elnix-force-attrs e)))
              (maphash (lambda (k x) (puthash k (cons x (gethash k groups)) groups))
                       (aref es 1))))
          (elnix--list-elems l))
    (let ((tbl (elnix-make-attrs (hash-table-count groups))))
      (maphash (lambda (k xs)
                 (let ((name (symbol-name k))
                       (vals (vconcat (nreverse xs))))
                   (puthash k (elnix--delay (elnix--call2 f name vals)) tbl)))
               groups)
      (elnix--mkattrs tbl))))

(elnix-defbuiltin functionArgs (f)
  (let ((v (elnix-force f)))
    (cond
     ((elnix--lam-p v)
      (elnix-attrs-from-alist
       (mapcar (lambda (fm) (cons (car fm) (elnix--mkbool (cdr fm)))) (aref v 2))))
     ((elnix-function-p v) elnix--empty-attrs)
     (t (elnix--type-error "a function" v)))))

(elnix-defbuiltin unsafeGetAttrPos (name s)
  (let* ((sym (intern (elnix--unibyte (elnix-force-string-no-ctx name))))
         (a (elnix-force-attrs s))
         (pos (elnix--attrs-pos-lookup a sym)))
    (if pos
        (elnix-attrs-from-alist
         (list (cons 'file (car pos)) (cons 'line (cadr pos))
               (cons 'column (cddr pos))))
      :null)))

(elnix-defbuiltin genericClosure (args)
  (let* ((a (elnix-force-attrs args))
         (start (gethash 'startSet (aref a 1) 'elnix--missing))
         (op (gethash 'operator (aref a 1) 'elnix--missing)))
    (when (eq start 'elnix--missing)
      (elnix-fail "attribute 'startSet' required"))
    (when (eq op 'elnix--missing)
      (elnix-fail "attribute 'operator' required"))
    (let ((work (append (elnix--list-elems start) nil))
          (done nil)
          (keys nil))
      (while work
        (let* ((e (pop work))
               (es (elnix-force-attrs e))
               (k (gethash 'key (aref es 1) 'elnix--missing)))
          (when (eq k 'elnix--missing)
            (elnix-fail "attribute 'key' required"))
          (let ((kv (elnix-force k)))
            (unless (cl-some (lambda (seen)
                               (and (not (elnix-less-than kv seen))
                                    (not (elnix-less-than seen kv))))
                             keys)
              (push kv keys)
              (push e done)
              (setq work (append work
                                 (append (elnix-force-list (elnix-app op e))
                                         nil)))))))
      (vconcat (nreverse done)))))

;;;; Strings

(elnix-defbuiltin toString (x)
  (elnix-coerce-to-string (elnix-force x) nil t))

(elnix-defbuiltin stringLength (s)
  (length (elnix--sval (elnix--coerce s t))))

(elnix-defbuiltin substring (start len s)
  (let ((st (elnix-force-int start))
        (ln (elnix-force-int len)))
    (when (< st 0)
      (elnix-fail "negative start position in 'substring'"))
    (let* ((str (elnix--coerce s t))
           (text (elnix--sval str))
           (n (length text)))
      (if (>= st n)
          (elnix--mkstr "" (elnix--sctx str))
        (elnix--mkstr (substring text st (if (or (< ln 0) (> (+ st ln) n))
                                             n
                                           (+ st ln)))
                      (elnix--sctx str))))))

(elnix-defbuiltin concatStringsSep (sep l)
  (let* ((sv (elnix-force-string sep))
         (ctxs (list (elnix--sctx sv)))
         (parts (mapcar (lambda (x)
                          (let ((s (elnix--coerce x t)))
                            (push (elnix--sctx s) ctxs)
                            (elnix--sval s)))
                        (elnix--list-elems l))))
    (elnix--mkstr (mapconcat #'identity parts (elnix--sval sv))
                  (elnix--ctx-union ctxs))))

(elnix-defbuiltin replaceStrings (from to s)
  (let* ((fv (elnix--list-elems from))
         (tv (elnix--list-elems to)))
    (unless (= (length fv) (length tv))
      (elnix-fail "'from' and 'to' arguments passed to builtins.replaceStrings have different lengths"))
    ;; A replacement is forced only when its pattern matches.
    (let* ((froms (mapcar #'elnix-force-string-text fv))
           (tos (mapcar (lambda (x) (elnix--delay (elnix-force-string x))) tv))
           (sv (elnix-force-string s))
           (text (elnix--sval sv))
           (n (length text))
           (ctxs (list (elnix--sctx sv)))
           (out nil)
           (p 0))
      (while (<= p n)
        (let ((fs froms) (ts tos) (found nil))
          (while (and fs (not found))
            (let ((f (car fs)))
              (when (and (<= (+ p (length f)) n)
                         (eq t (compare-strings f nil nil text p (+ p (length f)))))
                (setq found t)
                (let ((to (elnix-force (car ts))))
                  (push (elnix--sval to) out)
                  (push (elnix--sctx to) ctxs))
                (if (= (length f) 0)
                    (progn
                      (when (< p n) (push (substring text p (1+ p)) out))
                      (setq p (1+ p)))
                  (setq p (+ p (length f))))))
            (setq fs (cdr fs) ts (cdr ts)))
          (unless found
            (when (< p n) (push (substring text p (1+ p)) out))
            (setq p (1+ p)))))
      (elnix--mkstr (apply #'concat (nreverse out)) (elnix--ctx-union ctxs)))))

(defun elnix--base-name-of (path)
  (if (= (length path) 0)
      ""
    (let* ((last (1- (length path))))
      (when (and (eq (aref path last) ?/) (> last 0))
        (setq last (1- last)))
      (let ((pos (cl-position ?/ path :end (1+ last) :from-end t)))
        (substring path (if pos (1+ pos) 0) (1+ last))))))

(elnix-defbuiltin baseNameOf (x)
  (let ((s (elnix--coerce x nil)))
    (elnix--mkstr (elnix--base-name-of (elnix--sval s)) (elnix--sctx s))))

(defun elnix--dir-of (path)
  (let ((pos (cl-position ?/ path :from-end t)))
    (cond ((null pos) ".")
          ((= pos 0) "/")
          (t (substring path 0 pos)))))

(elnix-defbuiltin dirOf (x)
  (let ((v (elnix-force x)))
    (if (elnix--path-p v)
        (let ((p (aref v 1)))
          (if (string= p "/") v (elnix--mkpath (elnix--dir-of p))))
      (let ((s (elnix-coerce-to-string v nil)))
        (elnix--mkstr (elnix--dir-of (elnix--sval s)) (elnix--sctx s))))))

(elnix-defbuiltin hashString (algo s)
  (elnix-hash-string (elnix-force-string-no-ctx algo)
                     (elnix-force-string-text s)))

(elnix-defbuiltin parseDrvName (s)
  (let* ((text (elnix-force-string-text s))
         (name text) (version "")
         (n (length text)))
    (catch 'done
      (dotimes (i n)
        (when (and (eq (aref text i) ?-)
                   (< (1+ i) n)
                   (not (let ((c (aref text (1+ i))))
                          (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))))))
          (setq name (substring text 0 i)
                version (substring text (1+ i)))
          (throw 'done nil))))
    (elnix-attrs-from-alist (list (cons 'name name) (cons 'version version)))))

(defsubst elnix--digit-p (c) (and (>= c ?0) (<= c ?9)))

(defun elnix--version-next (s p)
  "The next version component of S at P: (COMPONENT . NEXT-P)."
  (let ((n (length s)))
    (while (and (< p n) (memq (aref s p) '(?. ?-))) (setq p (1+ p)))
    (if (>= p n)
        (cons "" p)
      (let ((start p))
        (if (elnix--digit-p (aref s p))
            (while (and (< p n) (elnix--digit-p (aref s p))) (setq p (1+ p)))
          (while (and (< p n)
                      (not (elnix--digit-p (aref s p)))
                      (not (memq (aref s p) '(?. ?-))))
            (setq p (1+ p))))
        (cons (substring s start p) p)))))

(defun elnix--version-int (c)
  "C as Nix's string2Int<int> reads it, or nil."
  (and (> (length c) 0)
       (cl-every #'elnix--digit-p c)
       (let ((n (string-to-number c)))
         (and (<= n 2147483647) n))))

(defun elnix--components-lt (c1 c2)
  (let ((n1 (elnix--version-int c1))
        (n2 (elnix--version-int c2)))
    (cond
     ((and n1 n2) (< n1 n2))
     ((and (string= c1 "") n2) t)
     ((and (string= c1 "pre") (not (string= c2 "pre"))) t)
     ((string= c2 "pre") nil)
     (n2 t)
     (n1 nil)
     (t (string< c1 c2)))))

(defun elnix-compare-versions (v1 v2)
  "Compare the version strings V1 and V2 as Nix does: -1, 0 or 1."
  (let ((p1 0) (p2 0) (result 0))
    (while (and (= result 0)
                (or (< p1 (length v1)) (< p2 (length v2))))
      (let ((c1 (elnix--version-next v1 p1))
            (c2 (elnix--version-next v2 p2)))
        (setq p1 (cdr c1) p2 (cdr c2))
        (cond ((elnix--components-lt (car c1) (car c2)) (setq result -1))
              ((elnix--components-lt (car c2) (car c1)) (setq result 1)))))
    result))

(elnix-defbuiltin compareVersions (a b)
  (elnix-compare-versions (elnix-force-string-text a)
                          (elnix-force-string-text b)))

(elnix-defbuiltin splitVersion (v)
  (let ((s (elnix-force-string-text v)) (p 0) (out nil) (done nil))
    (while (and (not done) (< p (length s)))
      (let ((c (elnix--version-next s p)))
        (setq p (cdr c))
        (if (string= (car c) "")
            (setq done t)
          (push (car c) out))))
    (vconcat (nreverse out))))

(elnix-defbuiltin match (re s)
  (elnix-regex-match (elnix-force-string-text re) (elnix-force-string-text s)))

(elnix-defbuiltin split (re s)
  (elnix-regex-split (elnix-force-string-text re) (elnix-force-string-text s)))

;;;; String contexts

(elnix-defbuiltin unsafeDiscardStringContext (s)
  (elnix--sval (elnix--coerce s t)))

(elnix-defbuiltin hasContext (s)
  (elnix--mkbool (elnix--sctx (elnix-force-string s))))

(elnix-defbuiltin unsafeDiscardOutputDependency (s)
  (let ((sv (elnix--coerce s t)))
    (elnix--mkstr (elnix--sval sv)
                  (delete-dups
                   (mapcar (lambda (c) (if (eq (aref c 0) ?=) (substring c 1) c))
                           (elnix--sctx sv))))))

(elnix-defbuiltin addDrvOutputDependencies (s)
  (let* ((sv (elnix--coerce s t))
         (ctx (elnix--sctx sv)))
    (unless (= (length ctx) 1)
      (elnix-fail "context of string '%s' must have exactly one element, but has %d"
                  (elnix--sval sv) (length ctx)))
    (let ((c (car ctx)))
      (cond
       ((eq (aref c 0) ?=) sv)
       ((eq (aref c 0) ?!)
        (elnix-fail "`addDrvOutputDependencies` can only act on derivations, not on a derivation output such as '%s'"
                    c))
       ((string-suffix-p ".drv" c) (elnix--mkstr (elnix--sval sv) (list (concat "=" c))))
       (t (elnix-fail "path '%s' is not a derivation" c))))))

(defun elnix--context-info (ctx)
  "Group the context CTX by path: an alist of (PATH PATH-P ALL-OUTPUTS OUTPUTS)."
  (let ((info nil))
    (dolist (c ctx)
      (let (path kind out)
        (cond
         ((eq (aref c 0) ?=) (setq path (substring c 1) kind 'all))
         ((eq (aref c 0) ?!)
          (let ((i (cl-position ?! c :start 1)))
            (setq out (substring c 1 i) path (substring c (1+ i)) kind 'out)))
         (t (setq path c kind 'path)))
        (let ((e (or (assoc path info)
                     (car (push (list path nil nil nil) info)))))
          (pcase kind
            ('path (setf (nth 1 e) t))
            ('all (setf (nth 2 e) t))
            ('out (setf (nth 3 e) (cons out (nth 3 e))))))))
    info))

(elnix-defbuiltin getContext (s)
  (let* ((sv (elnix-force-string s))
         (info (elnix--context-info (elnix--sctx sv)))
         (tbl (elnix-make-attrs (length info))))
    (dolist (e info)
      (let ((alist nil))
        (when (nth 3 e)
          (push (cons 'outputs (vconcat (sort (delete-dups (copy-sequence (nth 3 e)))
                                              #'string<)))
                alist))
        (when (nth 2 e) (push (cons 'allOutputs t) alist))
        (when (nth 1 e) (push (cons 'path t) alist))
        (puthash (intern (car e)) (elnix-attrs-from-alist alist) tbl)))
    (elnix--mkattrs tbl)))

(elnix-defbuiltin appendContext (s ctx)
  (let* ((sv (elnix-force-string s))
         (c (elnix-force-attrs ctx))
         (new (copy-sequence (elnix--sctx sv))))
    (maphash
     (lambda (k v)
       (let* ((path (symbol-name k))
              (info (elnix-force-attrs v))
              (tbl (aref info 1)))
         (unless (string-prefix-p (concat elnix-store-dir "/") path)
           (elnix-fail "context key '%s' is not a store path" path))
         (let ((p (gethash 'path tbl 'elnix--missing)))
           (when (and (not (eq p 'elnix--missing)) (elnix-force-bool p))
             (push path new)))
         (let ((a (gethash 'allOutputs tbl 'elnix--missing)))
           (when (and (not (eq a 'elnix--missing)) (elnix-force-bool a))
             (unless (string-suffix-p ".drv" path)
               (elnix-fail "tried to add all-outputs context of %s, which is not a derivation, to a string"
                           path))
             (push (concat "=" path) new)))
         (let ((o (gethash 'outputs tbl 'elnix--missing)))
           (unless (eq o 'elnix--missing)
             (let ((outs (elnix-force-list o)))
               (when (and (> (length outs) 0) (not (string-suffix-p ".drv" path)))
                 (elnix-fail "tried to add derivation output context of %s, which is not a derivation, to a string"
                             path))
               (mapc (lambda (out)
                       (push (concat "!" (elnix-force-string-text out) "!" path) new))
                     outs))))))
     (aref c 1))
    (elnix--mkstr (elnix--sval sv) (delete-dups new))))

;;;; Hashes and the store

(elnix-defbuiltin placeholder (out)
  (elnix-store-placeholder (elnix-force-string-text out)))

(elnix-defbuiltin toFile (name s)
  (let* ((n (elnix-force-string-no-ctx name))
         (sv (elnix-force-string s))
         (refs nil))
    (dolist (c (elnix--sctx sv))
      (when (memq (aref c 0) '(?= ?!))
        (elnix-fail "files created by builtins.toFile may not reference derivations, but %s references %s"
                    n c))
      (push c refs))
    (let ((p (elnix-store-add-text n (elnix--sval sv) refs)))
      (elnix--mkstr p (list p)))))

(elnix-defbuiltin convertHash (args)
  (let* ((a (elnix-force-attrs args))
         (tbl (aref a 1))
         (hash (elnix-force-string-text (gethash 'hash tbl)))
         (algo (let ((x (gethash 'hashAlgo tbl 'elnix--missing)))
                 (and (not (eq x 'elnix--missing)) (elnix-force-string-text x))))
         (to (elnix-force-string-text
              (let ((x (gethash 'toHashFormat tbl 'elnix--missing)))
                (when (eq x 'elnix--missing)
                  (elnix-fail "attribute 'toHashFormat' missing"))
                x))))
    (elnix-convert-hash hash algo to)))

;;;; Files

(defun elnix--real-path (v &optional what)
  "The absolute path text of the path or string value V."
  (let ((x (elnix-force v)))
    (cond
     ((elnix--path-p x) (aref x 1))
     ((elnix-string-p x)
      (let ((s (elnix--sval x)))
        (unless (string-prefix-p "/" s)
          (elnix-fail "string '%s' doesn't represent an absolute path%s" s
                      (if what (concat " " what) "")))
        (elnix-canon-path s)))
     ((elnix--attrs-p x)
      (elnix--sval (elnix-coerce-to-string x nil)))
     (t (elnix--type-error "a path" x)))))

(elnix-defbuiltin import (p)
  (elnix-import-path (elnix--real-path p)))

(elnix-defbuiltin scopedImport (_scope _p)
  (elnix-fail "builtins.scopedImport is not supported by elnix"))

(elnix-defbuiltin pathExists (p)
  ;; A string ending in / or /. must name a directory, through any
  ;; symlink; otherwise the path itself is looked at, so a dangling
  ;; symlink exists.
  (let* ((x (elnix-force p))
         (must-dir (and (elnix-string-p x)
                        (let ((s (elnix--sval x)))
                          (or (string-suffix-p "/" s) (string-suffix-p "/." s)))))
         (path (elnix--real-path x)))
    (elnix--mkbool
     (if must-dir
         (file-directory-p path)
       (file-attributes path)))))

(defun elnix-read-file-bytes (path)
  "The contents of the file PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (condition-case err
        (insert-file-contents-literally path)
      (file-missing (elnix-fail "opening file '%s': No such file or directory"
                                path))
      (file-error (elnix-fail "reading file '%s': %s" path (error-message-string err))))
    (buffer-string)))

(elnix-defbuiltin readFile (p)
  (let ((path (elnix--real-path p)))
    (when (file-directory-p path)
      (elnix-fail "error: reading from file '%s': Is a directory" path))
    (elnix-read-file-bytes path)))

(defun elnix--file-type (path)
  "The Nix file type name of PATH, not following a final symlink."
  (let ((attrs (file-attributes path)))
    (unless attrs
      (elnix-fail "getting status of '%s': No such file or directory" path))
    (let ((ty (file-attribute-type attrs)))
      (cond ((eq ty t) "directory")
            ((stringp ty) "symlink")
            ((null ty)
             (if (string-prefix-p "-" (file-attribute-modes attrs))
                 "regular"
               "unknown"))
            (t "unknown")))))

(elnix-defbuiltin readFileType (p)
  (elnix--file-type (elnix--real-path p)))

(elnix-defbuiltin readDir (p)
  (let* ((path (elnix--real-path p))
         (names (condition-case nil
                    (directory-files path nil nil t)
                  (file-error (elnix-fail "reading directory '%s'" path))))
         (tbl (elnix-make-attrs (length names))))
    (dolist (n names)
      (unless (member n '("." ".."))
        (let ((un (encode-coding-string n 'utf-8-unix t)))
          (puthash (intern un)
                   (elnix--file-type (concat path "/" n))
                   tbl))))
    (elnix--mkattrs tbl)))

(elnix-defbuiltin hashFile (algo p)
  (elnix-hash-string (elnix-force-string-no-ctx algo)
                     (elnix-read-file-bytes (elnix--real-path p))))

(elnix-defbuiltin filterSource (f p)
  (let ((path (elnix--real-path p)))
    (let ((sp (elnix-store-add-filtered path (elnix--base-name-of path) f t nil)))
      (elnix--mkstr sp (list sp)))))

(elnix-defbuiltin path (args)
  (let* ((a (elnix-force-attrs args))
         (tbl (aref a 1))
         (get (lambda (k) (let ((x (gethash k tbl 'elnix--missing)))
                            (if (eq x 'elnix--missing) nil x))))
         (pv (funcall get 'path)))
    (unless pv (elnix-fail "missing required 'path' attribute in the first argument to builtins.path"))
    (let* ((path (elnix--real-path pv))
           (name (let ((n (funcall get 'name)))
                   (if n (elnix-force-string-no-ctx n) (elnix--base-name-of path))))
           (filter (funcall get 'filter))
           ;; `false' is nil, so the lookup has to tell it from a
           ;; missing attribute.
           (recursive (let ((r (gethash 'recursive tbl 'elnix--missing)))
                        (if (eq r 'elnix--missing) t (elnix-force-bool r))))
           (sha (let ((s (funcall get 'sha256)))
                  (and s (elnix-force-string-no-ctx s))))
           (sp (elnix-store-add-filtered path name filter recursive sha)))
      (elnix--mkstr sp (list sp)))))

(elnix-defbuiltin findFile (search name)
  (let ((entries (elnix--list-elems search))
        (n (elnix-force-string-text name)))
    (elnix--find-in entries n)))

(defun elnix--find-in (entries name)
  (catch 'found
    (mapc
     (lambda (e)
       (let* ((es (elnix-force-attrs e))
              (prefix (let ((x (gethash 'prefix (aref es 1) 'elnix--missing)))
                        (if (eq x 'elnix--missing) "" (elnix-force-string-text x))))
              (path (elnix--sval (elnix--coerce (gethash 'path (aref es 1)) nil))))
         (when (or (string= prefix "")
                   (string= name prefix)
                   (string-prefix-p (concat prefix "/") name))
           (let* ((rest (if (string= prefix "") name
                          (substring name (min (length name) (1+ (length prefix))))))
                  (full (elnix-canon-path
                         (if (string= rest "") path (concat path "/" rest)))))
             (when (file-exists-p full)
               (throw 'found (elnix--mkpath full)))))))
     entries)
    (elnix--find-corepkg name)))

(defun elnix--find-corepkg (name)
  (if (and elnix-corepkgs-dir (string-prefix-p "nix/" name)
           (file-exists-p (expand-file-name (substring name 4) elnix-corepkgs-dir)))
      (elnix--mkpath (expand-file-name (substring name 4) elnix-corepkgs-dir))
    ;; Nix throws this one, so tryEval catches it.
    (signal 'elnix-throw
            (list (format "file '%s' was not found in the Nix search path (add it using $NIX_PATH or -I)"
                          name)))))

(defun elnix--find-file (name)
  "The path `<NAME>' denotes."
  (elnix--find-in (elnix--nix-path-value) name))

(elnix-defbuiltin getEnv (name)
  (let ((n (elnix-force-string-no-ctx name)))
    (or (and elnix-impure (getenv n)) "")))

(elnix-defbuiltin toPath (p)
  (let ((x (elnix-force p)))
    (let ((s (elnix-coerce-to-string x nil)))
      (unless (string-prefix-p "/" (elnix--sval s))
        (elnix-fail "string '%s' doesn't represent an absolute path" (elnix--sval s)))
      (elnix--mkstr (elnix-canon-path (elnix--sval s)) (elnix--sctx s)))))

(elnix-defbuiltin storePath (p)
  (let* ((path (elnix--real-path p)))
    (unless (string-prefix-p (concat elnix-store-dir "/") path)
      (elnix-fail "path '%s' is not in the Nix store" path))
    (let* ((rest (substring path (1+ (length elnix-store-dir))))
           (root (concat elnix-store-dir "/" (car (split-string rest "/")))))
      (elnix--mkstr path (list root)))))

(elnix-defbuiltin currentTime ()
  (elnix-fail "builtins.currentTime is not available in pure evaluation"))

;;;; Serialisation

(elnix-defbuiltin toJSON (x)
  (elnix-to-json (elnix-force x)))

(elnix-defbuiltin fromJSON (s)
  (elnix-from-json (elnix-force-string-text s)))

(elnix-defbuiltin fromTOML (s)
  (elnix-from-toml (elnix-force-string-text s)))

(elnix-defbuiltin toXML (x)
  (elnix-to-xml (elnix-force x)))

;;;; Derivations

(elnix-defbuiltin derivationStrict (attrs)
  (elnix-derivation-strict (elnix-force-attrs attrs)))

;;;; Fetchers and flakes

(dolist (name '("fetchurl" "fetchTarball" "fetchGit" "fetchMercurial"
                "fetchTree" "getFlake" "fetchClosure"))
  (let ((n name))
    (elnix--register-builtin
     n 1 (lambda (_x) (elnix-fail "builtins.%s is not supported by elnix: evaluation does not fetch" n)))))

(dolist (name '("parseFlakeRef" "flakeRefToString"))
  (let ((n name))
    (elnix--register-builtin
     n 1 (lambda (_x) (elnix-fail "builtins.%s is not supported by elnix" n)))))

(provide 'elnix-builtins)

;;; elnix-builtins.el ends here
