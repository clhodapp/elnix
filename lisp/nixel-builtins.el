;;; nixel-builtins.el --- The Nix builtins -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Every builtin of Nix 2.34.  A builtin of arity N is an Emacs Lisp
;; function `nixel-b--NAME' of N lazy arguments, which compiled code
;; calls directly when a call site supplies all N, and a curried value
;; registered under NAME for every other use.  A builtin of arity 0 is
;; a constant.
;;
;; The store, derivations, JSON, TOML, XML and regular expressions live
;; in their own libraries; the builtins here call into them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nixel-runtime)
(require 'nixel-compile)

(declare-function nixel-import-path "nixel" (path))
(declare-function nixel-print-value "nixel-print" (v &optional depth))
(declare-function nixel-store-add-text "nixel-store" (name text refs))
(declare-function nixel-store-add-filtered "nixel-store"
                  (path name filter recursive expected))
(declare-function nixel-store-placeholder "nixel-store" (output))
(declare-function nixel-hash-string "nixel-store" (algo s))
(declare-function nixel-convert-hash "nixel-store" (hash algo to-format))
(declare-function nixel-derivation-strict "nixel-derivation" (attrs))
(declare-function nixel-to-json "nixel-json" (v))
(declare-function nixel-from-json "nixel-json" (s))
(declare-function nixel-from-toml "nixel-toml" (s))
(declare-function nixel-to-xml "nixel-xml" (v))
(declare-function nixel-regex-match "nixel-regex" (re s))
(declare-function nixel-regex-split "nixel-regex" (re s))

;;;; Configuration

(defvar nixel-current-system "x86_64-linux"
  "The value of `builtins.currentSystem'.")

(defvar nixel-nix-version "2.34.9"
  "The value of `builtins.nixVersion'.")

(defvar nixel-store-dir "/nix/store"
  "The store directory, `builtins.storeDir'.")

(defvar nixel-nix-path nil
  "The search path: a list of (PREFIX . PATH), searched in order.")

(defvar nixel-impure nil
  "Non-nil to read the environment as `nix-instantiate --impure' does.
Otherwise `getEnv' returns the empty string, as under pure evaluation.")

(defvar nixel-corepkgs-dir nil
  "The directory holding nixel's own versions of Nix's corepkgs.
`<nix/NAME>' resolves to a file there.")

(defvar nixel-derivation-function nil
  "A function of no arguments returning the value of `builtins.derivation'.
The derivation builtin is written in Nix, so the library that can
import Nix files sets this.")

;;;; Registry

(defvar nixel--builtins (make-hash-table :test 'eq)
  "The builtins by name symbol: their values.")

(defvar nixel--builtin-arities (make-hash-table :test 'equal)
  "The arities of the builtins by name string.")

(defconst nixel--global-names
  '("abort" "baseNameOf" "break" "builtins" "derivation" "derivationStrict"
    "dirOf" "false" "fetchGit" "fetchMercurial" "fetchTarball" "fetchTree"
    "fromTOML" "import" "isNull" "map" "null" "placeholder" "removeAttrs"
    "scopedImport" "throw" "toString" "true")
  "The builtins that are global variables without the `__' prefix.")

(defun nixel--register-builtin (name arity fn)
  "Register FN as the builtin NAME (a string) of ARITY."
  (puthash name arity nixel--builtin-arities)
  (puthash (intern name) (record 'nixel--primop fn arity nil name)
           nixel--builtins))

(defun nixel--register-constant (name value)
  (puthash name 0 nixel--builtin-arities)
  (puthash (intern name) value nixel--builtins))

(defmacro nixel-defbuiltin (name args &rest body)
  "Define the builtin NAME taking the lazy ARGS, with BODY."
  (declare (indent defun) (doc-string 3))
  (let ((fname (intern (format "nixel-b--%s" name))))
    `(progn
       (defun ,fname ,args ,@body)
       (nixel--register-builtin ,(symbol-name name) ,(length args) #',fname))))

(defun nixel-builtin (sym)
  "The value of the global builtin SYM."
  (let ((v (gethash sym nixel--builtins 'nixel--missing)))
    (if (eq v 'nixel--missing)
        (let ((name (symbol-name sym)))
          (if (string-prefix-p "__" name)
              (nixel-builtin (intern (substring name 2)))
            (nixel-fail "undefined variable '%s'" name)))
      (if (nixel--primop-p v)
          v
        ;; Constants computed from the configuration.
        (pcase sym
          ('currentSystem nixel-current-system)
          ('nixVersion nixel-nix-version)
          ('storeDir nixel-store-dir)
          ('nixPath (nixel--nix-path-value))
          ('builtins (nixel--builtins-attrs))
          ('derivation (funcall nixel-derivation-function))
          (_ v))))))

(defun nixel--global-p (name)
  (or (member name nixel--global-names)
      (member name '("__curPos" "__nixPath"))
      (and (string-prefix-p "__" name)
           (gethash (substring name 2) nixel--builtin-arities))))

(setq nixel-compile-global-p #'nixel--global-p)
(setq nixel-compile-builtin-arity
      (lambda (name)
        ;; Builtins defined in Nix rather than as functions here have no
        ;; direct form.
        (and (not (member name '("derivation")))
             (fboundp (intern (concat "nixel-b--" name)))
             (gethash name nixel--builtin-arities))))

(defvar nixel--builtins-attrs nil)

(defun nixel--builtins-attrs ()
  "The `builtins' set."
  (or nixel--builtins-attrs
      (let ((tbl (nixel-make-attrs (hash-table-count nixel--builtins))))
        (maphash (lambda (k _v)
                   (puthash k (if (eq k 'builtins)
                                  (nixel--delay (nixel--builtins-attrs))
                                (nixel--delay (nixel-builtin k)))
                            tbl))
                 nixel--builtins)
        (setq nixel--builtins-attrs (nixel--mkattrs tbl)))))

(defun nixel-reset-builtins-cache ()
  "Forget the cached `builtins' set, after the configuration changes."
  (setq nixel--builtins-attrs nil))

(nixel--register-constant "true" t)
(nixel--register-constant "false" nil)
(nixel--register-constant "null" :null)
(nixel--register-constant "langVersion" 6)
(nixel--register-constant "currentSystem" 'config)
(nixel--register-constant "nixVersion" 'config)
(nixel--register-constant "storeDir" 'config)
(nixel--register-constant "nixPath" 'config)
(nixel--register-constant "builtins" 'config)
(nixel--register-constant "derivation" 'config)

(defun nixel--nix-path-value ()
  (vconcat
   (mapcar (lambda (e)
             (nixel-attrs-from-alist
              (list (cons 'prefix (car e)) (cons 'path (cdr e)))))
           nixel-nix-path)))

;;;; Helpers

(defun nixel--list-elems (v)
  "The vector of the list V, forced."
  (nixel-force-list v))

(defun nixel--call2 (f a b)
  (nixel-app (nixel-app f a) b))

(defun nixel--coerce (v &optional copy more)
  "Force V and coerce it to a string value."
  (nixel-coerce-to-string (nixel-force v) copy more))

(defun nixel--mkbool (x) (if x t nil))

;;;; Types

(nixel-defbuiltin typeOf (x) (nixel-type-of (nixel-force x)))
(nixel-defbuiltin isNull (x) (eq (nixel-force x) :null))
(nixel-defbuiltin isInt (x) (integerp (nixel-force x)))
(nixel-defbuiltin isFloat (x) (floatp (nixel-force x)))
(nixel-defbuiltin isBool (x) (nixel-bool-p (nixel-force x)))
(nixel-defbuiltin isString (x) (nixel-string-p (nixel-force x)))
(nixel-defbuiltin isList (x) (vectorp (nixel-force x)))
(nixel-defbuiltin isAttrs (x) (nixel--attrs-p (nixel-force x)))
(nixel-defbuiltin isPath (x) (nixel--path-p (nixel-force x)))
(nixel-defbuiltin isFunction (x)
  (let ((v (nixel-force x)))
    (nixel--mkbool (nixel-function-p v))))

;;;; Arithmetic

(nixel-defbuiltin add (a b) (nixel-add-num (nixel-force a) (nixel-force b)))
(nixel-defbuiltin sub (a b) (nixel-sub (nixel-force a) (nixel-force b)))
(nixel-defbuiltin mul (a b) (nixel-mul (nixel-force a) (nixel-force b)))
(nixel-defbuiltin div (a b) (nixel-div (nixel-force a) (nixel-force b)))
(nixel-defbuiltin lessThan (a b)
  (nixel-less-than (nixel-force a) (nixel-force b)))
(nixel-defbuiltin bitAnd (a b) (logand (nixel-force-int a) (nixel-force-int b)))
(nixel-defbuiltin bitOr (a b) (logior (nixel-force-int a) (nixel-force-int b)))
(nixel-defbuiltin bitXor (a b) (logxor (nixel-force-int a) (nixel-force-int b)))

(defun nixel--round (x fn what)
  (let ((v (nixel-force x)))
    (cond
     ((integerp v) v)
     ((floatp v)
      (if (or (/= v v) (> (abs v) 1.0e19))
          (nixel-fail "%s: float out of range" what)
        (nixel--check-int (funcall fn v) what)))
     (t (nixel--type-error "a float" v)))))

(nixel-defbuiltin ceil (x) (nixel--round x #'ceiling "builtins.ceil"))
(nixel-defbuiltin floor (x) (nixel--round x #'floor "builtins.floor"))

;;;; Control

(nixel-defbuiltin throw (msg)
  (signal 'nixel-throw (list (nixel--sval (nixel--coerce msg)))))

(nixel-defbuiltin abort (msg)
  (signal 'nixel-abort
          (list (format "evaluation aborted with the following error message: '%s'"
                        (nixel--sval (nixel--coerce msg))))))

(nixel-defbuiltin seq (a b) (nixel-force a) (nixel-force b))

(defun nixel-deep-force (v)
  "Force the lazy value V and everything it contains."
  (let ((seen (make-hash-table :test 'eq)))
    (cl-labels ((walk (x)
                  (let ((val (nixel-force x)))
                    (unless (gethash val seen)
                      (cond
                       ((vectorp val)
                        (puthash val t seen)
                        (mapc #'walk val))
                       ((nixel--attrs-p val)
                        (puthash val t seen)
                        (maphash (lambda (_k y) (walk y)) (aref val 1)))))
                    val)))
      (walk v))))

(nixel-defbuiltin deepSeq (a b) (nixel-deep-force a) (nixel-force b))

(nixel-defbuiltin tryEval (e)
  (nixel--catching
   (((nixel-throw nixel-assertion) _err
     (nixel-attrs-from-alist (list (cons 'success nil) (cons 'value nil)))))
   (let ((v (nixel-force e)))
     (nixel-attrs-from-alist (list (cons 'success t) (cons 'value v))))))

(defvar nixel-trace-function
  (lambda (msg) (princ (concat "trace: " msg "\n") #'external-debugging-output))
  "The function that prints a `builtins.trace' message.")

(defun nixel--trace-text (v)
  (let ((x (nixel-force v)))
    (if (nixel-string-p x)
        (nixel--sval x)
      (nixel-print-value x))))

(nixel-defbuiltin trace (msg e)
  (funcall nixel-trace-function (nixel--trace-text msg))
  (nixel-force e))

(nixel-defbuiltin traceVerbose (_msg e) (nixel-force e))

(nixel-defbuiltin warn (msg e)
  (let ((m (nixel-force msg)))
    (unless (nixel-string-p m)
      (nixel--type-error "a string" m))
    (princ (concat "evaluation warning: " (nixel--sval m) "\n")
           #'external-debugging-output)
    (nixel-force e)))

(nixel-defbuiltin addErrorContext (_ctx e) (nixel-force e))
(nixel-defbuiltin break (e) (nixel-force e))

;;;; Lists

(nixel-defbuiltin length (l) (length (nixel--list-elems l)))

(nixel-defbuiltin head (l)
  (let ((v (nixel--list-elems l)))
    (when (= (length v) 0)
      (nixel-fail "'builtins.head' called on an empty list"))
    (nixel-force (aref v 0))))

(nixel-defbuiltin tail (l)
  (let ((v (nixel--list-elems l)))
    (when (= (length v) 0)
      (nixel-fail "'builtins.tail' called on an empty list"))
    (substring v 1)))

(nixel-defbuiltin elemAt (l n)
  (let ((v (nixel--list-elems l))
        (i (nixel-force-int n)))
    (when (or (< i 0) (>= i (length v)))
      (nixel-fail "'builtins.elemAt' called with index %d on a list of size %d"
                  i (length v)))
    (nixel-force (aref v i))))

(nixel-defbuiltin map (f l)
  (let ((v (nixel--list-elems l)))
    (if (= (length v) 0)
        []
      (let ((out (make-vector (length v) nil)))
        (dotimes (i (length v))
          (let ((x (aref v i)))
            (aset out i (nixel--delay (nixel-app f x)))))
        out))))

(nixel-defbuiltin filter (f l)
  (let ((v (nixel--list-elems l)) (out nil) (same t))
    (dotimes (i (length v))
      (let ((x (aref v i)))
        (if (nixel-force-bool (nixel-app f x))
            (push x out)
          (setq same nil))))
    (if same v (vconcat (nreverse out)))))

(nixel-defbuiltin foldl\' (op nul l)
  (let ((v (nixel--list-elems l)))
    (if (= (length v) 0)
        (nixel-force nul)
      (let ((acc nul))
        (dotimes (i (length v))
          (setq acc (nixel--call2 op acc (aref v i))))
        (nixel-force acc)))))

(nixel-defbuiltin concatLists (l)
  (let ((v (nixel--list-elems l)))
    (apply #'vconcat (mapcar #'nixel-force-list v))))

(nixel-defbuiltin concatMap (f l)
  (let ((v (nixel--list-elems l)))
    (apply #'vconcat (mapcar (lambda (x) (nixel-force-list (nixel-app f x))) v))))

(nixel-defbuiltin genList (f n)
  (let ((len (nixel-force-int n)))
    (when (< len 0)
      (nixel-fail "cannot create list of size %d" len))
    (let ((out (make-vector len nil)))
      (dotimes (i len)
        (let ((j i))
          (aset out i (nixel--delay (nixel-app f j)))))
      out)))

(nixel-defbuiltin elem (x l)
  (let ((v (nixel--list-elems l)))
    (nixel--mkbool (cl-some (lambda (y) (nixel-eq-lazy x y)) v))))

(nixel-defbuiltin all (f l)
  (nixel--mkbool
   (cl-every (lambda (x) (nixel-force-bool (nixel-app f x))) (nixel--list-elems l))))

(nixel-defbuiltin any (f l)
  (nixel--mkbool
   (cl-some (lambda (x) (nixel-force-bool (nixel-app f x))) (nixel--list-elems l))))

(nixel-defbuiltin partition (f l)
  (let ((right nil) (wrong nil))
    (mapc (lambda (x)
            (if (nixel-force-bool (nixel-app f x)) (push x right) (push x wrong)))
          (nixel--list-elems l))
    (nixel-attrs-from-alist
     (list (cons 'right (vconcat (nreverse right)))
           (cons 'wrong (vconcat (nreverse wrong)))))))

(nixel-defbuiltin groupBy (f l)
  (let ((groups (make-hash-table :test 'eq)) (order nil))
    (mapc (lambda (x)
            (let ((k (intern (nixel--unibyte
                              (nixel-force-string-no-ctx (nixel-app f x))))))
              (unless (gethash k groups) (push k order))
              (puthash k (cons x (gethash k groups)) groups)))
          (nixel--list-elems l))
    (let ((tbl (nixel-make-attrs (length order))))
      (dolist (k order)
        (puthash k (vconcat (nreverse (gethash k groups))) tbl))
      (nixel--mkattrs tbl))))

(nixel-defbuiltin sort (f l)
  (let ((v (nixel--list-elems l)))
    (if (< (length v) 2)
        v
      (sort (copy-sequence v)
            :lessp (lambda (a b) (nixel-force-bool (nixel--call2 f a b)))))))

;;;; Attribute sets

(nixel-defbuiltin attrNames (s)
  (let ((a (nixel-force-attrs s)))
    (vconcat (mapcar #'symbol-name (nixel-attr-keys a)))))

(nixel-defbuiltin attrValues (s)
  (let* ((a (nixel-force-attrs s))
         (tbl (aref a 1)))
    (vconcat (mapcar (lambda (k) (gethash k tbl)) (nixel-attr-keys a)))))

(nixel-defbuiltin getAttr (name s)
  (nixel-sel (nixel-force-attrs s)
             (intern (nixel--unibyte (nixel-force-string-no-ctx name)))))

(nixel-defbuiltin hasAttr (name s)
  (let ((sym (intern (nixel--unibyte (nixel-force-string-no-ctx name))))
        (a (nixel-force-attrs s)))
    (not (eq (gethash sym (aref a 1) 'nixel--missing) 'nixel--missing))))

(nixel-defbuiltin removeAttrs (s names)
  (let* ((a (nixel-force-attrs s))
         (ns (nixel--list-elems names))
         (syms (mapcar (lambda (n) (intern (nixel--unibyte
                                            (nixel-force-string-no-ctx n))))
                       ns))
         (tbl (aref a 1)))
    (if (cl-notany (lambda (k) (not (eq (gethash k tbl 'nixel--missing)
                                        'nixel--missing)))
                   syms)
        a
      (let ((new (copy-hash-table tbl)))
        (dolist (k syms) (remhash k new))
        (nixel--mkattrs new (aref a 2))))))

(nixel-defbuiltin listToAttrs (l)
  (let* ((v (nixel--list-elems l))
         (tbl (nixel-make-attrs (length v))))
    (mapc (lambda (e)
            (let* ((es (nixel-force-attrs e))
                   (n (gethash 'name (aref es 1) 'nixel--missing)))
              (when (eq n 'nixel--missing)
                (nixel-fail "attribute 'name' missing in a call to 'listToAttrs'"))
              (let ((sym (intern (nixel--unibyte (nixel-force-string-no-ctx n)))))
                (when (eq (gethash sym tbl 'nixel--missing) 'nixel--missing)
                  (let ((val (gethash 'value (aref es 1) 'nixel--missing)))
                    (when (eq val 'nixel--missing)
                      (nixel-fail "attribute 'value' missing in a call to 'listToAttrs'"))
                    (puthash sym val tbl))))))
          v)
    (nixel--mkattrs tbl)))

(nixel-defbuiltin mapAttrs (f s)
  (let* ((a (nixel-force-attrs s))
         (tbl (make-hash-table :test 'eq :size (hash-table-count (aref a 1)))))
    (maphash (lambda (k x)
               (let ((name (symbol-name k)))
                 (puthash k (nixel--delay (nixel--call2 f name x)) tbl)))
             (aref a 1))
    (nixel--mkattrs tbl (aref a 2))))

(nixel-defbuiltin intersectAttrs (e1 e2)
  (let* ((a (nixel-force-attrs e1))
         (b (nixel-force-attrs e2))
         (ta (aref a 1)) (tb (aref b 1))
         (tbl (nixel-make-attrs)))
    (if (< (hash-table-count ta) (hash-table-count tb))
        (maphash (lambda (k _v)
                   (let ((y (gethash k tb 'nixel--missing)))
                     (unless (eq y 'nixel--missing) (puthash k y tbl))))
                 ta)
      (maphash (lambda (k y)
                 (unless (eq (gethash k ta 'nixel--missing) 'nixel--missing)
                   (puthash k y tbl)))
               tb))
    (nixel--mkattrs tbl (aref b 2))))

(nixel-defbuiltin catAttrs (name l)
  (let ((sym (intern (nixel--unibyte (nixel-force-string-no-ctx name))))
        (out nil))
    (mapc (lambda (e)
            (let* ((es (nixel-force-attrs e))
                   (x (gethash sym (aref es 1) 'nixel--missing)))
              (unless (eq x 'nixel--missing) (push x out))))
          (nixel--list-elems l))
    (vconcat (nreverse out))))

(nixel-defbuiltin zipAttrsWith (f l)
  (let ((groups (make-hash-table :test 'eq)))
    (mapc (lambda (e)
            (let ((es (nixel-force-attrs e)))
              (maphash (lambda (k x) (puthash k (cons x (gethash k groups)) groups))
                       (aref es 1))))
          (nixel--list-elems l))
    (let ((tbl (nixel-make-attrs (hash-table-count groups))))
      (maphash (lambda (k xs)
                 (let ((name (symbol-name k))
                       (vals (vconcat (nreverse xs))))
                   (puthash k (nixel--delay (nixel--call2 f name vals)) tbl)))
               groups)
      (nixel--mkattrs tbl))))

(nixel-defbuiltin functionArgs (f)
  (let ((v (nixel-force f)))
    (cond
     ((nixel--lam-p v)
      (nixel-attrs-from-alist
       (mapcar (lambda (fm) (cons (car fm) (nixel--mkbool (cdr fm)))) (aref v 2))))
     ((nixel-function-p v) nixel--empty-attrs)
     (t (nixel--type-error "a function" v)))))

(nixel-defbuiltin unsafeGetAttrPos (name s)
  (let* ((sym (intern (nixel--unibyte (nixel-force-string-no-ctx name))))
         (a (nixel-force-attrs s))
         (pos (nixel--attrs-pos-lookup a sym)))
    (if pos
        (nixel-attrs-from-alist
         (list (cons 'file (car pos)) (cons 'line (cadr pos))
               (cons 'column (cddr pos))))
      :null)))

(nixel-defbuiltin genericClosure (args)
  (let* ((a (nixel-force-attrs args))
         (start (gethash 'startSet (aref a 1) 'nixel--missing))
         (op (gethash 'operator (aref a 1) 'nixel--missing)))
    (when (eq start 'nixel--missing)
      (nixel-fail "attribute 'startSet' required"))
    (when (eq op 'nixel--missing)
      (nixel-fail "attribute 'operator' required"))
    (let ((work (append (nixel--list-elems start) nil))
          (done nil)
          (keys nil))
      (while work
        (let* ((e (pop work))
               (es (nixel-force-attrs e))
               (k (gethash 'key (aref es 1) 'nixel--missing)))
          (when (eq k 'nixel--missing)
            (nixel-fail "attribute 'key' required"))
          (let ((kv (nixel-force k)))
            (unless (cl-some (lambda (seen)
                               (and (not (nixel-less-than kv seen))
                                    (not (nixel-less-than seen kv))))
                             keys)
              (push kv keys)
              (push e done)
              (setq work (append work
                                 (append (nixel-force-list (nixel-app op e))
                                         nil)))))))
      (vconcat (nreverse done)))))

;;;; Strings

(nixel-defbuiltin toString (x)
  (nixel-coerce-to-string (nixel-force x) nil t))

(nixel-defbuiltin stringLength (s)
  (length (nixel--sval (nixel--coerce s t))))

(nixel-defbuiltin substring (start len s)
  (let ((st (nixel-force-int start))
        (ln (nixel-force-int len)))
    (when (< st 0)
      (nixel-fail "negative start position in 'substring'"))
    (let* ((str (nixel--coerce s t))
           (text (nixel--sval str))
           (n (length text)))
      (if (>= st n)
          (nixel--mkstr "" (nixel--sctx str))
        (nixel--mkstr (substring text st (if (or (< ln 0) (> (+ st ln) n))
                                             n
                                           (+ st ln)))
                      (nixel--sctx str))))))

(nixel-defbuiltin concatStringsSep (sep l)
  (let* ((sv (nixel-force-string sep))
         (ctxs (list (nixel--sctx sv)))
         (parts (mapcar (lambda (x)
                          (let ((s (nixel--coerce x t)))
                            (push (nixel--sctx s) ctxs)
                            (nixel--sval s)))
                        (nixel--list-elems l))))
    (nixel--mkstr (mapconcat #'identity parts (nixel--sval sv))
                  (nixel--ctx-union ctxs))))

(nixel-defbuiltin replaceStrings (from to s)
  (let* ((fv (nixel--list-elems from))
         (tv (nixel--list-elems to)))
    (unless (= (length fv) (length tv))
      (nixel-fail "'from' and 'to' arguments passed to builtins.replaceStrings have different lengths"))
    (let* ((froms (mapcar #'nixel-force-string-text fv))
           (tos (mapcar #'nixel-force-string tv))
           (sv (nixel-force-string s))
           (text (nixel--sval sv))
           (n (length text))
           (ctxs (list (nixel--sctx sv)))
           (out nil)
           (p 0))
      (while (<= p n)
        (let ((fs froms) (ts tos) (found nil))
          (while (and fs (not found))
            (let ((f (car fs)))
              (when (and (<= (+ p (length f)) n)
                         (eq t (compare-strings f nil nil text p (+ p (length f)))))
                (setq found t)
                (push (nixel--sval (car ts)) out)
                (push (nixel--sctx (car ts)) ctxs)
                (if (= (length f) 0)
                    (progn
                      (when (< p n) (push (substring text p (1+ p)) out))
                      (setq p (1+ p)))
                  (setq p (+ p (length f))))))
            (setq fs (cdr fs) ts (cdr ts)))
          (unless found
            (when (< p n) (push (substring text p (1+ p)) out))
            (setq p (1+ p)))))
      (nixel--mkstr (apply #'concat (nreverse out)) (nixel--ctx-union ctxs)))))

(defun nixel--base-name-of (path)
  (if (= (length path) 0)
      ""
    (let* ((last (1- (length path))))
      (when (and (eq (aref path last) ?/) (> last 0))
        (setq last (1- last)))
      (let ((pos (cl-position ?/ path :end (1+ last) :from-end t)))
        (substring path (if pos (1+ pos) 0) (1+ last))))))

(nixel-defbuiltin baseNameOf (x)
  (let ((s (nixel--coerce x nil)))
    (nixel--mkstr (nixel--base-name-of (nixel--sval s)) (nixel--sctx s))))

(defun nixel--dir-of (path)
  (let ((pos (cl-position ?/ path :from-end t)))
    (cond ((null pos) ".")
          ((= pos 0) "/")
          (t (substring path 0 pos)))))

(nixel-defbuiltin dirOf (x)
  (let ((v (nixel-force x)))
    (if (nixel--path-p v)
        (let ((p (aref v 1)))
          (if (string= p "/") v (nixel--mkpath (nixel--dir-of p))))
      (let ((s (nixel-coerce-to-string v nil)))
        (nixel--mkstr (nixel--dir-of (nixel--sval s)) (nixel--sctx s))))))

(nixel-defbuiltin hashString (algo s)
  (nixel-hash-string (nixel-force-string-no-ctx algo)
                     (nixel-force-string-text s)))

(nixel-defbuiltin parseDrvName (s)
  (let* ((text (nixel-force-string-text s))
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
    (nixel-attrs-from-alist (list (cons 'name name) (cons 'version version)))))

(defsubst nixel--digit-p (c) (and (>= c ?0) (<= c ?9)))

(defun nixel--version-next (s p)
  "The next version component of S at P: (COMPONENT . NEXT-P)."
  (let ((n (length s)))
    (while (and (< p n) (memq (aref s p) '(?. ?-))) (setq p (1+ p)))
    (if (>= p n)
        (cons "" p)
      (let ((start p))
        (if (nixel--digit-p (aref s p))
            (while (and (< p n) (nixel--digit-p (aref s p))) (setq p (1+ p)))
          (while (and (< p n)
                      (not (nixel--digit-p (aref s p)))
                      (not (memq (aref s p) '(?. ?-))))
            (setq p (1+ p))))
        (cons (substring s start p) p)))))

(defun nixel--version-int (c)
  "C as Nix's string2Int<int> reads it, or nil."
  (and (> (length c) 0)
       (cl-every #'nixel--digit-p c)
       (let ((n (string-to-number c)))
         (and (<= n 2147483647) n))))

(defun nixel--components-lt (c1 c2)
  (let ((n1 (nixel--version-int c1))
        (n2 (nixel--version-int c2)))
    (cond
     ((and n1 n2) (< n1 n2))
     ((and (string= c1 "") n2) t)
     ((and (string= c1 "pre") (not (string= c2 "pre"))) t)
     ((string= c2 "pre") nil)
     (n2 t)
     (n1 nil)
     (t (string< c1 c2)))))

(defun nixel-compare-versions (v1 v2)
  "Compare the version strings V1 and V2 as Nix does: -1, 0 or 1."
  (let ((p1 0) (p2 0) (result 0))
    (while (and (= result 0)
                (or (< p1 (length v1)) (< p2 (length v2))))
      (let ((c1 (nixel--version-next v1 p1))
            (c2 (nixel--version-next v2 p2)))
        (setq p1 (cdr c1) p2 (cdr c2))
        (cond ((nixel--components-lt (car c1) (car c2)) (setq result -1))
              ((nixel--components-lt (car c2) (car c1)) (setq result 1)))))
    result))

(nixel-defbuiltin compareVersions (a b)
  (nixel-compare-versions (nixel-force-string-text a)
                          (nixel-force-string-text b)))

(nixel-defbuiltin splitVersion (v)
  (let ((s (nixel-force-string-text v)) (p 0) (out nil) (done nil))
    (while (and (not done) (< p (length s)))
      (let ((c (nixel--version-next s p)))
        (setq p (cdr c))
        (if (string= (car c) "")
            (setq done t)
          (push (car c) out))))
    (vconcat (nreverse out))))

(nixel-defbuiltin match (re s)
  (nixel-regex-match (nixel-force-string-text re) (nixel-force-string-text s)))

(nixel-defbuiltin split (re s)
  (nixel-regex-split (nixel-force-string-text re) (nixel-force-string-text s)))

;;;; String contexts

(nixel-defbuiltin unsafeDiscardStringContext (s)
  (nixel--sval (nixel--coerce s t)))

(nixel-defbuiltin hasContext (s)
  (nixel--mkbool (nixel--sctx (nixel-force-string s))))

(nixel-defbuiltin unsafeDiscardOutputDependency (s)
  (let ((sv (nixel--coerce s t)))
    (nixel--mkstr (nixel--sval sv)
                  (delete-dups
                   (mapcar (lambda (c) (if (eq (aref c 0) ?=) (substring c 1) c))
                           (nixel--sctx sv))))))

(nixel-defbuiltin addDrvOutputDependencies (s)
  (let* ((sv (nixel--coerce s t))
         (ctx (nixel--sctx sv)))
    (unless (= (length ctx) 1)
      (nixel-fail "context of string '%s' must have exactly one element, but has %d"
                  (nixel--sval sv) (length ctx)))
    (let ((c (car ctx)))
      (cond
       ((eq (aref c 0) ?=) sv)
       ((eq (aref c 0) ?!)
        (nixel-fail "`addDrvOutputDependencies` can only act on derivations, not on a derivation output such as '%s'"
                    c))
       ((string-suffix-p ".drv" c) (nixel--mkstr (nixel--sval sv) (list (concat "=" c))))
       (t (nixel-fail "path '%s' is not a derivation" c))))))

(defun nixel--context-info (ctx)
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

(nixel-defbuiltin getContext (s)
  (let* ((sv (nixel-force-string s))
         (info (nixel--context-info (nixel--sctx sv)))
         (tbl (nixel-make-attrs (length info))))
    (dolist (e info)
      (let ((alist nil))
        (when (nth 3 e)
          (push (cons 'outputs (vconcat (sort (delete-dups (copy-sequence (nth 3 e)))
                                              #'string<)))
                alist))
        (when (nth 2 e) (push (cons 'allOutputs t) alist))
        (when (nth 1 e) (push (cons 'path t) alist))
        (puthash (intern (car e)) (nixel-attrs-from-alist alist) tbl)))
    (nixel--mkattrs tbl)))

(nixel-defbuiltin appendContext (s ctx)
  (let* ((sv (nixel-force-string s))
         (c (nixel-force-attrs ctx))
         (new (copy-sequence (nixel--sctx sv))))
    (maphash
     (lambda (k v)
       (let* ((path (symbol-name k))
              (info (nixel-force-attrs v))
              (tbl (aref info 1)))
         (unless (string-prefix-p (concat nixel-store-dir "/") path)
           (nixel-fail "context key '%s' is not a store path" path))
         (let ((p (gethash 'path tbl 'nixel--missing)))
           (when (and (not (eq p 'nixel--missing)) (nixel-force-bool p))
             (push path new)))
         (let ((a (gethash 'allOutputs tbl 'nixel--missing)))
           (when (and (not (eq a 'nixel--missing)) (nixel-force-bool a))
             (unless (string-suffix-p ".drv" path)
               (nixel-fail "tried to add all-outputs context of %s, which is not a derivation, to a string"
                           path))
             (push (concat "=" path) new)))
         (let ((o (gethash 'outputs tbl 'nixel--missing)))
           (unless (eq o 'nixel--missing)
             (let ((outs (nixel-force-list o)))
               (when (and (> (length outs) 0) (not (string-suffix-p ".drv" path)))
                 (nixel-fail "tried to add derivation output context of %s, which is not a derivation, to a string"
                             path))
               (mapc (lambda (out)
                       (push (concat "!" (nixel-force-string-text out) "!" path) new))
                     outs))))))
     (aref c 1))
    (nixel--mkstr (nixel--sval sv) (delete-dups new))))

;;;; Hashes and the store

(nixel-defbuiltin placeholder (out)
  (nixel-store-placeholder (nixel-force-string-text out)))

(nixel-defbuiltin toFile (name s)
  (let* ((n (nixel-force-string-no-ctx name))
         (sv (nixel-force-string s))
         (refs nil))
    (dolist (c (nixel--sctx sv))
      (when (memq (aref c 0) '(?= ?!))
        (nixel-fail "files created by builtins.toFile may not reference derivations, but %s references %s"
                    n c))
      (push c refs))
    (let ((p (nixel-store-add-text n (nixel--sval sv) refs)))
      (nixel--mkstr p (list p)))))

(nixel-defbuiltin convertHash (args)
  (let* ((a (nixel-force-attrs args))
         (tbl (aref a 1))
         (hash (nixel-force-string-text (gethash 'hash tbl)))
         (algo (let ((x (gethash 'hashAlgo tbl 'nixel--missing)))
                 (and (not (eq x 'nixel--missing)) (nixel-force-string-text x))))
         (to (nixel-force-string-text
              (let ((x (gethash 'toHashFormat tbl 'nixel--missing)))
                (when (eq x 'nixel--missing)
                  (nixel-fail "attribute 'toHashFormat' missing"))
                x))))
    (nixel-convert-hash hash algo to)))

;;;; Files

(defun nixel--real-path (v &optional what)
  "The absolute path text of the path or string value V."
  (let ((x (nixel-force v)))
    (cond
     ((nixel--path-p x) (aref x 1))
     ((nixel-string-p x)
      (let ((s (nixel--sval x)))
        (unless (string-prefix-p "/" s)
          (nixel-fail "string '%s' doesn't represent an absolute path%s" s
                      (if what (concat " " what) "")))
        (nixel-canon-path s)))
     ((nixel--attrs-p x)
      (nixel--sval (nixel-coerce-to-string x nil)))
     (t (nixel--type-error "a path" x)))))

(nixel-defbuiltin import (p)
  (nixel-import-path (nixel--real-path p)))

(nixel-defbuiltin scopedImport (_scope _p)
  (nixel-fail "builtins.scopedImport is not supported by nixel"))

(nixel-defbuiltin pathExists (p)
  ;; A string ending in / or /. must name a directory, through any
  ;; symlink; otherwise the path itself is looked at, so a dangling
  ;; symlink exists.
  (let* ((x (nixel-force p))
         (must-dir (and (nixel-string-p x)
                        (let ((s (nixel--sval x)))
                          (or (string-suffix-p "/" s) (string-suffix-p "/." s)))))
         (path (nixel--real-path x)))
    (nixel--mkbool
     (if must-dir
         (file-directory-p path)
       (file-attributes path)))))

(defun nixel-read-file-bytes (path)
  "The contents of the file PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (condition-case err
        (insert-file-contents-literally path)
      (file-missing (nixel-fail "opening file '%s': No such file or directory"
                                path))
      (file-error (nixel-fail "reading file '%s': %s" path (error-message-string err))))
    (buffer-string)))

(nixel-defbuiltin readFile (p)
  (let ((path (nixel--real-path p)))
    (when (file-directory-p path)
      (nixel-fail "error: reading from file '%s': Is a directory" path))
    (nixel-read-file-bytes path)))

(defun nixel--file-type (path)
  "The Nix file type name of PATH, not following a final symlink."
  (let ((attrs (file-attributes path)))
    (unless attrs
      (nixel-fail "getting status of '%s': No such file or directory" path))
    (let ((ty (file-attribute-type attrs)))
      (cond ((eq ty t) "directory")
            ((stringp ty) "symlink")
            ((null ty)
             (if (string-prefix-p "-" (file-attribute-modes attrs))
                 "regular"
               "unknown"))
            (t "unknown")))))

(nixel-defbuiltin readFileType (p)
  (nixel--file-type (nixel--real-path p)))

(nixel-defbuiltin readDir (p)
  (let* ((path (nixel--real-path p))
         (names (condition-case nil
                    (directory-files path nil nil t)
                  (file-error (nixel-fail "reading directory '%s'" path))))
         (tbl (nixel-make-attrs (length names))))
    (dolist (n names)
      (unless (member n '("." ".."))
        (let ((un (encode-coding-string n 'utf-8-unix t)))
          (puthash (intern un)
                   (nixel--file-type (concat path "/" n))
                   tbl))))
    (nixel--mkattrs tbl)))

(nixel-defbuiltin hashFile (algo p)
  (nixel-hash-string (nixel-force-string-no-ctx algo)
                     (nixel-read-file-bytes (nixel--real-path p))))

(nixel-defbuiltin filterSource (f p)
  (let ((path (nixel--real-path p)))
    (let ((sp (nixel-store-add-filtered path (nixel--base-name-of path) f t nil)))
      (nixel--mkstr sp (list sp)))))

(nixel-defbuiltin path (args)
  (let* ((a (nixel-force-attrs args))
         (tbl (aref a 1))
         (get (lambda (k) (let ((x (gethash k tbl 'nixel--missing)))
                            (if (eq x 'nixel--missing) nil x))))
         (pv (funcall get 'path)))
    (unless pv (nixel-fail "missing required 'path' attribute in the first argument to builtins.path"))
    (let* ((path (nixel--real-path pv))
           (name (let ((n (funcall get 'name)))
                   (if n (nixel-force-string-no-ctx n) (nixel--base-name-of path))))
           (filter (funcall get 'filter))
           (recursive (let ((r (funcall get 'recursive)))
                        (if r (nixel-force-bool r) t)))
           (sha (let ((s (funcall get 'sha256)))
                  (and s (nixel-force-string-no-ctx s))))
           (sp (nixel-store-add-filtered path name filter recursive sha)))
      (nixel--mkstr sp (list sp)))))

(nixel-defbuiltin findFile (search name)
  (let ((entries (nixel--list-elems search))
        (n (nixel-force-string-text name)))
    (nixel--find-in entries n)))

(defun nixel--find-in (entries name)
  (catch 'found
    (mapc
     (lambda (e)
       (let* ((es (nixel-force-attrs e))
              (prefix (let ((x (gethash 'prefix (aref es 1) 'nixel--missing)))
                        (if (eq x 'nixel--missing) "" (nixel-force-string-text x))))
              (path (nixel--sval (nixel--coerce (gethash 'path (aref es 1)) nil))))
         (when (or (string= prefix "")
                   (string= name prefix)
                   (string-prefix-p (concat prefix "/") name))
           (let* ((rest (if (string= prefix "") name
                          (substring name (min (length name) (1+ (length prefix))))))
                  (full (nixel-canon-path
                         (if (string= rest "") path (concat path "/" rest)))))
             (when (file-exists-p full)
               (throw 'found (nixel--mkpath full)))))))
     entries)
    (nixel--find-corepkg name)))

(defun nixel--find-corepkg (name)
  (if (and nixel-corepkgs-dir (string-prefix-p "nix/" name)
           (file-exists-p (expand-file-name (substring name 4) nixel-corepkgs-dir)))
      (nixel--mkpath (expand-file-name (substring name 4) nixel-corepkgs-dir))
    (nixel-fail "file '%s' was not found in the Nix search path (add it using $NIX_PATH or -I)"
                name)))

(defun nixel--find-file (name)
  "The path `<NAME>' denotes."
  (nixel--find-in (nixel--nix-path-value) name))

(nixel-defbuiltin getEnv (name)
  (let ((n (nixel-force-string-no-ctx name)))
    (or (and nixel-impure (getenv n)) "")))

(nixel-defbuiltin toPath (p)
  (let ((x (nixel-force p)))
    (let ((s (nixel-coerce-to-string x nil)))
      (unless (string-prefix-p "/" (nixel--sval s))
        (nixel-fail "string '%s' doesn't represent an absolute path" (nixel--sval s)))
      (nixel--mkstr (nixel-canon-path (nixel--sval s)) (nixel--sctx s)))))

(nixel-defbuiltin storePath (p)
  (let* ((path (nixel--real-path p)))
    (unless (string-prefix-p (concat nixel-store-dir "/") path)
      (nixel-fail "path '%s' is not in the Nix store" path))
    (let* ((rest (substring path (1+ (length nixel-store-dir))))
           (root (concat nixel-store-dir "/" (car (split-string rest "/")))))
      (nixel--mkstr path (list root)))))

(nixel-defbuiltin currentTime ()
  (nixel-fail "builtins.currentTime is not available in pure evaluation"))

;;;; Serialisation

(nixel-defbuiltin toJSON (x)
  (nixel-to-json (nixel-force x)))

(nixel-defbuiltin fromJSON (s)
  (nixel-from-json (nixel-force-string-text s)))

(nixel-defbuiltin fromTOML (s)
  (nixel-from-toml (nixel-force-string-text s)))

(nixel-defbuiltin toXML (x)
  (nixel-to-xml (nixel-force x)))

;;;; Derivations

(nixel-defbuiltin derivationStrict (attrs)
  (nixel-derivation-strict (nixel-force-attrs attrs)))

;;;; Fetchers and flakes

(dolist (name '("fetchurl" "fetchTarball" "fetchGit" "fetchMercurial"
                "fetchTree" "getFlake" "fetchClosure"))
  (let ((n name))
    (nixel--register-builtin
     n 1 (lambda (_x) (nixel-fail "builtins.%s is not supported by nixel: evaluation does not fetch" n)))))

(dolist (name '("parseFlakeRef" "flakeRefToString"))
  (let ((n name))
    (nixel--register-builtin
     n 1 (lambda (_x) (nixel-fail "builtins.%s is not supported by nixel" n)))))

(provide 'nixel-builtins)

;;; nixel-builtins.el ends here
