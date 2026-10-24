;;; elnix-runtime.el --- Values and operations of compiled Nix -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; How a Nix value lives in Emacs Lisp, and the operations compiled
;; code calls.
;;
;;   int      an integer, kept within the signed 64-bit range
;;   float    a float
;;   bool     t or nil
;;   null     the keyword :null
;;   string   a unibyte string, or a `elnix--cstr' record when the
;;            string carries a context
;;   path     a `elnix--path' record holding the absolute path
;;   list     a vector of lazy values
;;   attrs    a `elnix--attrs' record around an eq hash table from
;;            interned symbols to lazy values
;;   lambda   a closure of one argument, or a `elnix--lam' record for a
;;            lambda with formals; primops are closures too
;;
;; A lazy value is a value or a thunk.  A thunk is a cons cell, which
;; no value is: (CODE . nil) before it is forced, with CODE a closure
;; of no arguments; (elnix--blackhole . CODE) while it is being forced;
;; (nil . VALUE) afterwards.  `elnix-force' turns a lazy value into a
;; value.

;;; Code:

(require 'cl-lib)
(require 'elnix-parse)

(define-error 'elnix-eval-error "Nix evaluation error" 'elnix-error)
(define-error 'elnix-throw "Nix throw" 'elnix-eval-error)
(define-error 'elnix-assertion "Nix assertion failure" 'elnix-eval-error)
(define-error 'elnix-abort "Nix abort" 'elnix-eval-error)
(define-error 'elnix-infinite-recursion "infinite recursion encountered"
              'elnix-eval-error)

(defun elnix-fail (fmt &rest args)
  "Signal a Nix evaluation error with the message FMT and ARGS."
  (signal 'elnix-eval-error (list (apply #'format fmt args))))

;;;; Thunks

(defmacro elnix--delay (&rest body)
  "A thunk whose value is that of BODY."
  `(list (lambda () ,@body)))

(defvar elnix--forcing nil
  "The thunks being forced, innermost first.
A thunk that fails while it is being forced is put back to its
unforced state by the handler that catches the failure; see
`elnix--restore-forcing'.")

(defun elnix--force-thunk (th)
  "Force the thunk TH and return its value."
  (let ((code (car th)))
    (cond
     ((null code) (cdr th))
     ((eq code 'elnix--blackhole)
      (signal 'elnix-infinite-recursion
              (list "infinite recursion encountered")))
     (t
      (setcar th 'elnix--blackhole)
      (setcdr th code)
      (let ((saved elnix--forcing))
        (setq elnix--forcing (cons th saved))
        (let ((v (funcall code)))
          (while (consp v) (setq v (elnix--force-thunk v)))
          (setq elnix--forcing saved)
          (setcar th nil)
          (setcdr th v)
          v))))))

(defsubst elnix-force (v)
  "The value of the lazy value V."
  (if (consp v) (elnix--force-thunk v) v))

(defun elnix--restore-forcing (saved)
  "Return the thunks forced since SAVED to their unforced state.
Called by a handler that caught an error raised while the stack of
thunks in `elnix--forcing' had grown past SAVED."
  (while (and elnix--forcing (not (eq elnix--forcing saved)))
    (let ((th (car elnix--forcing)))
      (when (eq (car th) 'elnix--blackhole)
        (setcar th (cdr th))
        (setcdr th nil)))
    (setq elnix--forcing (cdr elnix--forcing))))

(defmacro elnix--catching (handlers &rest body)
  "Run BODY, restoring interrupted thunks before running a handler.
HANDLERS are `condition-case' handlers that do not bind a variable;
each is (CONDITIONS VAR-SYM FORMS...) where VAR-SYM names the error
data in FORMS."
  (let ((saved (make-symbol "saved")))
    `(let ((,saved elnix--forcing))
       (condition-case elnix--err
           (progn ,@body)
         ,@(mapcar (lambda (h)
                     `(,(car h)
                       (elnix--restore-forcing ,saved)
                       (let ((,(nth 1 h) elnix--err)) ,@(nthcdr 2 h))))
                   handlers)))))

(defun elnix--run (fn)
  "Call FN.  Compiled code uses this to keep a closure out of line."
  (funcall fn))

;;;; Records

(defsubst elnix--tag (v)
  "The record tag of V, or nil when V is not a record."
  (and (recordp v) (aref v 0)))

(defsubst elnix--attrs-p (v) (eq (elnix--tag v) 'elnix--attrs))
(defsubst elnix--cstr-p (v) (eq (elnix--tag v) 'elnix--cstr))
(defsubst elnix--path-p (v) (eq (elnix--tag v) 'elnix--path))
(defsubst elnix--lam-p (v) (eq (elnix--tag v) 'elnix--lam))
(defsubst elnix--primop-p (v) (eq (elnix--tag v) 'elnix--primop))

(defsubst elnix-string-p (v) (or (stringp v) (elnix--cstr-p v)))
(defsubst elnix-function-p (v)
  (or (functionp v) (memq (elnix--tag v) '(elnix--lam elnix--primop))))
(defsubst elnix-bool-p (v) (or (eq v t) (null v)))

;;;; Types

(defun elnix-type-of (v)
  "The Nix type name of the value V, as `builtins.typeOf' reports it."
  (cond
   ((integerp v) "int")
   ((floatp v) "float")
   ((or (eq v t) (null v)) "bool")
   ((eq v :null) "null")
   ((stringp v) "string")
   ((vectorp v) "list")
   ((recordp v)
    (pcase (aref v 0)
      ('elnix--attrs "set")
      ('elnix--cstr "string")
      ('elnix--path "path")
      ((or 'elnix--lam 'elnix--primop) "lambda")
      (_ (error "elnix: not a Nix value: %S" v))))
   ((functionp v) "lambda")
   (t (error "elnix: not a Nix value: %S" v))))

(defun elnix-show-type (v)
  "A description of the type of V for error messages, as Nix words it."
  (pcase (elnix-type-of v)
    ("int" "an integer")
    ("float" "a float")
    ("bool" "a Boolean")
    ("null" "null")
    ("string" "a string")
    ("list" "a list")
    ("set" "a set")
    ("path" "a path")
    ("lambda" "a function")))

(defun elnix--type-error (expected v)
  "Signal that V was expected to be EXPECTED."
  (elnix-fail "expected %s but found %s" expected (elnix-show-type v)))

;;;; Integers

(defconst elnix--int-max 9223372036854775807)
(defconst elnix--int-min -9223372036854775808)

(defsubst elnix--check-int (n what)
  "Return N, or signal an overflow in WHAT when N leaves 64 bits."
  (if (or (fixnump n) (and (<= n elnix--int-max) (>= n elnix--int-min)))
      n
    (elnix-fail "integer overflow in %s" what)))

;;;; Booleans and checked forcing

(defsubst elnix--bool (v)
  "V, which must be a Boolean, as t or nil."
  (cond ((eq v t) t)
        ((null v) nil)
        (t (elnix-fail "expected a Boolean but found %s" (elnix-show-type v)))))

(defun elnix-force-bool (v) (elnix--bool (elnix-force v)))

(defun elnix-force-int (v)
  (let ((x (elnix-force v)))
    (if (integerp x) x (elnix--type-error "an integer" x))))

(defun elnix-force-list (v)
  (let ((x (elnix-force v)))
    (if (vectorp x) x (elnix--type-error "a list" x))))

(defun elnix-force-attrs (v)
  (let ((x (elnix-force v)))
    (if (elnix--attrs-p x) x (elnix--type-error "a set" x))))

(defun elnix-force-function (v)
  (let ((x (elnix-force v)))
    (if (elnix-function-p x) x (elnix--type-error "a function" x))))

;;;; Strings and contexts

;; A context is a list of strings in Nix's own spelling: a store path
;; for a plain dependency, `!OUTPUT!DRV' for one output of a derivation,
;; and `=DRV' for a derivation with its whole closure.

(defsubst elnix--mkstr (s ctx)
  "The Nix string of S with context CTX."
  (if ctx (record 'elnix--cstr s ctx) s))

(defsubst elnix--sval (v)
  "The text of the string value V."
  (if (stringp v) v (aref v 1)))

(defsubst elnix--sctx (v)
  "The context of the string value V."
  (if (stringp v) nil (aref v 2)))

(defun elnix--ctx-union (ctxs)
  "The union of the list of contexts CTXS."
  (let ((nonempty (delq nil ctxs)))
    (cond
     ((null nonempty) nil)
     ((null (cdr nonempty)) (car nonempty))
     ;; A context list belongs to the string value that holds it, and
     ;; values share them, so the union is built from copies:
     ;; `delete-dups' edits its argument, and `append' would leave the
     ;; last list in the result uncopied.
     (t (delete-dups (apply #'append (append nonempty (list nil))))))))

(defun elnix-force-string (v)
  "Force V to a string value, keeping its context."
  (let ((x (elnix-force v)))
    (if (elnix-string-p x) x (elnix--type-error "a string" x))))

(defun elnix-force-string-text (v)
  "Force V to a string and return its text."
  (elnix--sval (elnix-force-string v)))

(defun elnix-force-string-no-ctx (v)
  "Force V to a string without context and return its text."
  (let ((x (elnix-force-string v)))
    (when (elnix--sctx x)
      (elnix-fail "the string '%s' is not allowed to refer to a store path (such as '%s')"
                  (elnix--sval x) (car (elnix--sctx x))))
    (elnix--sval x)))

(defsubst elnix--unibyte (s)
  "S as a unibyte string of its UTF-8 bytes."
  (if (multibyte-string-p s)
      (if (string-match-p "\\`[[:ascii:]]*\\'" s)
          (string-to-unibyte s)
        (encode-coding-string s 'utf-8-unix t))
    s))

;;;; Paths

(defsubst elnix--mkpath (abs)
  "The path value of the absolute path string ABS."
  (record 'elnix--path abs))

(defsubst elnix--path-str (p) (aref p 1))

(defun elnix-canon-path (s)
  "Canonicalise the absolute path S the way Nix does.
Collapse repeated slashes, drop `.' components and resolve `..'
components lexically; no symlink is followed."
  (let ((parts nil))
    (dolist (c (split-string s "/" t))
      (cond
       ((string= c "."))
       ((string= c "..") (when parts (pop parts)))
       (t (push c parts))))
    (if parts
        (mapconcat (lambda (p) (concat "/" p)) (nreverse parts) "")
      "/")))

(defun elnix--path-join (dir rel)
  "The canonical path of REL relative to the directory DIR."
  (elnix-canon-path (if (string-prefix-p "/" rel) rel (concat dir "/" rel))))

;;;; Attribute sets

;; An attribute set is [elnix--attrs TABLE POS KEYS].  TABLE maps
;; symbols to lazy values.  POS answers `unsafeGetAttrPos': nil, a
;; position table [FILE ALIST] of a literal set, where ALIST maps
;; symbols to (LINE . COLUMN), or (OVER . BASE), the positions of a set
;; made by `//'.  KEYS caches the sorted vector of names.

(defsubst elnix--attrs-table (a) (aref a 1))

(defsubst elnix--mkattrs (table &optional pos)
  (record 'elnix--attrs table pos nil))

(defconst elnix--empty-attrs
  (elnix--mkattrs (make-hash-table :test 'eq :size 0))
  "The empty attribute set.")

(defun elnix-make-attrs (&optional size)
  "A fresh hash table for an attribute set of about SIZE attributes."
  (make-hash-table :test 'eq :size (or size 0)))

(defsubst elnix--attr (a sym)
  "The lazy value of attribute SYM of the set A, or `elnix--missing'."
  (gethash sym (aref a 1) 'elnix--missing))

(defun elnix-attr-count (a) (hash-table-count (aref a 1)))

(defun elnix--sym-less (x y)
  (string< (symbol-name x) (symbol-name y)))

(defun elnix-attr-keys (a)
  "The vector of the attribute names of set A as symbols, sorted bytewise."
  (or (aref a 3)
      (aset a 3 (let ((keys (hash-table-keys (aref a 1))))
                  (vconcat (sort keys #'elnix--sym-less))))))

(defsubst elnix-intern (s)
  "The symbol of the attribute name S."
  (intern (elnix--unibyte s)))

(defun elnix-attrs-from-alist (alist &optional pos)
  "An attribute set of ALIST, a list of (SYMBOL . LAZY)."
  (let ((tbl (elnix-make-attrs (length alist))))
    (dolist (e alist) (puthash (car e) (cdr e) tbl))
    (elnix--mkattrs tbl pos)))

(defun elnix--attrs-pos-lookup (a sym)
  "The (FILE LINE . COLUMN) of attribute SYM of set A, or nil."
  (let ((pos (aref a 2)))
    (elnix--pos-lookup pos sym)))

(defun elnix--pos-lookup (pos sym)
  (cond
   ((null pos) nil)
   ((vectorp pos)
    (let ((e (assq sym (aref pos 1))))
      (and e (cons (aref pos 0) (cdr e)))))
   ((consp pos)
    (or (elnix--pos-lookup (car pos) sym)
        (elnix--pos-lookup (cdr pos) sym)))))

(defun elnix-update (a b)
  "The set A // B: A's attributes, overridden by B's."
  (let ((na (hash-table-count (aref a 1)))
        (nb (hash-table-count (aref b 1))))
    (cond
     ((= nb 0) a)
     ((= na 0) b)
     ((>= na nb)
      (let ((tbl (copy-hash-table (aref a 1))))
        (maphash (lambda (k v) (puthash k v tbl)) (aref b 1))
        (elnix--mkattrs tbl (elnix--merge-pos (aref b 2) (aref a 2)))))
     (t
      (let ((tbl (copy-hash-table (aref b 1)))
            (btbl (aref b 1)))
        (maphash (lambda (k v)
                   (when (eq (gethash k btbl 'elnix--missing) 'elnix--missing)
                     (puthash k v tbl)))
                 (aref a 1))
        (elnix--mkattrs tbl (elnix--merge-pos (aref b 2) (aref a 2))))))))

(defun elnix--merge-pos (over base)
  (cond ((null over) base)
        ((null base) over)
        (t (cons over base))))

;;;; Selection

(defun elnix--attr-missing (sym a)
  (elnix-fail "attribute '%s' missing" (symbol-name sym))
  (ignore a))

(defun elnix-sel (v sym)
  "The value of attribute SYM of the value V, which must be a set."
  (if (elnix--attrs-p v)
      (let ((x (gethash sym (aref v 1) 'elnix--missing)))
        (if (eq x 'elnix--missing)
            (elnix--attr-missing sym v)
          (if (consp x) (elnix--force-thunk x) x)))
    (elnix-fail "expected a set but found %s while selecting attribute '%s'"
                (elnix-show-type v) (symbol-name sym))))

(defun elnix-sel-lazy (v sym)
  "The lazy value of attribute SYM of the value V, which must be a set."
  (if (elnix--attrs-p v)
      (let ((x (gethash sym (aref v 1) 'elnix--missing)))
        (if (eq x 'elnix--missing) (elnix--attr-missing sym v) x))
    (elnix-fail "expected a set but found %s while selecting attribute '%s'"
                (elnix-show-type v) (symbol-name sym))))

(defun elnix-sel-or (v syms)
  "Select the path SYMS from V, or return `elnix--missing'.
Every step that is missing or not a set gives `elnix--missing'."
  (catch 'elnix--missing
    (dolist (s syms)
      (unless (elnix--attrs-p v) (throw 'elnix--missing 'elnix--missing))
      (let ((x (gethash s (aref v 1) 'elnix--missing)))
        (when (eq x 'elnix--missing) (throw 'elnix--missing 'elnix--missing))
        (setq v (elnix-force x))))
    v))

(defun elnix-has (v syms)
  "Whether the value V has the attribute path SYMS (the `?' operator)."
  (catch 'elnix--done
    (let ((n (length syms)) (i 0))
      (dolist (s syms)
        (setq i (1+ i))
        (unless (elnix--attrs-p v) (throw 'elnix--done nil))
        (let ((x (gethash s (aref v 1) 'elnix--missing)))
          (when (eq x 'elnix--missing) (throw 'elnix--done nil))
          (when (< i n) (setq v (elnix-force x)))))
      t)))

(defun elnix-dyn-sym (v)
  "The attribute name symbol of the dynamic name value V."
  (let ((x (elnix-force v)))
    (unless (elnix-string-p x)
      (elnix-fail "expected a string but found %s while evaluating an attribute name"
                  (elnix-show-type x)))
    (when (elnix--sctx x)
      (elnix-fail "the string '%s' is not allowed to refer to a store path (such as '%s')"
                  (elnix--sval x) (car (elnix--sctx x))))
    (intern (elnix--unibyte (elnix--sval x)))))

(defun elnix-dyn-sym-or-skip (v)
  "The symbol of the dynamic attribute name V in a set literal.
A name that evaluates to null drops its attribute, which this reports
as `elnix--skip'."
  (let ((x (elnix-force v)))
    (if (eq x :null) 'elnix--skip (elnix-dyn-sym x))))

(defun elnix--put-dynamic (tbl sym value)
  "Add the dynamic attribute SYM with VALUE to the table TBL."
  (unless (eq sym 'elnix--skip)
    (unless (eq (gethash sym tbl 'elnix--missing) 'elnix--missing)
      (elnix-fail "dynamic attribute '%s' already defined" (symbol-name sym)))
    (puthash sym value tbl)))

(defun elnix--check-attrs (v)
  "V, which must be a set."
  (if (elnix--attrs-p v) v (elnix--type-error "a set" v)))

(defun elnix--cur-pos (file line column)
  "The value of `__curPos' at LINE and COLUMN of FILE."
  (elnix-attrs-from-alist
   (list (cons 'file file) (cons 'line line) (cons 'column column))))

(defun elnix--assert-fail (file line column)
  (signal 'elnix-assertion
          (list (format "assertion failed at %s:%d:%d" file line column))))

(defun elnix--home-path (text)
  "The path value of the literal TEXT, which starts with `~/'.
The parser of Nix does not canonicalise these, so `~/a/../b' keeps its
dots."
  (elnix--mkpath (concat (or (getenv "HOME") "") (substring text 1))))

(defun elnix--path-interp (dir anchor &rest parts)
  "The path value of a path literal with interpolations.
DIR is the directory of the file, ANCHOR how the literal starts, and
PARTS its text runs and the values of its interpolations."
  (let* ((first (car parts))
         (base (if (stringp first)
                   (pcase anchor
                     ('absolute first)
                     ('home (concat (or (getenv "HOME") "") (substring first 1)))
                     (_ (concat (elnix--path-join dir first)
                                ;; Nix puts back the slash that
                                ;; canonicalisation takes off the first
                                ;; segment.
                                (if (and (> (length first) 1)
                                         (eq (aref first (1- (length first))) ?/))
                                    "/" ""))))
                 ""))
         (strs (list base)))
    (dolist (p (if (stringp first) (cdr parts) parts))
      (if (stringp p)
          (push p strs)
        (let ((s (elnix-coerce-to-string p nil nil)))
          (when (elnix--sctx s)
            (elnix-fail "a string that refers to a store path cannot be appended to a path"))
          (push (elnix--sval s) strs))))
    (elnix--mkpath (elnix-canon-path (apply #'concat (nreverse strs))))))

;;;; With scopes

;; A with scope is (LAZY-SET . OUTER), OUTER the enclosing with scope
;; or nil.

(defun elnix-with-get (scope sym)
  "The value of the variable SYM looked up through the with SCOPE."
  (let ((found 'elnix--missing))
    (while (and scope (eq found 'elnix--missing))
      (let ((a (elnix-force (car scope))))
        (unless (elnix--attrs-p a)
          (elnix-fail "expected a set but found %s in a with expression"
                      (elnix-show-type a)))
        (setq found (gethash sym (aref a 1) 'elnix--missing))
        (setq scope (cdr scope))))
    (if (eq found 'elnix--missing)
        (elnix-fail "undefined variable '%s'" (symbol-name sym))
      (elnix-force found))))

;;;; Functions

;; A lambda with formals is [elnix--lam FN FORMALS ELLIPSIS], FORMALS an
;; alist of (SYMBOL . HAS-DEFAULT) in source order.

(defsubst elnix--mklam (fn formals ellipsis)
  (record 'elnix--lam fn formals ellipsis))

;; A primop is [elnix--primop FN ARITY ARGS NAME]: FN takes ARITY lazy
;; arguments, ARGS holds those supplied so far, newest first.

(defun elnix--app-primop (p arg)
  (let ((args (cons arg (aref p 3))))
    (if (= (length args) (aref p 2))
        (apply (aref p 1) (reverse args))
      (record 'elnix--primop (aref p 1) (aref p 2) args (aref p 4)))))

(defun elnix--app-slow (fv arg)
  (cond
   ((elnix--primop-p fv) (elnix--app-primop fv arg))
   ((elnix--lam-p fv) (funcall (aref fv 1) arg))
   ((elnix--attrs-p fv)
    (let ((functor (gethash '__functor (aref fv 1) 'elnix--missing)))
      (if (eq functor 'elnix--missing)
          (elnix-fail "attempt to call something which is not a function but a set")
        (elnix-app (elnix-app functor fv) arg))))
   (t (elnix-fail "attempt to call something which is not a function but %s"
                  (elnix-show-type fv)))))

(defsubst elnix-app (f arg)
  "Apply the lazy function F to the lazy argument ARG."
  (let ((fv (if (consp f) (elnix--force-thunk f) f)))
    (if (functionp fv) (funcall fv arg) (elnix--app-slow fv arg))))

(defun elnix-app* (f &rest args)
  "Apply the lazy function F to the lazy ARGS in turn."
  (dolist (a args) (setq f (elnix-app f a)))
  f)

(defun elnix-formals-check (arg formals ellipsis)
  "Force ARG to a set and check it against FORMALS; return the set.
FORMALS is an alist of (SYMBOL . HAS-DEFAULT); ELLIPSIS allows other
attributes."
  (let ((a (elnix-force arg)))
    (unless (elnix--attrs-p a)
      (elnix-fail "expected a set but found %s: the function takes a set as its argument"
                  (elnix-show-type a)))
    (let ((tbl (aref a 1)))
      (dolist (f formals)
        (when (and (not (cdr f))
                   (eq (gethash (car f) tbl 'elnix--missing) 'elnix--missing))
          (elnix-fail "function called without required argument '%s'"
                      (symbol-name (car f)))))
      (unless ellipsis
        (when (> (hash-table-count tbl) (length formals))
          (maphash (lambda (k _v)
                     (unless (assq k formals)
                       (elnix-fail "function called with unexpected argument '%s'"
                                   (symbol-name k))))
                   tbl))))
    a))

(defsubst elnix-formal (a sym)
  "The lazy value of formal SYM from the checked argument set A."
  (gethash sym (aref a 1)))

;;;; Arithmetic

(defun elnix-add-num (a b)
  (cond
   ((and (integerp a) (integerp b)) (elnix--check-int (+ a b) "addition"))
   ((and (numberp a) (numberp b)) (+ (float a) (float b)))
   (t (elnix-fail "cannot add %s to %s" (elnix-show-type b) (elnix-show-type a)))))

(defun elnix-sub (a b)
  (cond
   ((and (integerp a) (integerp b)) (elnix--check-int (- a b) "subtraction"))
   ((and (numberp a) (numberp b)) (- (float a) (float b)))
   (t (elnix-fail "cannot subtract %s from %s"
                  (elnix-show-type b) (elnix-show-type a)))))

(defun elnix-mul (a b)
  (cond
   ((and (integerp a) (integerp b)) (elnix--check-int (* a b) "multiplication"))
   ((and (numberp a) (numberp b)) (* (float a) (float b)))
   (t (elnix-fail "cannot multiply %s with %s"
                  (elnix-show-type a) (elnix-show-type b)))))

(defun elnix-div (a b)
  (unless (and (numberp a) (numberp b))
    (elnix-fail "cannot divide %s by %s" (elnix-show-type a) (elnix-show-type b)))
  (when (and (numberp b) (= b 0)) (elnix-fail "division by zero"))
  (if (and (integerp a) (integerp b))
      (elnix--check-int (truncate a b) "division")
    (/ (float a) (float b))))

;;;; Equality and ordering

(defun elnix--is-derivation (a)
  "Whether the set A has type = \"derivation\"."
  (let ((ty (gethash 'type (aref a 1) 'elnix--missing)))
    (and (not (eq ty 'elnix--missing))
         (let ((v (elnix-force ty)))
           (and (elnix-string-p v) (string= (elnix--sval v) "derivation"))))))

(defun elnix-eq-lazy (x y)
  "Whether the lazy values X and Y are equal, Nix's identity shortcut first.
Nix forces both and then treats the same value cell as equal to itself,
which is what makes a list holding a function equal to itself."
  (let ((a (elnix-force x)) (b (elnix-force y)))
    (or (eq x y) (elnix-eq a b))))

(defun elnix-eq (a b)
  "Whether the values A and B are equal under Nix's `=='."
  (cond
   ((integerp a)
    (cond ((integerp b) (= a b))
          ((floatp b) (= a b))
          (t nil)))
   ((floatp a) (and (numberp b) (= a b)))
   ((stringp a)
    (cond ((stringp b) (string= a b))
          ((elnix--cstr-p b) (string= a (aref b 1)))
          (t nil)))
   ((or (eq a t) (null a) (eq a :null)) (eq a b))
   ((vectorp a)
    (and (vectorp b)
         (= (length a) (length b))
         (let ((n (length a)) (i 0) (ok t))
           (while (and ok (< i n))
             (setq ok (elnix-eq-lazy (aref a i) (aref b i)))
             (setq i (1+ i)))
           ok)))
   ((recordp a)
    (pcase (aref a 0)
      ('elnix--cstr (and (elnix-string-p b) (string= (aref a 1) (elnix--sval b))))
      ('elnix--path (and (elnix--path-p b) (string= (aref a 1) (aref b 1))))
      ('elnix--attrs
       (and (elnix--attrs-p b)
            (or (eq a b)
                (catch 'elnix--done
                  (when (and (elnix--is-derivation a) (elnix--is-derivation b))
                    (let ((oa (gethash 'outPath (aref a 1) 'elnix--missing))
                          (ob (gethash 'outPath (aref b 1) 'elnix--missing)))
                      (unless (or (eq oa 'elnix--missing) (eq ob 'elnix--missing))
                        (throw 'elnix--done (elnix-eq-lazy oa ob)))))
                  (unless (= (hash-table-count (aref a 1))
                             (hash-table-count (aref b 1)))
                    (throw 'elnix--done nil))
                  (let ((tb (aref b 1)))
                    (dolist (k (append (elnix-attr-keys a) nil))
                      (let ((y (gethash k tb 'elnix--missing)))
                        (when (or (eq y 'elnix--missing)
                                  (not (elnix-eq-lazy (gethash k (aref a 1)) y)))
                          (throw 'elnix--done nil)))))
                  t))))
      (_ nil)))
   (t nil)))

(defun elnix-less-than (a b)
  "Whether A < B under Nix's ordering."
  (cond
   ((and (numberp a) (numberp b)) (< a b))
   ((and (elnix-string-p a) (elnix-string-p b))
    (string< (elnix--sval a) (elnix--sval b)))
   ((and (elnix--path-p a) (elnix--path-p b))
    (string< (aref a 1) (aref b 1)))
   ((and (vectorp a) (vectorp b))
    (let ((i 0) (na (length a)) (nb (length b)) (result 'unknown))
      (while (eq result 'unknown)
        (cond
         ((= i nb) (setq result nil))
         ((= i na) (setq result t))
         ((not (elnix-eq-lazy (aref a i) (aref b i)))
          (setq result (elnix-less-than (elnix-force (aref a i))
                                        (elnix-force (aref b i)))))
         (t (setq i (1+ i)))))
      result))
   (t (elnix-fail "cannot compare %s with %s"
                  (elnix-show-type a) (elnix-show-type b)))))

;;;; Lists

(defun elnix-concat-lists (a b)
  (unless (vectorp a) (elnix--type-error "a list" a))
  (unless (vectorp b) (elnix--type-error "a list" b))
  (cond ((= (length a) 0) b)
        ((= (length b) 0) a)
        (t (vconcat a b))))

;;;; Coercion to strings

(declare-function elnix-store-copy-path "elnix-store" (path))

(defun elnix-coerce-to-string (v &optional copy more)
  "Coerce the value V to a string value as Nix's coerceToString does.
With COPY, a path is copied to the store and becomes its store path;
otherwise it becomes its own text.  With MORE, integers, floats,
Booleans, null and lists are accepted too, as `toString' and
derivation attributes accept them."
  (cond
   ((stringp v) v)
   ((elnix--cstr-p v) v)
   ((elnix--path-p v)
    (if copy
        (let ((sp (elnix-store-copy-path (aref v 1))))
          (elnix--mkstr sp (list sp)))
      (aref v 1)))
   ((elnix--attrs-p v)
    (let* ((tbl (aref v 1))
           (ts (gethash '__toString tbl 'elnix--missing)))
      (if (not (eq ts 'elnix--missing))
          (elnix-coerce-to-string (elnix-app ts v) copy more)
        (let ((op (gethash 'outPath tbl 'elnix--missing)))
          (if (eq op 'elnix--missing)
              (elnix-fail "cannot coerce a set to a string")
            (elnix-coerce-to-string (elnix-force op) copy more))))))
   ((and more (integerp v)) (number-to-string v))
   ((and more (floatp v)) (elnix-format-float-to-string v))
   ((and more (eq v t)) "1")
   ((and more (or (null v) (eq v :null))) "")
   ((and more (vectorp v))
    (let ((n (length v)) (parts nil) (ctxs nil))
      (dotimes (i n)
        (let* ((ev (elnix-force (aref v i)))
               (s (elnix-coerce-to-string ev copy more)))
          (push (elnix--sval s) parts)
          (push (elnix--sctx s) ctxs)
          (when (and (< i (1- n))
                     (not (and (vectorp ev) (= (length ev) 0))))
            (push " " parts))))
      (elnix--mkstr (apply #'concat (nreverse parts)) (elnix--ctx-union ctxs))))
   (t (elnix-fail "cannot coerce %s to a string" (elnix-show-type v)))))

(defun elnix-format-float-to-string (f)
  "F formatted as std::to_string formats a double, for `toString'."
  (cond
   ((/= f f) (if (< (copysign 1.0 f) 0) "-nan" "nan"))
   ((= f 1.0e+INF) "inf")
   ((= f -1.0e+INF) "-inf")
   (t (format "%f" f))))

(defun elnix-interp (&rest parts)
  "The string interpolation of PARTS, each a string or a value.
Values are coerced as `${...}' coerces them, copying paths to the store."
  (let ((strs nil) (ctxs nil))
    (dolist (p parts)
      (if (stringp p)
          (push p strs)
        (let ((s (elnix-coerce-to-string p t nil)))
          (push (elnix--sval s) strs)
          (when (elnix--cstr-p s) (push (aref s 2) ctxs)))))
    (elnix--mkstr (apply #'concat (nreverse strs)) (elnix--ctx-union ctxs))))

(defun elnix-add (a b)
  "A + B: numbers add, strings and paths concatenate."
  (cond
   ((numberp a) (elnix-add-num a b))
   ((elnix--path-p a)
    (let ((s (cond ((elnix--path-p b) (aref b 1))
                   ((elnix-string-p b)
                    (when (elnix--sctx b)
                      (elnix-fail "a string that refers to a store path cannot be appended to a path"))
                    (elnix--sval b))
                   (t (elnix-coerce-to-string b nil nil)))))
      (elnix--mkpath (elnix-canon-path (concat (aref a 1) (elnix--sval s))))))
   ((elnix-string-p a)
    (let ((sb (elnix-coerce-to-string b t nil)))
      (elnix--mkstr (concat (elnix--sval a) (elnix--sval sb))
                    (elnix--ctx-union (list (elnix--sctx a) (elnix--sctx sb))))))
   ((elnix--attrs-p a)
    ;; A set with outPath or __toString concatenates as a string.  Nix
    ;; copies paths to the store only when the first operand is a
    ;; string, so a path after a set stays its own text.
    (let ((sa (elnix-coerce-to-string a nil nil))
          (sb (elnix-coerce-to-string b nil nil)))
      (elnix--mkstr (concat (elnix--sval sa) (elnix--sval sb))
                    (elnix--ctx-union (list (elnix--sctx sa) (elnix--sctx sb))))))
   (t (elnix-fail "cannot add %s to %s" (elnix-show-type b) (elnix-show-type a)))))

(provide 'elnix-runtime)

;;; elnix-runtime.el ends here
