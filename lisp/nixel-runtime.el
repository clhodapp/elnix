;;; nixel-runtime.el --- Values and operations of compiled Nix -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; How a Nix value lives in Emacs Lisp, and the operations compiled
;; code calls.
;;
;;   int      an integer, kept within the signed 64-bit range
;;   float    a float
;;   bool     t or nil
;;   null     the keyword :null
;;   string   a unibyte string, or a `nixel--cstr' record when the
;;            string carries a context
;;   path     a `nixel--path' record holding the absolute path
;;   list     a vector of lazy values
;;   attrs    a `nixel--attrs' record around an eq hash table from
;;            interned symbols to lazy values
;;   lambda   a closure of one argument, or a `nixel--lam' record for a
;;            lambda with formals; primops are closures too
;;
;; A lazy value is a value or a thunk.  A thunk is a cons cell, which
;; no value is: (CODE . nil) before it is forced, with CODE a closure
;; of no arguments; (nixel--blackhole . CODE) while it is being forced;
;; (nil . VALUE) afterwards.  `nixel-force' turns a lazy value into a
;; value.

;;; Code:

(require 'cl-lib)
(require 'nixel-parse)

(define-error 'nixel-eval-error "Nix evaluation error" 'nixel-error)
(define-error 'nixel-throw "Nix throw" 'nixel-eval-error)
(define-error 'nixel-assertion "Nix assertion failure" 'nixel-eval-error)
(define-error 'nixel-abort "Nix abort" 'nixel-eval-error)
(define-error 'nixel-infinite-recursion "infinite recursion encountered"
              'nixel-eval-error)

(defun nixel-fail (fmt &rest args)
  "Signal a Nix evaluation error with the message FMT and ARGS."
  (signal 'nixel-eval-error (list (apply #'format fmt args))))

;;;; Thunks

(defmacro nixel--delay (&rest body)
  "A thunk whose value is that of BODY."
  `(list (lambda () ,@body)))

(defvar nixel--forcing nil
  "The thunks being forced, innermost first.
A thunk that fails while it is being forced is put back to its
unforced state by the handler that catches the failure; see
`nixel--restore-forcing'.")

(defun nixel--force-thunk (th)
  "Force the thunk TH and return its value."
  (let ((code (car th)))
    (cond
     ((null code) (cdr th))
     ((eq code 'nixel--blackhole)
      (signal 'nixel-infinite-recursion
              (list "infinite recursion encountered")))
     (t
      (setcar th 'nixel--blackhole)
      (setcdr th code)
      (let ((saved nixel--forcing))
        (setq nixel--forcing (cons th saved))
        (let ((v (funcall code)))
          (while (consp v) (setq v (nixel--force-thunk v)))
          (setq nixel--forcing saved)
          (setcar th nil)
          (setcdr th v)
          v))))))

(defsubst nixel-force (v)
  "The value of the lazy value V."
  (if (consp v) (nixel--force-thunk v) v))

(defun nixel--restore-forcing (saved)
  "Return the thunks forced since SAVED to their unforced state.
Called by a handler that caught an error raised while the stack of
thunks in `nixel--forcing' had grown past SAVED."
  (while (and nixel--forcing (not (eq nixel--forcing saved)))
    (let ((th (car nixel--forcing)))
      (when (eq (car th) 'nixel--blackhole)
        (setcar th (cdr th))
        (setcdr th nil)))
    (setq nixel--forcing (cdr nixel--forcing))))

(defmacro nixel--catching (handlers &rest body)
  "Run BODY, restoring interrupted thunks before running a handler.
HANDLERS are `condition-case' handlers that do not bind a variable;
each is (CONDITIONS VAR-SYM FORMS...) where VAR-SYM names the error
data in FORMS."
  (let ((saved (make-symbol "saved")))
    `(let ((,saved nixel--forcing))
       (condition-case nixel--err
           (progn ,@body)
         ,@(mapcar (lambda (h)
                     `(,(car h)
                       (nixel--restore-forcing ,saved)
                       (let ((,(nth 1 h) nixel--err)) ,@(nthcdr 2 h))))
                   handlers)))))

(defun nixel--run (fn)
  "Call FN.  Compiled code uses this to keep a closure out of line."
  (funcall fn))

;;;; Records

(defsubst nixel--tag (v)
  "The record tag of V, or nil when V is not a record."
  (and (recordp v) (aref v 0)))

(defsubst nixel--attrs-p (v) (eq (nixel--tag v) 'nixel--attrs))
(defsubst nixel--cstr-p (v) (eq (nixel--tag v) 'nixel--cstr))
(defsubst nixel--path-p (v) (eq (nixel--tag v) 'nixel--path))
(defsubst nixel--lam-p (v) (eq (nixel--tag v) 'nixel--lam))
(defsubst nixel--primop-p (v) (eq (nixel--tag v) 'nixel--primop))

(defsubst nixel-string-p (v) (or (stringp v) (nixel--cstr-p v)))
(defsubst nixel-function-p (v)
  (or (functionp v) (memq (nixel--tag v) '(nixel--lam nixel--primop))))
(defsubst nixel-bool-p (v) (or (eq v t) (null v)))

;;;; Types

(defun nixel-type-of (v)
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
      ('nixel--attrs "set")
      ('nixel--cstr "string")
      ('nixel--path "path")
      ((or 'nixel--lam 'nixel--primop) "lambda")
      (_ (error "nixel: not a Nix value: %S" v))))
   ((functionp v) "lambda")
   (t (error "nixel: not a Nix value: %S" v))))

(defun nixel-show-type (v)
  "A description of the type of V for error messages, as Nix words it."
  (pcase (nixel-type-of v)
    ("int" "an integer")
    ("float" "a float")
    ("bool" "a Boolean")
    ("null" "null")
    ("string" "a string")
    ("list" "a list")
    ("set" "a set")
    ("path" "a path")
    ("lambda" "a function")))

(defun nixel--type-error (expected v)
  "Signal that V was expected to be EXPECTED."
  (nixel-fail "expected %s but found %s" expected (nixel-show-type v)))

;;;; Integers

(defconst nixel--int-max 9223372036854775807)
(defconst nixel--int-min -9223372036854775808)

(defsubst nixel--check-int (n what)
  "Return N, or signal an overflow in WHAT when N leaves 64 bits."
  (if (or (fixnump n) (and (<= n nixel--int-max) (>= n nixel--int-min)))
      n
    (nixel-fail "integer overflow in %s" what)))

;;;; Booleans and checked forcing

(defsubst nixel--bool (v)
  "V, which must be a Boolean, as t or nil."
  (cond ((eq v t) t)
        ((null v) nil)
        (t (nixel-fail "expected a Boolean but found %s" (nixel-show-type v)))))

(defun nixel-force-bool (v) (nixel--bool (nixel-force v)))

(defun nixel-force-int (v)
  (let ((x (nixel-force v)))
    (if (integerp x) x (nixel--type-error "an integer" x))))

(defun nixel-force-list (v)
  (let ((x (nixel-force v)))
    (if (vectorp x) x (nixel--type-error "a list" x))))

(defun nixel-force-attrs (v)
  (let ((x (nixel-force v)))
    (if (nixel--attrs-p x) x (nixel--type-error "a set" x))))

(defun nixel-force-function (v)
  (let ((x (nixel-force v)))
    (if (nixel-function-p x) x (nixel--type-error "a function" x))))

;;;; Strings and contexts

;; A context is a list of strings in Nix's own spelling: a store path
;; for a plain dependency, `!OUTPUT!DRV' for one output of a derivation,
;; and `=DRV' for a derivation with its whole closure.

(defsubst nixel--mkstr (s ctx)
  "The Nix string of S with context CTX."
  (if ctx (record 'nixel--cstr s ctx) s))

(defsubst nixel--sval (v)
  "The text of the string value V."
  (if (stringp v) v (aref v 1)))

(defsubst nixel--sctx (v)
  "The context of the string value V."
  (if (stringp v) nil (aref v 2)))

(defun nixel--ctx-union (ctxs)
  "The union of the list of contexts CTXS."
  (let ((nonempty (delq nil ctxs)))
    (cond
     ((null nonempty) nil)
     ((null (cdr nonempty)) (car nonempty))
     (t (delete-dups (apply #'append nonempty))))))

(defun nixel-force-string (v)
  "Force V to a string value, keeping its context."
  (let ((x (nixel-force v)))
    (if (nixel-string-p x) x (nixel--type-error "a string" x))))

(defun nixel-force-string-text (v)
  "Force V to a string and return its text."
  (nixel--sval (nixel-force-string v)))

(defun nixel-force-string-no-ctx (v)
  "Force V to a string without context and return its text."
  (let ((x (nixel-force-string v)))
    (when (nixel--sctx x)
      (nixel-fail "the string '%s' is not allowed to refer to a store path (such as '%s')"
                  (nixel--sval x) (car (nixel--sctx x))))
    (nixel--sval x)))

(defsubst nixel--unibyte (s)
  "S as a unibyte string of its UTF-8 bytes."
  (if (multibyte-string-p s)
      (if (string-match-p "\\`[[:ascii:]]*\\'" s)
          (string-to-unibyte s)
        (encode-coding-string s 'utf-8-unix t))
    s))

;;;; Paths

(defsubst nixel--mkpath (abs)
  "The path value of the absolute path string ABS."
  (record 'nixel--path abs))

(defsubst nixel--path-str (p) (aref p 1))

(defun nixel-canon-path (s)
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

(defun nixel--path-join (dir rel)
  "The canonical path of REL relative to the directory DIR."
  (nixel-canon-path (if (string-prefix-p "/" rel) rel (concat dir "/" rel))))

;;;; Attribute sets

;; An attribute set is [nixel--attrs TABLE POS KEYS].  TABLE maps
;; symbols to lazy values.  POS answers `unsafeGetAttrPos': nil, a
;; position table [FILE ALIST] of a literal set, where ALIST maps
;; symbols to (LINE . COLUMN), or (OVER . BASE), the positions of a set
;; made by `//'.  KEYS caches the sorted vector of names.

(defsubst nixel--attrs-table (a) (aref a 1))

(defsubst nixel--mkattrs (table &optional pos)
  (record 'nixel--attrs table pos nil))

(defconst nixel--empty-attrs
  (nixel--mkattrs (make-hash-table :test 'eq :size 0))
  "The empty attribute set.")

(defun nixel-make-attrs (&optional size)
  "A fresh hash table for an attribute set of about SIZE attributes."
  (make-hash-table :test 'eq :size (or size 0)))

(defsubst nixel--attr (a sym)
  "The lazy value of attribute SYM of the set A, or `nixel--missing'."
  (gethash sym (aref a 1) 'nixel--missing))

(defun nixel-attr-count (a) (hash-table-count (aref a 1)))

(defun nixel--sym-less (x y)
  (string< (symbol-name x) (symbol-name y)))

(defun nixel-attr-keys (a)
  "The vector of the attribute names of set A as symbols, sorted bytewise."
  (or (aref a 3)
      (aset a 3 (let ((keys (hash-table-keys (aref a 1))))
                  (vconcat (sort keys #'nixel--sym-less))))))

(defsubst nixel-intern (s)
  "The symbol of the attribute name S."
  (intern (nixel--unibyte s)))

(defun nixel-attrs-from-alist (alist &optional pos)
  "An attribute set of ALIST, a list of (SYMBOL . LAZY)."
  (let ((tbl (nixel-make-attrs (length alist))))
    (dolist (e alist) (puthash (car e) (cdr e) tbl))
    (nixel--mkattrs tbl pos)))

(defun nixel--attrs-pos-lookup (a sym)
  "The (FILE LINE . COLUMN) of attribute SYM of set A, or nil."
  (let ((pos (aref a 2)))
    (nixel--pos-lookup pos sym)))

(defun nixel--pos-lookup (pos sym)
  (cond
   ((null pos) nil)
   ((vectorp pos)
    (let ((e (assq sym (aref pos 1))))
      (and e (cons (aref pos 0) (cdr e)))))
   ((consp pos)
    (or (nixel--pos-lookup (car pos) sym)
        (nixel--pos-lookup (cdr pos) sym)))))

(defun nixel-update (a b)
  "The set A // B: A's attributes, overridden by B's."
  (let ((na (hash-table-count (aref a 1)))
        (nb (hash-table-count (aref b 1))))
    (cond
     ((= nb 0) a)
     ((= na 0) b)
     ((>= na nb)
      (let ((tbl (copy-hash-table (aref a 1))))
        (maphash (lambda (k v) (puthash k v tbl)) (aref b 1))
        (nixel--mkattrs tbl (nixel--merge-pos (aref b 2) (aref a 2)))))
     (t
      (let ((tbl (copy-hash-table (aref b 1)))
            (btbl (aref b 1)))
        (maphash (lambda (k v)
                   (when (eq (gethash k btbl 'nixel--missing) 'nixel--missing)
                     (puthash k v tbl)))
                 (aref a 1))
        (nixel--mkattrs tbl (nixel--merge-pos (aref b 2) (aref a 2))))))))

(defun nixel--merge-pos (over base)
  (cond ((null over) base)
        ((null base) over)
        (t (cons over base))))

;;;; Selection

(defun nixel--attr-missing (sym a)
  (nixel-fail "attribute '%s' missing" (symbol-name sym))
  (ignore a))

(defun nixel-sel (v sym)
  "The value of attribute SYM of the value V, which must be a set."
  (if (nixel--attrs-p v)
      (let ((x (gethash sym (aref v 1) 'nixel--missing)))
        (if (eq x 'nixel--missing)
            (nixel--attr-missing sym v)
          (if (consp x) (nixel--force-thunk x) x)))
    (nixel-fail "expected a set but found %s while selecting attribute '%s'"
                (nixel-show-type v) (symbol-name sym))))

(defun nixel-sel-lazy (v sym)
  "The lazy value of attribute SYM of the value V, which must be a set."
  (if (nixel--attrs-p v)
      (let ((x (gethash sym (aref v 1) 'nixel--missing)))
        (if (eq x 'nixel--missing) (nixel--attr-missing sym v) x))
    (nixel-fail "expected a set but found %s while selecting attribute '%s'"
                (nixel-show-type v) (symbol-name sym))))

(defun nixel-sel-or (v syms)
  "Select the path SYMS from V, or return `nixel--missing'.
Every step that is missing or not a set gives `nixel--missing'."
  (catch 'nixel--missing
    (dolist (s syms)
      (unless (nixel--attrs-p v) (throw 'nixel--missing 'nixel--missing))
      (let ((x (gethash s (aref v 1) 'nixel--missing)))
        (when (eq x 'nixel--missing) (throw 'nixel--missing 'nixel--missing))
        (setq v (nixel-force x))))
    v))

(defun nixel-has (v syms)
  "Whether the value V has the attribute path SYMS (the `?' operator)."
  (catch 'nixel--done
    (let ((n (length syms)) (i 0))
      (dolist (s syms)
        (setq i (1+ i))
        (unless (nixel--attrs-p v) (throw 'nixel--done nil))
        (let ((x (gethash s (aref v 1) 'nixel--missing)))
          (when (eq x 'nixel--missing) (throw 'nixel--done nil))
          (when (< i n) (setq v (nixel-force x)))))
      t)))

(defun nixel-dyn-sym (v)
  "The attribute name symbol of the dynamic name value V."
  (let ((x (nixel-force v)))
    (unless (nixel-string-p x)
      (nixel-fail "expected a string but found %s while evaluating an attribute name"
                  (nixel-show-type x)))
    (intern (nixel--unibyte (nixel--sval x)))))

(defun nixel-dyn-sym-or-skip (v)
  "The symbol of the dynamic attribute name V in a set literal.
A name that evaluates to null drops its attribute, which this reports
as `nixel--skip'."
  (let ((x (nixel-force v)))
    (if (eq x :null) 'nixel--skip (nixel-dyn-sym x))))

(defun nixel--put-dynamic (tbl sym value)
  "Add the dynamic attribute SYM with VALUE to the table TBL."
  (unless (eq sym 'nixel--skip)
    (unless (eq (gethash sym tbl 'nixel--missing) 'nixel--missing)
      (nixel-fail "dynamic attribute '%s' already defined" (symbol-name sym)))
    (puthash sym value tbl)))

(defun nixel--check-attrs (v)
  "V, which must be a set."
  (if (nixel--attrs-p v) v (nixel--type-error "a set" v)))

(defun nixel--cur-pos (file line column)
  "The value of `__curPos' at LINE and COLUMN of FILE."
  (nixel-attrs-from-alist
   (list (cons 'file file) (cons 'line line) (cons 'column column))))

(defun nixel--assert-fail (file line column)
  (signal 'nixel-assertion
          (list (format "assertion failed at %s:%d:%d" file line column))))

(defun nixel--home-path (text)
  "The path value of the literal TEXT, which starts with `~/'.
The parser of Nix does not canonicalise these, so `~/a/../b' keeps its
dots."
  (nixel--mkpath (concat (or (getenv "HOME") "") (substring text 1))))

(defun nixel--path-interp (dir anchor &rest parts)
  "The path value of a path literal with interpolations.
DIR is the directory of the file, ANCHOR how the literal starts, and
PARTS its text runs and the values of its interpolations."
  (let* ((first (car parts))
         (base (if (stringp first)
                   (pcase anchor
                     ('absolute first)
                     ('home (concat (or (getenv "HOME") "") (substring first 1)))
                     (_ (concat (nixel--path-join dir first)
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
        (let ((s (nixel-coerce-to-string p nil nil)))
          (when (nixel--sctx s)
            (nixel-fail "a string that refers to a store path cannot be appended to a path"))
          (push (nixel--sval s) strs))))
    (nixel--mkpath (nixel-canon-path (apply #'concat (nreverse strs))))))

;;;; With scopes

;; A with scope is (LAZY-SET . OUTER), OUTER the enclosing with scope
;; or nil.

(defun nixel-with-get (scope sym)
  "The value of the variable SYM looked up through the with SCOPE."
  (let ((found 'nixel--missing))
    (while (and scope (eq found 'nixel--missing))
      (let ((a (nixel-force (car scope))))
        (unless (nixel--attrs-p a)
          (nixel-fail "expected a set but found %s in a with expression"
                      (nixel-show-type a)))
        (setq found (gethash sym (aref a 1) 'nixel--missing))
        (setq scope (cdr scope))))
    (if (eq found 'nixel--missing)
        (nixel-fail "undefined variable '%s'" (symbol-name sym))
      (nixel-force found))))

;;;; Functions

;; A lambda with formals is [nixel--lam FN FORMALS ELLIPSIS], FORMALS an
;; alist of (SYMBOL . HAS-DEFAULT) in source order.

(defsubst nixel--mklam (fn formals ellipsis)
  (record 'nixel--lam fn formals ellipsis))

;; A primop is [nixel--primop FN ARITY ARGS NAME]: FN takes ARITY lazy
;; arguments, ARGS holds those supplied so far, newest first.

(defun nixel--app-primop (p arg)
  (let ((args (cons arg (aref p 3))))
    (if (= (length args) (aref p 2))
        (apply (aref p 1) (reverse args))
      (record 'nixel--primop (aref p 1) (aref p 2) args (aref p 4)))))

(defun nixel--app-slow (fv arg)
  (cond
   ((nixel--primop-p fv) (nixel--app-primop fv arg))
   ((nixel--lam-p fv) (funcall (aref fv 1) arg))
   ((nixel--attrs-p fv)
    (let ((functor (gethash '__functor (aref fv 1) 'nixel--missing)))
      (if (eq functor 'nixel--missing)
          (nixel-fail "attempt to call something which is not a function but a set")
        (nixel-app (nixel-app functor fv) arg))))
   (t (nixel-fail "attempt to call something which is not a function but %s"
                  (nixel-show-type fv)))))

(defsubst nixel-app (f arg)
  "Apply the lazy function F to the lazy argument ARG."
  (let ((fv (if (consp f) (nixel--force-thunk f) f)))
    (if (functionp fv) (funcall fv arg) (nixel--app-slow fv arg))))

(defun nixel-app* (f &rest args)
  "Apply the lazy function F to the lazy ARGS in turn."
  (dolist (a args) (setq f (nixel-app f a)))
  f)

(defun nixel-formals-check (arg formals ellipsis)
  "Force ARG to a set and check it against FORMALS; return the set.
FORMALS is an alist of (SYMBOL . HAS-DEFAULT); ELLIPSIS allows other
attributes."
  (let ((a (nixel-force arg)))
    (unless (nixel--attrs-p a)
      (nixel-fail "expected a set but found %s: the function takes a set as its argument"
                  (nixel-show-type a)))
    (let ((tbl (aref a 1)))
      (dolist (f formals)
        (when (and (not (cdr f))
                   (eq (gethash (car f) tbl 'nixel--missing) 'nixel--missing))
          (nixel-fail "function called without required argument '%s'"
                      (symbol-name (car f)))))
      (unless ellipsis
        (when (> (hash-table-count tbl) (length formals))
          (maphash (lambda (k _v)
                     (unless (assq k formals)
                       (nixel-fail "function called with unexpected argument '%s'"
                                   (symbol-name k))))
                   tbl))))
    a))

(defsubst nixel-formal (a sym)
  "The lazy value of formal SYM from the checked argument set A."
  (gethash sym (aref a 1)))

;;;; Arithmetic

(defun nixel-add-num (a b)
  (cond
   ((and (integerp a) (integerp b)) (nixel--check-int (+ a b) "addition"))
   ((and (numberp a) (numberp b)) (+ (float a) (float b)))
   (t (nixel-fail "cannot add %s to %s" (nixel-show-type b) (nixel-show-type a)))))

(defun nixel-sub (a b)
  (cond
   ((and (integerp a) (integerp b)) (nixel--check-int (- a b) "subtraction"))
   ((and (numberp a) (numberp b)) (- (float a) (float b)))
   (t (nixel-fail "cannot subtract %s from %s"
                  (nixel-show-type b) (nixel-show-type a)))))

(defun nixel-mul (a b)
  (cond
   ((and (integerp a) (integerp b)) (nixel--check-int (* a b) "multiplication"))
   ((and (numberp a) (numberp b)) (* (float a) (float b)))
   (t (nixel-fail "cannot multiply %s with %s"
                  (nixel-show-type a) (nixel-show-type b)))))

(defun nixel-div (a b)
  (unless (and (numberp a) (numberp b))
    (nixel-fail "cannot divide %s by %s" (nixel-show-type a) (nixel-show-type b)))
  (when (and (numberp b) (= b 0)) (nixel-fail "division by zero"))
  (if (and (integerp a) (integerp b))
      (nixel--check-int (truncate a b) "division")
    (/ (float a) (float b))))

;;;; Equality and ordering

(defun nixel--is-derivation (a)
  "Whether the set A has type = \"derivation\"."
  (let ((ty (gethash 'type (aref a 1) 'nixel--missing)))
    (and (not (eq ty 'nixel--missing))
         (let ((v (nixel-force ty)))
           (and (nixel-string-p v) (string= (nixel--sval v) "derivation"))))))

(defun nixel-eq-lazy (x y)
  "Whether the lazy values X and Y are equal, Nix's identity shortcut first.
Nix forces both and then treats the same value cell as equal to itself,
which is what makes a list holding a function equal to itself."
  (let ((a (nixel-force x)) (b (nixel-force y)))
    (or (eq x y) (nixel-eq a b))))

(defun nixel-eq (a b)
  "Whether the values A and B are equal under Nix's `=='."
  (cond
   ((integerp a)
    (cond ((integerp b) (= a b))
          ((floatp b) (= a b))
          (t nil)))
   ((floatp a) (and (numberp b) (= a b)))
   ((stringp a)
    (cond ((stringp b) (string= a b))
          ((nixel--cstr-p b) (string= a (aref b 1)))
          (t nil)))
   ((or (eq a t) (null a) (eq a :null)) (eq a b))
   ((vectorp a)
    (and (vectorp b)
         (= (length a) (length b))
         (let ((n (length a)) (i 0) (ok t))
           (while (and ok (< i n))
             (setq ok (nixel-eq-lazy (aref a i) (aref b i)))
             (setq i (1+ i)))
           ok)))
   ((recordp a)
    (pcase (aref a 0)
      ('nixel--cstr (and (nixel-string-p b) (string= (aref a 1) (nixel--sval b))))
      ('nixel--path (and (nixel--path-p b) (string= (aref a 1) (aref b 1))))
      ('nixel--attrs
       (and (nixel--attrs-p b)
            (or (eq a b)
                (catch 'nixel--done
                  (when (and (nixel--is-derivation a) (nixel--is-derivation b))
                    (let ((oa (gethash 'outPath (aref a 1) 'nixel--missing))
                          (ob (gethash 'outPath (aref b 1) 'nixel--missing)))
                      (unless (or (eq oa 'nixel--missing) (eq ob 'nixel--missing))
                        (throw 'nixel--done (nixel-eq-lazy oa ob)))))
                  (unless (= (hash-table-count (aref a 1))
                             (hash-table-count (aref b 1)))
                    (throw 'nixel--done nil))
                  (let ((tb (aref b 1)))
                    (dolist (k (append (nixel-attr-keys a) nil))
                      (let ((y (gethash k tb 'nixel--missing)))
                        (when (or (eq y 'nixel--missing)
                                  (not (nixel-eq-lazy (gethash k (aref a 1)) y)))
                          (throw 'nixel--done nil)))))
                  t))))
      (_ nil)))
   (t nil)))

(defun nixel-less-than (a b)
  "Whether A < B under Nix's ordering."
  (cond
   ((and (numberp a) (numberp b)) (< a b))
   ((and (nixel-string-p a) (nixel-string-p b))
    (string< (nixel--sval a) (nixel--sval b)))
   ((and (nixel--path-p a) (nixel--path-p b))
    (string< (aref a 1) (aref b 1)))
   ((and (vectorp a) (vectorp b))
    (let ((i 0) (na (length a)) (nb (length b)) (result 'unknown))
      (while (eq result 'unknown)
        (cond
         ((= i nb) (setq result nil))
         ((= i na) (setq result t))
         ((not (nixel-eq-lazy (aref a i) (aref b i)))
          (setq result (nixel-less-than (nixel-force (aref a i))
                                        (nixel-force (aref b i)))))
         (t (setq i (1+ i)))))
      result))
   (t (nixel-fail "cannot compare %s with %s"
                  (nixel-show-type a) (nixel-show-type b)))))

;;;; Lists

(defun nixel-concat-lists (a b)
  (unless (vectorp a) (nixel--type-error "a list" a))
  (unless (vectorp b) (nixel--type-error "a list" b))
  (cond ((= (length a) 0) b)
        ((= (length b) 0) a)
        (t (vconcat a b))))

;;;; Coercion to strings

(declare-function nixel-store-copy-path "nixel-store" (path))

(defun nixel-coerce-to-string (v &optional copy more)
  "Coerce the value V to a string value as Nix's coerceToString does.
With COPY, a path is copied to the store and becomes its store path;
otherwise it becomes its own text.  With MORE, integers, floats,
Booleans, null and lists are accepted too, as `toString' and
derivation attributes accept them."
  (cond
   ((stringp v) v)
   ((nixel--cstr-p v) v)
   ((nixel--path-p v)
    (if copy
        (let ((sp (nixel-store-copy-path (aref v 1))))
          (nixel--mkstr sp (list sp)))
      (aref v 1)))
   ((nixel--attrs-p v)
    (let* ((tbl (aref v 1))
           (ts (gethash '__toString tbl 'nixel--missing)))
      (if (not (eq ts 'nixel--missing))
          (nixel-coerce-to-string (nixel-app ts v) copy more)
        (let ((op (gethash 'outPath tbl 'nixel--missing)))
          (if (eq op 'nixel--missing)
              (nixel-fail "cannot coerce a set to a string")
            (nixel-coerce-to-string (nixel-force op) copy more))))))
   ((and more (integerp v)) (number-to-string v))
   ((and more (floatp v)) (nixel-format-float-to-string v))
   ((and more (eq v t)) "1")
   ((and more (or (null v) (eq v :null))) "")
   ((and more (vectorp v))
    (let ((n (length v)) (parts nil) (ctxs nil))
      (dotimes (i n)
        (let* ((ev (nixel-force (aref v i)))
               (s (nixel-coerce-to-string ev copy more)))
          (push (nixel--sval s) parts)
          (push (nixel--sctx s) ctxs)
          (when (and (< i (1- n))
                     (not (and (vectorp ev) (= (length ev) 0))))
            (push " " parts))))
      (nixel--mkstr (apply #'concat (nreverse parts)) (nixel--ctx-union ctxs))))
   (t (nixel-fail "cannot coerce %s to a string" (nixel-show-type v)))))

(defun nixel-format-float-to-string (f)
  "F formatted as std::to_string formats a double, for `toString'."
  (cond
   ((/= f f) (if (< (copysign 1.0 f) 0) "-nan" "nan"))
   ((= f 1.0e+INF) "inf")
   ((= f -1.0e+INF) "-inf")
   (t (format "%f" f))))

(defun nixel-interp (&rest parts)
  "The string interpolation of PARTS, each a string or a value.
Values are coerced as `${...}' coerces them, copying paths to the store."
  (let ((strs nil) (ctxs nil))
    (dolist (p parts)
      (if (stringp p)
          (push p strs)
        (let ((s (nixel-coerce-to-string p t nil)))
          (push (nixel--sval s) strs)
          (when (nixel--cstr-p s) (push (aref s 2) ctxs)))))
    (nixel--mkstr (apply #'concat (nreverse strs)) (nixel--ctx-union ctxs))))

(defun nixel-add (a b)
  "A + B: numbers add, strings and paths concatenate."
  (cond
   ((numberp a) (nixel-add-num a b))
   ((nixel--path-p a)
    (let ((s (cond ((nixel--path-p b) (aref b 1))
                   ((nixel-string-p b)
                    (when (nixel--sctx b)
                      (nixel-fail "a string that refers to a store path cannot be appended to a path"))
                    (nixel--sval b))
                   (t (nixel-coerce-to-string b nil nil)))))
      (nixel--mkpath (nixel-canon-path (concat (aref a 1) (nixel--sval s))))))
   ((nixel-string-p a)
    (let ((sb (nixel-coerce-to-string b t nil)))
      (nixel--mkstr (concat (nixel--sval a) (nixel--sval sb))
                    (nixel--ctx-union (list (nixel--sctx a) (nixel--sctx sb))))))
   ((nixel--attrs-p a)
    ;; A set with outPath or __toString concatenates as a string.  Nix
    ;; copies paths to the store only when the first operand is a
    ;; string, so a path after a set stays its own text.
    (let ((sa (nixel-coerce-to-string a nil nil))
          (sb (nixel-coerce-to-string b nil nil)))
      (nixel--mkstr (concat (nixel--sval sa) (nixel--sval sb))
                    (nixel--ctx-union (list (nixel--sctx sa) (nixel--sctx sb))))))
   (t (nixel-fail "cannot add %s to %s" (nixel-show-type b) (nixel-show-type a)))))

(provide 'nixel-runtime)

;;; nixel-runtime.el ends here
