;;; elnix-compile.el --- Compile Nix syntax trees to Emacs Lisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The compiler turns the syntax tree of one Nix file into one Emacs
;; Lisp form, a function of the file's directory and name that returns
;; the file's value.
;;
;; Every subexpression is compiled in one of two modes.  A strict form
;; produces a value; a lazy form produces a value or a thunk.  Nix
;; allocates a thunk for every subexpression in a lazy position except
;; literals, variables and the empty list, and so does the compiler,
;; with lambdas added to the exceptions.  Lazy positions are attribute
;; values, list elements, function arguments, let bindings, the
;; defaults of formals and the set of a `with'.
;;
;; Nix variables become lexical variables named `$NAME'.  Names the
;; compiler introduces start with `%', which no Nix identifier can.
;; Scoping is resolved here, as Nix resolves it at parse time: a
;; lexical binding anywhere outward wins over a `with', a name free
;; under a `with' is looked up at run time, and a name free under no
;; `with' must be a builtin or the compilation fails.
;;
;; The compile-time environment is a list of frames, innermost first.
;; A frame is [vars ALIST USED] for bindings, ALIST mapping names to
;; variable symbols and USED set when a lookup resolves through the
;; frame, or [with SYMBOL] for a `with', SYMBOL the variable holding
;; its run-time scope.  USED is how the compiler tells a `let' whose
;; values refer to its own names, which needs recursive binding, from
;; one that does not.

;;; Code:

(require 'cl-lib)
(require 'elnix-parse)
(require 'elnix-runtime)

(defvar elnix-compile--line-starts nil
  "The line table of the file being compiled.")

(defvar elnix-compile--hoisted nil
  "Module-level bindings, newest first: (SYMBOL FORM).
Path constants and position tables are computed once per module load
and shared by every evaluation of the expression that names them.")

(defvar elnix-compile--counter 0
  "The counter that makes the names of compiler temporaries unique.")

(defvar elnix-compile--file nil
  "The name of the source file being compiled, for error messages.")

(defvar elnix-compile-builtin-arity nil
  "A function from a builtin name to its arity, or nil.
Set by the builtins library; nil arity means the builtin is not called
directly.")

(defvar elnix-compile-global-p nil
  "A function returning non-nil when a name is a global builtin.")

(defun elnix-compile--gensym (prefix)
  "A fresh symbol named %PREFIXN."
  (setq elnix-compile--counter (1+ elnix-compile--counter))
  (intern (format "%%%s%d" prefix elnix-compile--counter)))

(defun elnix-compile--var-sym (name)
  "The Lisp variable of the Nix variable NAME."
  (intern (concat "$" name)))

(defun elnix-compile--line-col (pos)
  (elnix-parse-line-col pos elnix-compile--line-starts))

(defun elnix-compile--fail (pos fmt &rest args)
  (let ((lc (elnix-compile--line-col pos)))
    (signal 'elnix-parse-error
            (list (format "%s at %s:%d:%d" (apply #'format fmt args)
                          (or elnix-compile--file "«string»")
                          (car lc) (cdr lc))))))

(defun elnix-compile--str-const (s)
  "A form evaluating to the unibyte string S.
A module file is read back as raw bytes, which turns a string literal
holding bytes outside ASCII into a multibyte string of raw-byte
characters, so such a literal is made unibyte again when the module
loads."
  (if (string-match-p "\\`[[:ascii:]]*\\'" s)
      s
    `(string-to-unibyte ,s)))

(defun elnix-compile--sym-form (name)
  "A form evaluating to the attribute symbol of the string NAME."
  (let ((s (elnix--unibyte name)))
    (if (string-match-p "\\`[[:ascii:]]*\\'" s)
        (list 'quote (intern s))
      ;; A symbol with bytes outside ASCII cannot round-trip through a
      ;; source file, so it is interned when the module loads.
      `(intern (string-to-unibyte ,s)))))

(defun elnix-compile--hoist (prefix form)
  "Bind FORM at module level and return the variable that holds it."
  (let ((sym (elnix-compile--gensym prefix)))
    (push (list sym form) elnix-compile--hoisted)
    sym))

;;;; Environments

(defun elnix-compile--lookup (name env)
  "Resolve the Nix variable NAME in ENV.
Return (lex SYMBOL), (with SYMBOL) or (global NAME)."
  (let ((found nil) (with nil))
    (dolist (fr env)
      (pcase (aref fr 0)
        ('vars (unless found
                 (let ((e (assoc name (aref fr 1))))
                   (when e
                     (aset fr 2 t)
                     (setq found (cdr e))))))
        ('with (unless with (setq with (aref fr 1))))))
    ;; The globals are lexical bindings of the outermost scope, so they
    ;; too win over a `with'.
    (cond
     (found (list 'lex found))
     ((and elnix-compile-global-p (funcall elnix-compile-global-p name))
      (list 'global name))
     (with (list 'with with))
     (t nil))))

(defun elnix-compile--innermost-with (env)
  (cl-loop for fr in env
           when (eq (aref fr 0) 'with) return (aref fr 1)))

;;;; Expressions

(defun elnix-compile--trivial-p (e env)
  "Whether E compiles to the same form in lazy and strict-free use.
Such an expression needs no thunk in a lazy position."
  (pcase (car e)
    ((or 'int 'float) t)
    ('str (cl-every #'stringp (nth 2 e)))
    ('lambda t)
    ('list (null (nth 2 e)))
    ('var (eq (car (elnix-compile--lookup (nth 2 e) env)) 'lex))
    (_ nil)))

(defun elnix-compile-lazy (e env)
  "A form producing the lazy value of E in ENV."
  (pcase (car e)
    ((or 'int 'float 'lambda) (elnix-compile-strict e env))
    ('str (if (cl-every #'stringp (nth 2 e))
              (elnix-compile-strict e env)
            `(elnix--delay ,(elnix-compile-strict e env))))
    ('list (if (null (nth 2 e))
               []
             `(elnix--delay ,(elnix-compile-strict e env))))
    ('var
     (let ((r (elnix-compile--lookup (nth 2 e) env)))
       (pcase (car r)
         ('lex (nth 1 r))
         ('global (if (string= (nth 2 e) "__curPos")
                      (elnix-compile-strict e env)
                    (elnix-compile--global-value (nth 2 e))))
         (_ `(elnix--delay ,(elnix-compile-strict e env))))))
    (_ `(elnix--delay ,(elnix-compile-strict e env)))))

(defun elnix-compile--global-value (name)
  "A form evaluating to the value of the global NAME."
  (pcase name
    ("true" t)
    ("false" nil)
    ("null" :null)
    (_ `(elnix-builtin ,(elnix-compile--sym-form name)))))

(defun elnix-compile-strict (e env)
  "A form producing the value of E in ENV."
  (pcase e
    (`(int ,_ ,n) n)
    (`(float ,_ ,f) f)
    (`(str ,_ ,parts) (elnix-compile--string parts env))
    (`(path ,pos ,anchor ,parts) (elnix-compile--path pos anchor parts env))
    (`(var ,pos ,name) (elnix-compile--var pos name env))
    (`(select ,_ ,subject ,path ,default)
     (elnix-compile--select subject path default env))
    (`(has ,_ ,subject ,path)
     `(elnix-has ,(elnix-compile-strict subject env)
                 ,(elnix-compile--attrpath-form path env)))
    (`(lambda ,_ ,param ,formals ,body)
     (elnix-compile--lambda param formals body env))
    (`(call . ,_) (elnix-compile--call e env))
    (`(list ,_ ,elems)
     (if (null elems)
         []
       `(vector ,@(mapcar (lambda (x) (elnix-compile-lazy x env)) elems))))
    (`(attrs ,pos ,rec ,bindings)
     (if rec
         (elnix-compile--rec-attrs pos bindings env)
       (elnix-compile--attrs pos bindings env)))
    (`(let ,_ ,bindings ,body) (elnix-compile--let bindings body env))
    (`(with ,_ ,scope ,body)
     (let ((w (elnix-compile--gensym "w")))
       `(let ((,w (cons ,(elnix-compile-lazy scope env)
                        ,(elnix-compile--innermost-with env))))
          ,(elnix-compile-strict body (cons (vector 'with w) env)))))
    (`(assert ,pos ,c ,body)
     (let ((lc (elnix-compile--line-col pos)))
       `(if (elnix--bool ,(elnix-compile-strict c env))
            ,(elnix-compile-strict body env)
          (elnix--assert-fail %file ,(car lc) ,(cdr lc)))))
    (`(if ,_ ,c ,then ,else)
     `(if (elnix--bool ,(elnix-compile-strict c env))
          ,(elnix-compile-strict then env)
        ,(elnix-compile-strict else env)))
    (`(not ,_ ,x) `(not (elnix--bool ,(elnix-compile-strict x env))))
    (`(neg ,_ ,x) `(elnix-sub 0 ,(elnix-compile-strict x env)))
    (`(binop ,_ ,op ,l ,r) (elnix-compile--binop op l r env))
    (_ (error "elnix: unknown syntax node %S" (car e)))))

(defun elnix-compile--var (pos name env)
  (let ((r (elnix-compile--lookup name env)))
    (pcase (car r)
      ('lex `(elnix-force ,(nth 1 r)))
      ('with `(elnix-with-get ,(nth 1 r) ,(elnix-compile--sym-form name)))
      ('global
       (if (string= name "__curPos")
           (let ((lc (elnix-compile--line-col pos)))
             `(elnix--cur-pos %file ,(car lc) ,(cdr lc)))
         (elnix-compile--global-value name)))
      (_ (elnix-compile--fail pos "undefined variable '%s'" name)))))

(defun elnix-compile--string (parts env)
  (cond
   ((null parts) "")
   ((cl-every #'stringp parts) (elnix-compile--str-const (apply #'concat parts)))
   (t `(elnix-interp ,@(mapcar (lambda (p)
                                 (if (stringp p) (elnix-compile--str-const p)
                                   (elnix-compile-strict p env)))
                               parts)))))

(defun elnix-compile--path (_pos anchor parts env)
  (pcase anchor
    ('search
     `(elnix--find-file ,(car parts)))
    (_
     (if (cl-every #'stringp parts)
         (let ((text (apply #'concat parts)))
           (pcase anchor
             ('absolute
              (elnix-compile--hoist
               "p" `(elnix--mkpath ,(elnix-canon-path text))))
             ('home `(elnix--home-path ,text))
             (_ (elnix-compile--hoist
                 "p" `(elnix--mkpath (elnix--path-join %dir ,text))))))
       `(elnix--path-interp %dir ',anchor
                            ,@(mapcar (lambda (p)
                                        (if (stringp p) p
                                          (elnix-compile-strict p env)))
                                      parts))))))

(defun elnix-compile--attrname-form (name env)
  "A form evaluating to the symbol of the attribute NAME."
  (pcase name
    (`(static ,_ ,s) (elnix-compile--sym-form s))
    (`(dynamic ,_ ,e) `(elnix-dyn-sym ,(elnix-compile-strict e env)))))

(defun elnix-compile--attrpath-form (path env)
  "A form evaluating to the list of symbols of the attribute PATH."
  (if (cl-every (lambda (n) (eq (car n) 'static)) path)
      (list 'quote (mapcar (lambda (n) (intern (elnix--unibyte (nth 2 n))))
                           path))
    `(list ,@(mapcar (lambda (n) (elnix-compile--attrname-form n env)) path))))

(defun elnix-compile--select (subject path default env)
  (if default
      (let ((v (elnix-compile--gensym "v")))
        `(let ((,v (elnix-sel-or ,(elnix-compile-strict subject env)
                                 ,(elnix-compile--attrpath-form path env))))
           (if (eq ,v 'elnix--missing)
               ,(elnix-compile-strict default env)
             ,v)))
    (let ((form (elnix-compile-strict subject env)))
      (dolist (n path)
        (setq form `(elnix-sel ,form ,(elnix-compile--attrname-form n env))))
      form)))

;;;; Functions

(defun elnix-compile--lambda (param formals body env)
  (if (null formals)
      (let ((sym (elnix-compile--var-sym param)))
        `(lambda (,sym)
           ,(elnix-compile-strict
             body (cons (vector 'vars (list (cons param sym)) nil) env))))
    (elnix-compile--formals-lambda param formals body env)))

(defun elnix-compile--formals-lambda (param formals body env)
  (let* ((entries (car formals))
         (ellipsis (cdr formals))
         (arg (elnix-compile--gensym "arg"))
         (set (elnix-compile--gensym "set"))
         (alist (append
                 (mapcar (lambda (f) (cons (car f) (elnix-compile--var-sym (car f))))
                         entries)
                 (and param (list (cons param (elnix-compile--var-sym param))))))
         (frame (vector 'vars alist nil))
         (inner (cons frame env))
         (spec (mapcar (lambda (f)
                         (cons (intern (elnix--unibyte (car f)))
                               (and (nth 2 f) t)))
                       entries))
         ;; Each formal's form, compiled in the function's own scope so
         ;; that a default can name the other formals.
         (inits (mapcar
                 (lambda (f)
                   (let ((sym-form (elnix-compile--sym-form (car f))))
                     (list (elnix-compile--var-sym (car f))
                           (if (nth 2 f)
                               (let ((x (elnix-compile--gensym "x"))
                                     (default (elnix-compile-lazy (nth 2 f) inner)))
                                 ;; A default that is another formal
                                 ;; would read it before it is assigned.
                                 (when (and (symbolp default) (rassq default alist))
                                   (setq default `(elnix--delay (elnix-force ,default))))
                                 `(let ((,x (gethash ,sym-form (aref ,set 1)
                                                     'elnix--missing)))
                                    (if (eq ,x 'elnix--missing)
                                        ,default
                                      ,x)))
                             `(gethash ,sym-form (aref ,set 1))))))
                 entries))
         (recursive (aref frame 2))
         (param-binding (and param (list (list (elnix-compile--var-sym param) arg))))
         (body-form (elnix-compile-strict body inner)))
    `(elnix--mklam
      (lambda (,arg)
        (let ((,set (elnix-formals-check ,arg ',spec ,ellipsis)))
          ,(if recursive
               `(let ,(append (mapcar #'car param-binding) (mapcar #'car inits))
                  ,@(mapcar (lambda (b) `(setq ,@b)) param-binding)
                  ,@(mapcar (lambda (b) `(setq ,@b)) inits)
                  ,body-form)
             `(let (,@param-binding ,@inits)
                ,body-form))))
      ',spec ,ellipsis)))

(defun elnix-compile--builtin-callee (fn env)
  "The builtin name FN refers to, when FN is a reference to a builtin."
  (pcase fn
    (`(var ,_ ,name)
     (and (eq (car (elnix-compile--lookup name env)) 'global)
          (if (string-prefix-p "__" name) (substring name 2) name)))
    (`(select ,_ (var ,_ "builtins") ((static ,_ ,name)) nil)
     (and (eq (car (elnix-compile--lookup "builtins" env)) 'global)
          name))))

(defun elnix-compile--call (e env)
  ;; Flatten the curried application f a b c.
  (let ((args nil) (fn e))
    (while (eq (car fn) 'call)
      (push (nth 3 fn) args)
      (setq fn (nth 2 fn)))
    (let* ((bname (elnix-compile--builtin-callee fn env))
           (arity (and bname elnix-compile-builtin-arity
                       (funcall elnix-compile-builtin-arity bname))))
      (if (and arity (> arity 0) (>= (length args) arity))
          (let ((form `(,(intern (concat "elnix-b--" bname))
                        ,@(mapcar (lambda (a) (elnix-compile-lazy a env))
                                  (cl-subseq args 0 arity)))))
            (dolist (a (nthcdr arity args))
              (setq form `(elnix-app ,form ,(elnix-compile-lazy a env))))
            form)
        (let ((form (elnix-compile-strict fn env)))
          (dolist (a args)
            (setq form `(elnix-app ,form ,(elnix-compile-lazy a env))))
          form)))))

;;;; Operators

(defun elnix-compile--binop (op l r env)
  (let ((sl (lambda () (elnix-compile-strict l env)))
        (sr (lambda () (elnix-compile-strict r env))))
    (pcase op
      ("+" `(elnix-add ,(funcall sl) ,(funcall sr)))
      ("-" `(elnix-sub ,(funcall sl) ,(funcall sr)))
      ("*" `(elnix-mul ,(funcall sl) ,(funcall sr)))
      ("/" `(elnix-div ,(funcall sl) ,(funcall sr)))
      ("==" `(elnix-eq ,(funcall sl) ,(funcall sr)))
      ("!=" `(not (elnix-eq ,(funcall sl) ,(funcall sr))))
      ;; Nix rewrites the comparisons to __lessThan, which fixes the
      ;; order the operands are evaluated in.
      ("<" `(elnix-less-than ,(funcall sl) ,(funcall sr)))
      (">" `(elnix-less-than ,(funcall sr) ,(funcall sl)))
      ("<=" `(not (elnix-less-than ,(funcall sr) ,(funcall sl))))
      (">=" `(not (elnix-less-than ,(funcall sl) ,(funcall sr))))
      ("&&" `(and (elnix--bool ,(funcall sl)) (elnix--bool ,(funcall sr))))
      ("||" `(or (elnix--bool ,(funcall sl)) (elnix--bool ,(funcall sr))))
      ("->" `(or (not (elnix--bool ,(funcall sl))) (elnix--bool ,(funcall sr))))
      ("//" `(elnix-update (elnix--check-attrs ,(funcall sl))
                           (elnix--check-attrs ,(funcall sr))))
      ("++" `(elnix-concat-lists ,(funcall sl) ,(funcall sr)))
      ("|>" `(elnix-app ,(funcall sr) ,(elnix-compile-lazy l env)))
      ("<|" `(elnix-app ,(funcall sl) ,(elnix-compile-lazy r env)))
      (_ (error "elnix: unknown operator %s" op)))))

;;;; Bindings

(defun elnix-compile--pos-table (bindings)
  "A hoisted position table for the static names of BINDINGS, or nil."
  (let ((alist nil))
    (dolist (b bindings)
      (when (eq (car (car b)) 'static)
        (let ((lc (elnix-compile--line-col (nth 1 (car b)))))
          (push (cons (intern (elnix--unibyte (nth 2 (car b)))) lc) alist))))
    (when alist
      (elnix-compile--hoist "pos" `(vector %file ',(nreverse alist))))))

(defun elnix-compile--group (bindings env inner-env)
  "Compile the values of a group of BINDINGS.
ENV is the scope outside the group, INNER-ENV the scope its values see.
Return a plist with :froms, a list of (SYMBOL FORM) for the shared
sources of `inherit (e)', and :values, a list of (NAME FORM KIND) for
the static bindings in order, and :dynamic, a list of (NAME-FORM FORM)."
  (let ((froms nil) (values nil) (dynamic nil))
    (dolist (b bindings)
      (pcase-let ((`(,name ,value ,kind) b))
        (let ((form
               (pcase kind
                 ('inherit (elnix-compile-lazy value env))
                 ('inherit-from
                  (let* ((from (nth 2 value))
                         (cell (assq from froms)))
                    (unless cell
                      (let ((form (elnix-compile-lazy from inner-env)))
                        ;; A source that is a variable is read where it
                        ;; is used: copying it into a temporary would
                        ;; read a recursive binding before it is set.
                        (setq cell (if (symbolp form)
                                       (list from form nil)
                                     (list from (elnix-compile--gensym "from") form))))
                      (push cell froms))
                    `(elnix--delay
                      (elnix-sel (elnix-force ,(nth 1 cell))
                                 ,(elnix-compile--sym-form (nth 3 value))))))
                 (_ (elnix-compile-lazy value inner-env)))))
          (if (eq (car name) 'static)
              (push (list (nth 2 name) form kind) values)
            (push (list `(elnix-dyn-sym-or-skip
                          ,(elnix-compile-strict (nth 2 name) inner-env))
                        form)
                  dynamic)))))
    (list :froms (mapcar #'cdr (cl-remove-if-not (lambda (c) (nth 2 c)) (nreverse froms)))
          :values (nreverse values)
          :dynamic (nreverse dynamic))))

(defun elnix-compile--binding-group (bindings env body-fn &optional force-recursive)
  "Compile a recursive group of BINDINGS (a let or rec) around a body.
BODY-FN is called with the inner environment and the compiled group
and returns the body form.  FORCE-RECURSIVE binds the group's variables
assignably even when no value refers to another."
  (let* ((names (cl-loop for b in bindings
                         when (eq (car (car b)) 'static)
                         collect (nth 2 (car b))))
         (alist (mapcar (lambda (n) (cons n (elnix-compile--var-sym n))) names))
         (frame (vector 'vars alist nil))
         (inner (cons frame env))
         (group (elnix-compile--group bindings env inner))
         (recursive (or force-recursive (aref frame 2)))
         (froms (plist-get group :froms))
         (values (plist-get group :values))
         (body (funcall body-fn inner group)))
    (if (not recursive)
        `(let ,froms
           (let ,(mapcar (lambda (v) (list (elnix-compile--var-sym (car v))
                                           (nth 1 v)))
                         values)
             ,body))
      ;; Recursive: bind every name first, then assign.  A value that is
      ;; a bare variable of the group would read the variable before it
      ;; is assigned, so it is delayed; an inherited outer variable is
      ;; read before the group shadows it.
      (let* ((outer-temps nil)
             (assigns
              (mapcar
               (lambda (v)
                 (let ((sym (elnix-compile--var-sym (car v)))
                       (form (nth 1 v)))
                   (cond
                    ((eq (nth 2 v) 'inherit)
                     (let ((tmp (elnix-compile--gensym "outer")))
                       (push (list tmp form) outer-temps)
                       (list sym tmp)))
                    ((and (symbolp form) (rassq form alist))
                     (list sym `(elnix--delay (elnix-force ,form))))
                    (t (list sym form)))))
               values)))
        `(let ,(nreverse outer-temps)
           (let ,(append (mapcar #'car froms) (mapcar #'car assigns))
             ,@(mapcar (lambda (f) `(setq ,@f)) froms)
             ,@(elnix-compile--chunked (mapcar (lambda (a) `(setq ,@a)) assigns))
             ,body))))))

(defconst elnix-compile--chunk-size 200
  "The most forms of a set construction compiled into one function.")

(defun elnix-compile--chunked (forms)
  "FORMS, split into closures of at most `elnix-compile--chunk-size' forms.
One byte-compiled function holds at most 65536 constants, and the byte
compiler slows down on very large functions, so a set of thousands of
attributes is built by a series of closures, each with constants of its
own.  `elnix--run' is an ordinary function, which keeps the byte
optimiser from inlining the closures back."
  (if (<= (length forms) elnix-compile--chunk-size)
      forms
    (let ((out nil))
      (while forms
        (let ((chunk (cl-subseq forms 0 (min elnix-compile--chunk-size (length forms)))))
          (setq forms (nthcdr elnix-compile--chunk-size forms))
          (push `(elnix--run (lambda () ,@chunk)) out)))
      (nreverse out))))

(defun elnix-compile--build-attrs (pos-table values dynamic value-form-fn)
  "A form building an attribute set.
VALUES are (NAME FORM KIND); VALUE-FORM-FN maps one to the form that
gives its lazy value.  DYNAMIC are (NAME-FORM FORM)."
  (let ((tbl (elnix-compile--gensym "t")))
    `(let ((,tbl (make-hash-table :test 'eq :size ,(+ (length values)
                                                      (length dynamic)))))
       ,@(elnix-compile--chunked
          (mapcar (lambda (v)
                    `(puthash ,(elnix-compile--sym-form (car v))
                              ,(funcall value-form-fn v) ,tbl))
                  values))
       ,@(mapcar (lambda (d) `(elnix--put-dynamic ,tbl ,(car d) ,(nth 1 d)))
                 dynamic)
       (elnix--mkattrs ,tbl ,pos-table))))

(defun elnix-compile--attrs (_pos bindings env)
  (let* ((group (elnix-compile--group bindings env env))
         (pos-table (elnix-compile--pos-table bindings))
         (froms (plist-get group :froms))
         (form (elnix-compile--build-attrs
                pos-table (plist-get group :values) (plist-get group :dynamic)
                (lambda (v) (nth 1 v)))))
    (if froms `(let ,froms ,form) form)))

(defun elnix-compile--rec-attrs (_pos bindings env)
  (let ((pos-table (elnix-compile--pos-table bindings))
        (overrides (cl-some (lambda (b) (and (eq (car (car b)) 'static)
                                             (string= (nth 2 (car b)) "__overrides")))
                            bindings)))
    (elnix-compile--binding-group
     bindings env
     (lambda (_inner group)
       (let ((build (elnix-compile--build-attrs
                     pos-table (plist-get group :values) (plist-get group :dynamic)
                     (lambda (v) (elnix-compile--var-sym (car v))))))
         (if overrides
             (elnix-compile--with-overrides (plist-get group :values) build)
           build)))
     overrides)))

(defun elnix-compile--with-overrides (values build)
  "Wrap BUILD, the construction of a rec set, in the `__overrides' rule.
The legacy rule: the set named `__overrides' is forced when the rec set
is, and each of its attributes replaces the attribute of the same name,
for the other attributes' references too, or is added to the set."
  (let ((ov (elnix-compile--gensym "ov"))
        (set (elnix-compile--gensym "set"))
        (k (elnix-compile--gensym "k"))
        (x (elnix-compile--gensym "x")))
    `(let ((,ov (elnix-force-attrs $__overrides)))
       (maphash (lambda (,k ,x)
                  (cond
                   ,@(mapcar (lambda (v)
                               `((eq ,k ,(elnix-compile--sym-form (car v)))
                                 (setq ,(elnix-compile--var-sym (car v)) ,x)))
                             values)))
                (aref ,ov 1))
       (let ((,set ,build))
         (maphash (lambda (,k ,x) (puthash ,k ,x (aref ,set 1))) (aref ,ov 1))
         (aset ,set 3 nil)
         ,set))))

(defun elnix-compile--let (bindings body env)
  (elnix-compile--binding-group
   bindings env
   (lambda (inner _group) (elnix-compile-strict body inner))))

;;;; Modules

(defun elnix-compile-module (ast line-starts &optional file)
  "Compile AST, with its LINE-STARTS table, to a module form.
The form evaluates to a function of the source directory and the
source file name that returns the module's value.  FILE names the
source in compile errors."
  (let* ((elnix-compile--line-starts line-starts)
         (elnix-compile--hoisted nil)
         (elnix-compile--counter 0)
         (elnix-compile--file file)
         (body (elnix-compile-strict ast nil)))
    `(lambda (%dir %file)
       (ignore %dir %file)
       (let* ,(reverse elnix-compile--hoisted)
         ,body))))

(provide 'elnix-compile)

;;; elnix-compile.el ends here
