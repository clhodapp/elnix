;;; nixel-compile.el --- Compile Nix syntax trees to Emacs Lisp -*- lexical-binding: t; -*-

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
(require 'nixel-parse)
(require 'nixel-runtime)

(defvar nixel-compile--line-starts nil
  "The line table of the file being compiled.")

(defvar nixel-compile--hoisted nil
  "Module-level bindings, newest first: (SYMBOL FORM).
Path constants and position tables are computed once per module load
and shared by every evaluation of the expression that names them.")

(defvar nixel-compile--counter 0
  "The counter that makes the names of compiler temporaries unique.")

(defvar nixel-compile--file nil
  "The name of the source file being compiled, for error messages.")

(defvar nixel-compile-builtin-arity nil
  "A function from a builtin name to its arity, or nil.
Set by the builtins library; nil arity means the builtin is not called
directly.")

(defvar nixel-compile-global-p nil
  "A function returning non-nil when a name is a global builtin.")

(defun nixel-compile--gensym (prefix)
  "A fresh symbol named %PREFIXN."
  (setq nixel-compile--counter (1+ nixel-compile--counter))
  (intern (format "%%%s%d" prefix nixel-compile--counter)))

(defun nixel-compile--var-sym (name)
  "The Lisp variable of the Nix variable NAME."
  (intern (concat "$" name)))

(defun nixel-compile--line-col (pos)
  (nixel-parse-line-col pos nixel-compile--line-starts))

(defun nixel-compile--fail (pos fmt &rest args)
  (let ((lc (nixel-compile--line-col pos)))
    (signal 'nixel-parse-error
            (list (format "%s at %s:%d:%d" (apply #'format fmt args)
                          (or nixel-compile--file "«string»")
                          (car lc) (cdr lc))))))

(defun nixel-compile--sym-form (name)
  "A form evaluating to the attribute symbol of the string NAME."
  (let ((s (nixel--unibyte name)))
    (if (string-match-p "\\`[[:ascii:]]*\\'" s)
        (list 'quote (intern s))
      ;; A symbol with bytes outside ASCII cannot round-trip through a
      ;; source file, so it is interned when the module loads.
      `(load-time-value (intern ,s)))))

(defun nixel-compile--hoist (prefix form)
  "Bind FORM at module level and return the variable that holds it."
  (let ((sym (nixel-compile--gensym prefix)))
    (push (list sym form) nixel-compile--hoisted)
    sym))

;;;; Environments

(defun nixel-compile--lookup (name env)
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
     ((and nixel-compile-global-p (funcall nixel-compile-global-p name))
      (list 'global name))
     (with (list 'with with))
     (t nil))))

(defun nixel-compile--innermost-with (env)
  (cl-loop for fr in env
           when (eq (aref fr 0) 'with) return (aref fr 1)))

;;;; Expressions

(defun nixel-compile--trivial-p (e env)
  "Whether E compiles to the same form in lazy and strict-free use.
Such an expression needs no thunk in a lazy position."
  (pcase (car e)
    ((or 'int 'float) t)
    ('str (cl-every #'stringp (nth 2 e)))
    ('lambda t)
    ('list (null (nth 2 e)))
    ('var (eq (car (nixel-compile--lookup (nth 2 e) env)) 'lex))
    (_ nil)))

(defun nixel-compile-lazy (e env)
  "A form producing the lazy value of E in ENV."
  (pcase (car e)
    ((or 'int 'float 'lambda) (nixel-compile-strict e env))
    ('str (if (cl-every #'stringp (nth 2 e))
              (nixel-compile-strict e env)
            `(nixel--delay ,(nixel-compile-strict e env))))
    ('list (if (null (nth 2 e))
               []
             `(nixel--delay ,(nixel-compile-strict e env))))
    ('var
     (let ((r (nixel-compile--lookup (nth 2 e) env)))
       (pcase (car r)
         ('lex (nth 1 r))
         ('global (if (string= (nth 2 e) "__curPos")
                      (nixel-compile-strict e env)
                    (nixel-compile--global-value (nth 2 e))))
         (_ `(nixel--delay ,(nixel-compile-strict e env))))))
    (_ `(nixel--delay ,(nixel-compile-strict e env)))))

(defun nixel-compile--global-value (name)
  "A form evaluating to the value of the global NAME."
  (pcase name
    ("true" t)
    ("false" nil)
    ("null" :null)
    (_ `(nixel-builtin ,(nixel-compile--sym-form name)))))

(defun nixel-compile-strict (e env)
  "A form producing the value of E in ENV."
  (pcase e
    (`(int ,_ ,n) n)
    (`(float ,_ ,f) f)
    (`(str ,_ ,parts) (nixel-compile--string parts env))
    (`(path ,pos ,anchor ,parts) (nixel-compile--path pos anchor parts env))
    (`(var ,pos ,name) (nixel-compile--var pos name env))
    (`(select ,_ ,subject ,path ,default)
     (nixel-compile--select subject path default env))
    (`(has ,_ ,subject ,path)
     `(nixel-has ,(nixel-compile-strict subject env)
                 ,(nixel-compile--attrpath-form path env)))
    (`(lambda ,_ ,param ,formals ,body)
     (nixel-compile--lambda param formals body env))
    (`(call . ,_) (nixel-compile--call e env))
    (`(list ,_ ,elems)
     (if (null elems)
         []
       `(vector ,@(mapcar (lambda (x) (nixel-compile-lazy x env)) elems))))
    (`(attrs ,pos ,rec ,bindings)
     (if rec
         (nixel-compile--rec-attrs pos bindings env)
       (nixel-compile--attrs pos bindings env)))
    (`(let ,_ ,bindings ,body) (nixel-compile--let bindings body env))
    (`(with ,_ ,scope ,body)
     (let ((w (nixel-compile--gensym "w")))
       `(let ((,w (cons ,(nixel-compile-lazy scope env)
                        ,(nixel-compile--innermost-with env))))
          ,(nixel-compile-strict body (cons (vector 'with w) env)))))
    (`(assert ,pos ,c ,body)
     (let ((lc (nixel-compile--line-col pos)))
       `(if (nixel--bool ,(nixel-compile-strict c env))
            ,(nixel-compile-strict body env)
          (nixel--assert-fail %file ,(car lc) ,(cdr lc)))))
    (`(if ,_ ,c ,then ,else)
     `(if (nixel--bool ,(nixel-compile-strict c env))
          ,(nixel-compile-strict then env)
        ,(nixel-compile-strict else env)))
    (`(not ,_ ,x) `(not (nixel--bool ,(nixel-compile-strict x env))))
    (`(neg ,_ ,x) `(nixel-sub 0 ,(nixel-compile-strict x env)))
    (`(binop ,_ ,op ,l ,r) (nixel-compile--binop op l r env))
    (_ (error "nixel: unknown syntax node %S" (car e)))))

(defun nixel-compile--var (pos name env)
  (let ((r (nixel-compile--lookup name env)))
    (pcase (car r)
      ('lex `(nixel-force ,(nth 1 r)))
      ('with `(nixel-with-get ,(nth 1 r) ,(nixel-compile--sym-form name)))
      ('global
       (if (string= name "__curPos")
           (let ((lc (nixel-compile--line-col pos)))
             `(nixel--cur-pos %file ,(car lc) ,(cdr lc)))
         (nixel-compile--global-value name)))
      (_ (nixel-compile--fail pos "undefined variable '%s'" name)))))

(defun nixel-compile--string (parts env)
  (cond
   ((null parts) "")
   ((cl-every #'stringp parts) (apply #'concat parts))
   (t `(nixel-interp ,@(mapcar (lambda (p)
                                 (if (stringp p) p
                                   (nixel-compile-strict p env)))
                               parts)))))

(defun nixel-compile--path (_pos anchor parts env)
  (pcase anchor
    ('search
     `(nixel--find-file ,(car parts)))
    (_
     (if (cl-every #'stringp parts)
         (let ((text (apply #'concat parts)))
           (pcase anchor
             ('absolute
              (nixel-compile--hoist
               "p" `(nixel--mkpath ,(nixel-canon-path text))))
             ('home `(nixel--home-path ,text))
             (_ (nixel-compile--hoist
                 "p" `(nixel--mkpath (nixel--path-join %dir ,text))))))
       `(nixel--path-interp %dir ',anchor
                            ,@(mapcar (lambda (p)
                                        (if (stringp p) p
                                          (nixel-compile-strict p env)))
                                      parts))))))

(defun nixel-compile--attrname-form (name env)
  "A form evaluating to the symbol of the attribute NAME."
  (pcase name
    (`(static ,_ ,s) (nixel-compile--sym-form s))
    (`(dynamic ,_ ,e) `(nixel-dyn-sym ,(nixel-compile-strict e env)))))

(defun nixel-compile--attrpath-form (path env)
  "A form evaluating to the list of symbols of the attribute PATH."
  (if (cl-every (lambda (n) (eq (car n) 'static)) path)
      (list 'quote (mapcar (lambda (n) (intern (nixel--unibyte (nth 2 n))))
                           path))
    `(list ,@(mapcar (lambda (n) (nixel-compile--attrname-form n env)) path))))

(defun nixel-compile--select (subject path default env)
  (if default
      (let ((v (nixel-compile--gensym "v")))
        `(let ((,v (nixel-sel-or ,(nixel-compile-strict subject env)
                                 ,(nixel-compile--attrpath-form path env))))
           (if (eq ,v 'nixel--missing)
               ,(nixel-compile-strict default env)
             ,v)))
    (let ((form (nixel-compile-strict subject env)))
      (dolist (n path)
        (setq form `(nixel-sel ,form ,(nixel-compile--attrname-form n env))))
      form)))

;;;; Functions

(defun nixel-compile--lambda (param formals body env)
  (if (null formals)
      (let ((sym (nixel-compile--var-sym param)))
        `(lambda (,sym)
           ,(nixel-compile-strict
             body (cons (vector 'vars (list (cons param sym)) nil) env))))
    (nixel-compile--formals-lambda param formals body env)))

(defun nixel-compile--formals-lambda (param formals body env)
  (let* ((entries (car formals))
         (ellipsis (cdr formals))
         (arg (nixel-compile--gensym "arg"))
         (set (nixel-compile--gensym "set"))
         (alist (append
                 (mapcar (lambda (f) (cons (car f) (nixel-compile--var-sym (car f))))
                         entries)
                 (and param (list (cons param (nixel-compile--var-sym param))))))
         (frame (vector 'vars alist nil))
         (inner (cons frame env))
         (spec (mapcar (lambda (f)
                         (cons (intern (nixel--unibyte (car f)))
                               (and (nth 2 f) t)))
                       entries))
         ;; Each formal's form, compiled in the function's own scope so
         ;; that a default can name the other formals.
         (inits (mapcar
                 (lambda (f)
                   (let ((sym-form (nixel-compile--sym-form (car f))))
                     (list (nixel-compile--var-sym (car f))
                           (if (nth 2 f)
                               (let ((x (nixel-compile--gensym "x"))
                                     (default (nixel-compile-lazy (nth 2 f) inner)))
                                 ;; A default that is another formal
                                 ;; would read it before it is assigned.
                                 (when (and (symbolp default) (rassq default alist))
                                   (setq default `(nixel--delay (nixel-force ,default))))
                                 `(let ((,x (gethash ,sym-form (aref ,set 1)
                                                     'nixel--missing)))
                                    (if (eq ,x 'nixel--missing)
                                        ,default
                                      ,x)))
                             `(gethash ,sym-form (aref ,set 1))))))
                 entries))
         (recursive (aref frame 2))
         (param-binding (and param (list (list (nixel-compile--var-sym param) arg))))
         (body-form (nixel-compile-strict body inner)))
    `(nixel--mklam
      (lambda (,arg)
        (let ((,set (nixel-formals-check ,arg ',spec ,ellipsis)))
          ,(if recursive
               `(let ,(append (mapcar #'car param-binding) (mapcar #'car inits))
                  ,@(mapcar (lambda (b) `(setq ,@b)) param-binding)
                  ,@(mapcar (lambda (b) `(setq ,@b)) inits)
                  ,body-form)
             `(let (,@param-binding ,@inits)
                ,body-form))))
      ',spec ,ellipsis)))

(defun nixel-compile--builtin-callee (fn env)
  "The builtin name FN refers to, when FN is a reference to a builtin."
  (pcase fn
    (`(var ,_ ,name)
     (and (eq (car (nixel-compile--lookup name env)) 'global)
          (if (string-prefix-p "__" name) (substring name 2) name)))
    (`(select ,_ (var ,_ "builtins") ((static ,_ ,name)) nil)
     (and (eq (car (nixel-compile--lookup "builtins" env)) 'global)
          name))))

(defun nixel-compile--call (e env)
  ;; Flatten the curried application f a b c.
  (let ((args nil) (fn e))
    (while (eq (car fn) 'call)
      (push (nth 3 fn) args)
      (setq fn (nth 2 fn)))
    (let* ((bname (nixel-compile--builtin-callee fn env))
           (arity (and bname nixel-compile-builtin-arity
                       (funcall nixel-compile-builtin-arity bname))))
      (if (and arity (> arity 0) (>= (length args) arity))
          (let ((form `(,(intern (concat "nixel-b--" bname))
                        ,@(mapcar (lambda (a) (nixel-compile-lazy a env))
                                  (cl-subseq args 0 arity)))))
            (dolist (a (nthcdr arity args))
              (setq form `(nixel-app ,form ,(nixel-compile-lazy a env))))
            form)
        (let ((form (nixel-compile-strict fn env)))
          (dolist (a args)
            (setq form `(nixel-app ,form ,(nixel-compile-lazy a env))))
          form)))))

;;;; Operators

(defun nixel-compile--binop (op l r env)
  (let ((sl (lambda () (nixel-compile-strict l env)))
        (sr (lambda () (nixel-compile-strict r env))))
    (pcase op
      ("+" `(nixel-add ,(funcall sl) ,(funcall sr)))
      ("-" `(nixel-sub ,(funcall sl) ,(funcall sr)))
      ("*" `(nixel-mul ,(funcall sl) ,(funcall sr)))
      ("/" `(nixel-div ,(funcall sl) ,(funcall sr)))
      ("==" `(nixel-eq ,(funcall sl) ,(funcall sr)))
      ("!=" `(not (nixel-eq ,(funcall sl) ,(funcall sr))))
      ;; Nix rewrites the comparisons to __lessThan, which fixes the
      ;; order the operands are evaluated in.
      ("<" `(nixel-less-than ,(funcall sl) ,(funcall sr)))
      (">" `(nixel-less-than ,(funcall sr) ,(funcall sl)))
      ("<=" `(not (nixel-less-than ,(funcall sr) ,(funcall sl))))
      (">=" `(not (nixel-less-than ,(funcall sl) ,(funcall sr))))
      ("&&" `(and (nixel--bool ,(funcall sl)) (nixel--bool ,(funcall sr))))
      ("||" `(or (nixel--bool ,(funcall sl)) (nixel--bool ,(funcall sr))))
      ("->" `(or (not (nixel--bool ,(funcall sl))) (nixel--bool ,(funcall sr))))
      ("//" `(nixel-update (nixel--check-attrs ,(funcall sl))
                           (nixel--check-attrs ,(funcall sr))))
      ("++" `(nixel-concat-lists ,(funcall sl) ,(funcall sr)))
      ("|>" `(nixel-app ,(funcall sr) ,(nixel-compile-lazy l env)))
      ("<|" `(nixel-app ,(funcall sl) ,(nixel-compile-lazy r env)))
      (_ (error "nixel: unknown operator %s" op)))))

;;;; Bindings

(defun nixel-compile--pos-table (bindings)
  "A hoisted position table for the static names of BINDINGS, or nil."
  (let ((alist nil))
    (dolist (b bindings)
      (when (eq (car (car b)) 'static)
        (let ((lc (nixel-compile--line-col (nth 1 (car b)))))
          (push (cons (intern (nixel--unibyte (nth 2 (car b)))) lc) alist))))
    (when alist
      (nixel-compile--hoist "pos" `(vector %file ',(nreverse alist))))))

(defun nixel-compile--group (bindings env inner-env)
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
                 ('inherit (nixel-compile-lazy value env))
                 ('inherit-from
                  (let* ((from (nth 2 value))
                         (cell (assq from froms)))
                    (unless cell
                      (let ((form (nixel-compile-lazy from inner-env)))
                        ;; A source that is a variable is read where it
                        ;; is used: copying it into a temporary would
                        ;; read a recursive binding before it is set.
                        (setq cell (if (symbolp form)
                                       (list from form nil)
                                     (list from (nixel-compile--gensym "from") form))))
                      (push cell froms))
                    `(nixel--delay
                      (nixel-sel (nixel-force ,(nth 1 cell))
                                 ,(nixel-compile--sym-form (nth 3 value))))))
                 (_ (nixel-compile-lazy value inner-env)))))
          (if (eq (car name) 'static)
              (push (list (nth 2 name) form kind) values)
            (push (list `(nixel-dyn-sym-or-skip
                          ,(nixel-compile-strict (nth 2 name) inner-env))
                        form)
                  dynamic)))))
    (list :froms (mapcar #'cdr (cl-remove-if-not (lambda (c) (nth 2 c)) (nreverse froms)))
          :values (nreverse values)
          :dynamic (nreverse dynamic))))

(defun nixel-compile--binding-group (bindings env body-fn &optional force-recursive)
  "Compile a recursive group of BINDINGS (a let or rec) around a body.
BODY-FN is called with the inner environment and the compiled group
and returns the body form.  FORCE-RECURSIVE binds the group's variables
assignably even when no value refers to another."
  (let* ((names (cl-loop for b in bindings
                         when (eq (car (car b)) 'static)
                         collect (nth 2 (car b))))
         (alist (mapcar (lambda (n) (cons n (nixel-compile--var-sym n))) names))
         (frame (vector 'vars alist nil))
         (inner (cons frame env))
         (group (nixel-compile--group bindings env inner))
         (recursive (or force-recursive (aref frame 2)))
         (froms (plist-get group :froms))
         (values (plist-get group :values))
         (body (funcall body-fn inner group)))
    (if (not recursive)
        `(let ,froms
           (let ,(mapcar (lambda (v) (list (nixel-compile--var-sym (car v))
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
                 (let ((sym (nixel-compile--var-sym (car v)))
                       (form (nth 1 v)))
                   (cond
                    ((eq (nth 2 v) 'inherit)
                     (let ((tmp (nixel-compile--gensym "outer")))
                       (push (list tmp form) outer-temps)
                       (list sym tmp)))
                    ((and (symbolp form) (rassq form alist))
                     (list sym `(nixel--delay (nixel-force ,form))))
                    (t (list sym form)))))
               values)))
        `(let ,(nreverse outer-temps)
           (let ,(append (mapcar #'car froms) (mapcar #'car assigns))
             ,@(mapcar (lambda (f) `(setq ,@f)) froms)
             ,@(mapcar (lambda (a) `(setq ,@a)) assigns)
             ,body))))))

(defun nixel-compile--build-attrs (pos-table values dynamic value-form-fn)
  "A form building an attribute set.
VALUES are (NAME FORM KIND); VALUE-FORM-FN maps one to the form that
gives its lazy value.  DYNAMIC are (NAME-FORM FORM)."
  (let ((tbl (nixel-compile--gensym "t")))
    `(let ((,tbl (make-hash-table :test 'eq :size ,(+ (length values)
                                                      (length dynamic)))))
       ,@(mapcar (lambda (v)
                   `(puthash ,(nixel-compile--sym-form (car v))
                             ,(funcall value-form-fn v) ,tbl))
                 values)
       ,@(mapcar (lambda (d) `(nixel--put-dynamic ,tbl ,(car d) ,(nth 1 d)))
                 dynamic)
       (nixel--mkattrs ,tbl ,pos-table))))

(defun nixel-compile--attrs (_pos bindings env)
  (let* ((group (nixel-compile--group bindings env env))
         (pos-table (nixel-compile--pos-table bindings))
         (froms (plist-get group :froms))
         (form (nixel-compile--build-attrs
                pos-table (plist-get group :values) (plist-get group :dynamic)
                (lambda (v) (nth 1 v)))))
    (if froms `(let ,froms ,form) form)))

(defun nixel-compile--rec-attrs (_pos bindings env)
  (let ((pos-table (nixel-compile--pos-table bindings))
        (overrides (cl-some (lambda (b) (and (eq (car (car b)) 'static)
                                             (string= (nth 2 (car b)) "__overrides")))
                            bindings)))
    (nixel-compile--binding-group
     bindings env
     (lambda (_inner group)
       (let ((build (nixel-compile--build-attrs
                     pos-table (plist-get group :values) (plist-get group :dynamic)
                     (lambda (v) (nixel-compile--var-sym (car v))))))
         (if overrides
             (nixel-compile--with-overrides (plist-get group :values) build)
           build)))
     overrides)))

(defun nixel-compile--with-overrides (values build)
  "Wrap BUILD, the construction of a rec set, in the `__overrides' rule.
The legacy rule: the set named `__overrides' is forced when the rec set
is, and each of its attributes replaces the attribute of the same name,
for the other attributes' references too, or is added to the set."
  (let ((ov (nixel-compile--gensym "ov"))
        (set (nixel-compile--gensym "set"))
        (k (nixel-compile--gensym "k"))
        (x (nixel-compile--gensym "x")))
    `(let ((,ov (nixel-force-attrs $__overrides)))
       (maphash (lambda (,k ,x)
                  (cond
                   ,@(mapcar (lambda (v)
                               `((eq ,k ,(nixel-compile--sym-form (car v)))
                                 (setq ,(nixel-compile--var-sym (car v)) ,x)))
                             values)))
                (aref ,ov 1))
       (let ((,set ,build))
         (maphash (lambda (,k ,x) (puthash ,k ,x (aref ,set 1))) (aref ,ov 1))
         (aset ,set 3 nil)
         ,set))))

(defun nixel-compile--let (bindings body env)
  (nixel-compile--binding-group
   bindings env
   (lambda (inner _group) (nixel-compile-strict body inner))))

;;;; Modules

(defun nixel-compile-module (ast line-starts &optional file)
  "Compile AST, with its LINE-STARTS table, to a module form.
The form evaluates to a function of the source directory and the
source file name that returns the module's value.  FILE names the
source in compile errors."
  (let* ((nixel-compile--line-starts line-starts)
         (nixel-compile--hoisted nil)
         (nixel-compile--counter 0)
         (nixel-compile--file file)
         (body (nixel-compile-strict ast nil)))
    `(lambda (%dir %file)
       (ignore %dir %file)
       (let* ,(reverse nixel-compile--hoisted)
         ,body))))

(provide 'nixel-compile)

;;; nixel-compile.el ends here
