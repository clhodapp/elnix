;;; elnix-cli.el --- The elnix command line -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `bin/elnix' runs Emacs in batch mode and calls `elnix-cli-main' with
;; the remaining arguments.  The commands:
;;
;;   elnix eval [FLAGS] FILE | -E EXPR     like nix-instantiate --eval
;;   elnix drv-path [FLAGS] FILE | -E EXPR the .drv path of a derivation
;;   elnix compile FILE                    print the compiled Lisp of FILE
;;
;; The flags of `nix-instantiate' that the evaluation honours: --strict,
;; --json, --xml, --no-location, -A ATTRPATH, --arg NAME EXPR, --argstr
;; NAME VALUE, -I [PREFIX=]PATH, --impure, --system SYSTEM.  Flags that
;; do not change the value (--read-write-mode, --no-show-trace, the lint
;; and experimental-feature settings) are accepted and ignored.

;;; Code:

(require 'cl-lib)
(require 'elnix)

(defun elnix-cli--out (s)
  "Write the unibyte string S to standard output as bytes."
  (let ((coding-system-for-write 'binary))
    (princ s)))

(defun elnix-cli--err (s)
  (princ s #'external-debugging-output))

(defun elnix-cli--split-attr-path (s)
  "Split the attribute path S at dots, honouring double quotes."
  (let ((parts nil) (cur nil) (quoted nil))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (cond
         ((eq c ?\") (setq quoted (not quoted)))
         ((and (eq c ?.) (not quoted))
          (push (apply #'string (nreverse cur)) parts)
          (setq cur nil))
         (t (push c cur)))))
    (push (apply #'string (nreverse cur)) parts)
    (seq-remove #'string-empty-p (nreverse parts))))

(defun elnix-cli--auto-call (v auto-args)
  "Call V with AUTO-ARGS as nix-instantiate's autoCallFunction does."
  (let ((f (elnix-force v)))
    (cond
     ((and (elnix--attrs-p f)
           (not (eq (elnix--attr f '__functor) 'elnix--missing)))
      (let ((inner (elnix-force (elnix-app (elnix--attr f '__functor) f))))
        (elnix-cli--auto-call-lam inner auto-args f)))
     (t (elnix-cli--auto-call-lam f auto-args nil)))))

(defun elnix-cli--auto-call-lam (f auto-args functor-self)
  (if (elnix--lam-p f)
      (let ((alist nil))
        (dolist (formal (aref f 2))
          (let ((a (assq (car formal) auto-args)))
            (cond
             (a (push (cons (car a) (cdr a)) alist))
             ((not (cdr formal))
              (elnix-fail "cannot evaluate a function that has an argument without a value ('%s')"
                          (symbol-name (car formal)))))))
        (elnix-force (elnix-app f (elnix-attrs-from-alist alist))))
    (or functor-self f)))

(defun elnix-cli--select-path (v path auto-args)
  (dolist (attr path)
    (setq v (elnix-cli--auto-call v auto-args))
    (let ((x (elnix-force v)))
      (cond
       ((elnix--attrs-p x)
        (let ((e (elnix--attr x (intern attr))))
          (when (eq e 'elnix--missing)
            (elnix-fail "attribute '%s' in selection path not found" attr))
          (setq v (elnix-force e))))
       ((and (vectorp x) (string-match-p "\\`[0-9]+\\'" attr))
        (let ((i (string-to-number attr)))
          (when (>= i (length x))
            (elnix-fail "list index %d in selection path is out of range" i))
          (setq v (elnix-force (aref x i)))))
       (t (elnix-fail "the expression selected by the selection path is not a set")))))
  v)

(defun elnix-cli--parse-include (arg)
  "The search path entry of the -I argument ARG: (PREFIX . PATH)."
  (if (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" arg)
      (cons (match-string 1 arg) (expand-file-name (match-string 2 arg)))
    (cons "" (expand-file-name arg))))

(defun elnix-cli--nix-path-env ()
  (let ((env (getenv "NIX_PATH")))
    (and env (> (length env) 0)
         (mapcar #'elnix-cli--parse-include (split-string env ":" t)))))

(defun elnix-cli--eval (args mode)
  "Evaluate as nix-instantiate does with ARGS; MODE is `eval' or `drv-path'."
  (let ((strict nil) (output 'nix) (expr nil) (file nil)
        (attr-paths nil) (auto-args nil) (includes nil))
    ;; nix-instantiate reads the environment unless told --pure-eval.
    (setq elnix-impure t)
    (while args
      (let ((a (pop args)))
        (pcase a
          ("--eval" nil)
          ("--strict" (setq strict t))
          ("--json" (setq output 'json))
          ("--xml" (setq output 'xml))
          ((or "--no-location" "--read-write-mode" "--no-show-trace"
               "--show-trace" "--parse"))
          ((or "-E" "--expr") (setq expr (pop args)))
          ((or "-A" "--attr") (push (pop args) attr-paths))
          ("--arg" (let* ((n (pop args)) (e (pop args)))
                     (push (cons (intern n) (let ((src e))
                                              (elnix--delay (elnix-eval-string src))))
                           auto-args)))
          ("--argstr" (let* ((n (pop args)) (s (pop args)))
                        (push (cons (intern n) (elnix--unibyte s)) auto-args)))
          ("-I" (push (elnix-cli--parse-include (pop args)) includes))
          ("--impure" (setq elnix-impure t))
          ("--pure-eval" (setq elnix-impure nil))
          ("--system" (setq elnix-current-system (pop args)))
          ((or "--extra-experimental-features" "--experimental-features")
           (when (member "parse-toml-timestamps" (split-string (pop args)))
             (setq elnix-toml-timestamps t)))
          ((or "--max-call-depth" "--lint-url-literals"
               "--lint-short-path-literals" "--lint-absolute-path-literals"
               "--option")
           (pop args)
           (when (string= a "--option") (pop args)))
          (_ (if (string-prefix-p "-" a)
                 (elnix-fail "unrecognised flag '%s'" a)
               (setq file a))))))
    (setq elnix-nix-path (append (nreverse includes) (elnix-cli--nix-path-env)))
    (elnix-reset-builtins-cache)
    (let* ((root (cond (expr (elnix-eval-string expr default-directory "«string»"))
                       (file (elnix-eval-file file))
                       (t (elnix-fail "no expression or file given"))))
           (paths (or (nreverse attr-paths) (list ""))))
      (dolist (p paths)
        (let* ((v (elnix-cli--select-path root (elnix-cli--split-attr-path p)
                                          auto-args))
               (res (if auto-args (elnix-cli--auto-call v auto-args) (elnix-force v))))
          (pcase mode
            ('eval
             (pcase output
               ('json (elnix-cli--out (elnix--sval (elnix-to-json res))))
               ('xml (elnix-cli--out (elnix--sval (elnix-to-xml res))))
               (_ (when strict (elnix-deep-force res))
                  (elnix-cli--out (elnix-print-value res strict))))
             (elnix-cli--out "\n"))
            ('drv-path
             (let ((x (elnix-force res)))
               (unless (elnix--attrs-p x)
                 (elnix-fail "expression does not evaluate to a derivation"))
               (elnix-cli--out
                (concat (elnix-force-string-text (elnix--attr x 'drvPath)) "\n"))))))))))

;;;; drv-paths: the jobs of a set, as nix-eval-jobs walks it

(defun elnix-cli--json-line (alist)
  (concat "{"
          (mapconcat (lambda (e) (concat (elnix-json-quote (car e)) ":" (cdr e)))
                     alist ",")
          "}\n"))

(defun elnix-cli--error-text (err)
  (elnix--unibyte
   (if (memq (car err) '(elnix-eval-error elnix-throw elnix-assertion elnix-abort
                                          elnix-infinite-recursion elnix-parse-error))
       (format "%s" (cadr err))
     (format "%S" err))))

(defun elnix-cli--walk-jobs (v path emit)
  "Report the derivations under V at the attribute PATH through EMIT.
Like nix-eval-jobs: a derivation is a job; a set below the top that has
recurseForDerivations = true is walked; anything else is skipped.  An
error stops only the attribute it happens in."
  (elnix--catching
   ((error err (funcall emit path (list (cons "error" (elnix-json-quote
                                                       (elnix-cli--error-text err)))))))
   (let ((x (elnix-force v)))
     (when (elnix--attrs-p x)
       (cond
        ((elnix--is-derivation x)
         (let ((drv (elnix-force-string-text (elnix--attr x 'drvPath))))
           (funcall emit path (list (cons "drvPath" (elnix-json-quote drv))))))
        ((or (null path)
             (let ((r (elnix--attr x 'recurseForDerivations)))
               (and (not (eq r 'elnix--missing)) (eq (elnix-force r) t))))
         (let ((keys (append (elnix-attr-keys x) nil)))
           (dolist (k keys)
             (unless (eq k 'recurseForDerivations)
               (elnix-cli--walk-jobs (elnix--attr x k) (append path (list (symbol-name k)))
                                     emit))))))))))

(defun elnix-cli--drv-paths (args)
  "elnix drv-paths [--shard K/N] [--system S] -E EXPR | FILE"
  (let ((shard nil) (expr nil) (file nil))
    (setq elnix-impure t)
    (while args
      (let ((a (pop args)))
        (pcase a
          ("--shard" (let ((s (split-string (pop args) "/")))
                       (setq shard (cons (string-to-number (car s))
                                         (string-to-number (cadr s))))))
          ("--system" (setq elnix-current-system (pop args)))
          ("--pure-eval" (setq elnix-impure nil))
          ((or "-E" "--expr") (setq expr (pop args)))
          (_ (setq file a)))))
    (elnix-reset-builtins-cache)
    (let* ((root (elnix-force (if expr (elnix-eval-string expr default-directory "«string»")
                                (elnix-eval-file file))))
           (keys (append (elnix-attr-keys root) nil))
           (i 0))
      (dolist (k keys)
        (when (or (null shard) (= (% i (cdr shard)) (car shard)))
          (let ((start (float-time)))
            (elnix-cli--walk-jobs
             (elnix--attr root k) (list (symbol-name k))
             (lambda (path fields)
               (elnix-cli--out
                (elnix-cli--json-line
                 (append
                  (list (cons "attr" (elnix-json-quote (mapconcat #'identity path "."))))
                  fields
                  ;; Milliseconds since the top-level attribute began.
                  (list (cons "ms" (number-to-string
                                    (round (* 1000 (- (float-time) start)))))))))))))
        (setq i (1+ i))))))

(defun elnix-cli--compile (args)
  (let ((form (elnix-compile-file-form (expand-file-name (car args))))
        (print-escape-newlines t))
    (elnix-cli--out (pp-to-string form))))

(defun elnix-cli-main ()
  "The entry point of bin/elnix; reads `command-line-args-left'."
  (let ((args command-line-args-left)
        (gc-cons-threshold (* 1024 1024 1024))
        (max-lisp-eval-depth 1000000))
    (setq command-line-args-left nil)
    (let ((code
           (condition-case err
               (progn
                 (pcase (car args)
                   ("eval" (elnix-cli--eval (cdr args) 'eval))
                   ("drv-path" (elnix-cli--eval (cdr args) 'drv-path))
                   ("compile" (elnix-cli--compile (cdr args)))
                   ("drv-paths" (elnix-cli--drv-paths (cdr args)))
                   ;; elnix compile-tree DIR [K/N]
                   ("compile-tree"
                    (let ((shard (and (nth 2 args)
                                      (let ((s (split-string (nth 2 args) "/")))
                                        (cons (string-to-number (car s))
                                              (string-to-number (cadr s)))))))
                      (elnix-compile-tree (expand-file-name (nth 1 args)) shard)))
                   ;; Parse and resolve scope without evaluating.
                   ("check" (elnix-compile-file-form (expand-file-name (cadr args))))
                   (_ (elnix-cli--err "usage: elnix eval|drv-path|compile ...\n")
                      (kill-emacs 2)))
                 0)
             (elnix-error
              (elnix-cli--err (format "error: %s\n" (error-message-string err)))
              1)
             (error
              (elnix-cli--err (format "error: internal: %S\n" err))
              1))))
      (kill-emacs code))))

(provide 'elnix-cli)

;;; elnix-cli.el ends here
