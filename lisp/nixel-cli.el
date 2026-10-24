;;; nixel-cli.el --- The nixel command line -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `bin/nixel' runs Emacs in batch mode and calls `nixel-cli-main' with
;; the remaining arguments.  The commands:
;;
;;   nixel eval [FLAGS] FILE | -E EXPR     like nix-instantiate --eval
;;   nixel drv-path [FLAGS] FILE | -E EXPR the .drv path of a derivation
;;   nixel compile FILE                    print the compiled Lisp of FILE
;;
;; The flags of `nix-instantiate' that the evaluation honours: --strict,
;; --json, --xml, --no-location, -A ATTRPATH, --arg NAME EXPR, --argstr
;; NAME VALUE, -I [PREFIX=]PATH, --impure, --system SYSTEM.  Flags that
;; do not change the value (--read-write-mode, --no-show-trace, the lint
;; and experimental-feature settings) are accepted and ignored.

;;; Code:

(require 'cl-lib)
(require 'nixel)

(defun nixel-cli--out (s)
  "Write the unibyte string S to standard output as bytes."
  (let ((coding-system-for-write 'binary))
    (princ s)))

(defun nixel-cli--err (s)
  (princ s #'external-debugging-output))

(defun nixel-cli--split-attr-path (s)
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

(defun nixel-cli--auto-call (v auto-args)
  "Call V with AUTO-ARGS as nix-instantiate's autoCallFunction does."
  (let ((f (nixel-force v)))
    (cond
     ((and (nixel--attrs-p f)
           (not (eq (nixel--attr f '__functor) 'nixel--missing)))
      (let ((inner (nixel-force (nixel-app (nixel--attr f '__functor) f))))
        (nixel-cli--auto-call-lam inner auto-args f)))
     (t (nixel-cli--auto-call-lam f auto-args nil)))))

(defun nixel-cli--auto-call-lam (f auto-args functor-self)
  (if (nixel--lam-p f)
      (let ((alist nil))
        (dolist (formal (aref f 2))
          (let ((a (assq (car formal) auto-args)))
            (cond
             (a (push (cons (car a) (cdr a)) alist))
             ((not (cdr formal))
              (nixel-fail "cannot evaluate a function that has an argument without a value ('%s')"
                          (symbol-name (car formal)))))))
        (nixel-force (nixel-app f (nixel-attrs-from-alist alist))))
    (or functor-self f)))

(defun nixel-cli--select-path (v path auto-args)
  (dolist (attr path)
    (setq v (nixel-cli--auto-call v auto-args))
    (let ((x (nixel-force v)))
      (cond
       ((nixel--attrs-p x)
        (let ((e (nixel--attr x (intern attr))))
          (when (eq e 'nixel--missing)
            (nixel-fail "attribute '%s' in selection path not found" attr))
          (setq v (nixel-force e))))
       ((and (vectorp x) (string-match-p "\\`[0-9]+\\'" attr))
        (let ((i (string-to-number attr)))
          (when (>= i (length x))
            (nixel-fail "list index %d in selection path is out of range" i))
          (setq v (nixel-force (aref x i)))))
       (t (nixel-fail "the expression selected by the selection path is not a set")))))
  v)

(defun nixel-cli--parse-include (arg)
  "The search path entry of the -I argument ARG: (PREFIX . PATH)."
  (if (string-match "\\`\\([^=]*\\)=\\(.*\\)\\'" arg)
      (cons (match-string 1 arg) (expand-file-name (match-string 2 arg)))
    (cons "" (expand-file-name arg))))

(defun nixel-cli--nix-path-env ()
  (let ((env (getenv "NIX_PATH")))
    (and env (> (length env) 0)
         (mapcar #'nixel-cli--parse-include (split-string env ":" t)))))

(defun nixel-cli--eval (args mode)
  "Evaluate as nix-instantiate does with ARGS; MODE is `eval' or `drv-path'."
  (let ((strict nil) (output 'nix) (expr nil) (file nil)
        (attr-paths nil) (auto-args nil) (includes nil))
    ;; nix-instantiate reads the environment unless told --pure-eval.
    (setq nixel-impure t)
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
                                              (nixel--delay (nixel-eval-string src))))
                           auto-args)))
          ("--argstr" (let* ((n (pop args)) (s (pop args)))
                        (push (cons (intern n) (nixel--unibyte s)) auto-args)))
          ("-I" (push (nixel-cli--parse-include (pop args)) includes))
          ("--impure" (setq nixel-impure t))
          ("--pure-eval" (setq nixel-impure nil))
          ("--system" (setq nixel-current-system (pop args)))
          ((or "--extra-experimental-features" "--experimental-features")
           (when (member "parse-toml-timestamps" (split-string (pop args)))
             (setq nixel-toml-timestamps t)))
          ((or "--max-call-depth" "--lint-url-literals"
               "--lint-short-path-literals" "--lint-absolute-path-literals"
               "--option")
           (pop args)
           (when (string= a "--option") (pop args)))
          (_ (if (string-prefix-p "-" a)
                 (nixel-fail "unrecognised flag '%s'" a)
               (setq file a))))))
    (setq nixel-nix-path (append (nreverse includes) (nixel-cli--nix-path-env)))
    (nixel-reset-builtins-cache)
    (let* ((root (cond (expr (nixel-eval-string expr default-directory "«string»"))
                       (file (nixel-eval-file file))
                       (t (nixel-fail "no expression or file given"))))
           (paths (or (nreverse attr-paths) (list ""))))
      (dolist (p paths)
        (let* ((v (nixel-cli--select-path root (nixel-cli--split-attr-path p)
                                          auto-args))
               (res (if auto-args (nixel-cli--auto-call v auto-args) (nixel-force v))))
          (pcase mode
            ('eval
             (pcase output
               ('json (nixel-cli--out (nixel--sval (nixel-to-json res))))
               ('xml (nixel-cli--out (nixel--sval (nixel-to-xml res))))
               (_ (when strict (nixel-deep-force res))
                  (nixel-cli--out (nixel-print-value res strict))))
             (nixel-cli--out "\n"))
            ('drv-path
             (let ((x (nixel-force res)))
               (unless (nixel--attrs-p x)
                 (nixel-fail "expression does not evaluate to a derivation"))
               (nixel-cli--out
                (concat (nixel-force-string-text (nixel--attr x 'drvPath)) "\n"))))))))))

(defun nixel-cli--compile (args)
  (let ((form (nixel-compile-file-form (expand-file-name (car args))))
        (print-escape-newlines t))
    (nixel-cli--out (pp-to-string form))))

(defun nixel-cli-main ()
  "The entry point of bin/nixel; reads `command-line-args-left'."
  (let ((args command-line-args-left)
        (gc-cons-threshold (* 1024 1024 1024))
        (max-lisp-eval-depth 1000000))
    (setq command-line-args-left nil)
    (let ((code
           (condition-case err
               (progn
                 (pcase (car args)
                   ("eval" (nixel-cli--eval (cdr args) 'eval))
                   ("drv-path" (nixel-cli--eval (cdr args) 'drv-path))
                   ("compile" (nixel-cli--compile (cdr args)))
                   ;; Parse and resolve scope without evaluating.
                   ("check" (nixel-compile-file-form (expand-file-name (cadr args))))
                   (_ (nixel-cli--err "usage: nixel eval|drv-path|compile ...\n")
                      (kill-emacs 2)))
                 0)
             (nixel-error
              (nixel-cli--err (format "error: %s\n" (error-message-string err)))
              1)
             (error
              (nixel-cli--err (format "error: internal: %S\n" err))
              1))))
      (kill-emacs code))))

(provide 'nixel-cli)

;;; nixel-cli.el ends here
