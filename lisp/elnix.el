;;; elnix.el --- Nix compiled to Emacs Lisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;; Author: Chris Hodapp
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, nix

;;; Commentary:

;; elnix compiles Nix files to Emacs Lisp and evaluates them, with the
;; derivations Nix itself would compute.  Each Nix file compiles to one
;; Lisp function of its directory and name, byte-compiled and cached by
;; the SHA-256 of its source; `import' loads the compiled function,
;; calls it once and keeps the value, as Nix keeps the value of every
;; file it imports.
;;
;; Entry points: `elnix-eval-string', `elnix-eval-file' and
;; `elnix-import-path'.  `elnix-cli' (bin/elnix) is the command line.

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'elnix-parse)
(require 'elnix-runtime)
(require 'elnix-compile)
(require 'elnix-builtins)
(require 'elnix-store)
(require 'elnix-derivation)
(require 'elnix-json)
(require 'elnix-regex)
(require 'elnix-print)
(require 'elnix-toml)
(require 'elnix-xml)

(defconst elnix-version "0.1.0")

(defconst elnix--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory holding elnix's Lisp files.")

(setq elnix-corepkgs-dir
      (expand-file-name "../corepkgs/" elnix--lisp-dir))

(defvar elnix-cache-dir
  (or (getenv "ELNIX_CACHE_DIR")
      (expand-file-name "elnix" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
  "Where compiled modules are cached.")

(defvar elnix-byte-compile t
  "Non-nil to byte-compile modules; nil interprets the compiled forms.")

(defvar elnix--import-cache (make-hash-table :test 'equal)
  "The values of imported files by canonical path.")

(defvar elnix--loaded-module nil
  "Set by a compiled module file when it is loaded.")

(defconst elnix--compiler-files
  '("elnix-parse.el" "elnix-compile.el")
  "The files whose contents determine the compiled form of a module.")

(defvar elnix--compiler-digest nil)

(defun elnix--compiler-digest ()
  "A digest of the compiler, so a changed compiler invalidates the cache."
  (or elnix--compiler-digest
      (setq elnix--compiler-digest
            (secure-hash 'sha256
                         (mapconcat (lambda (f)
                                      (elnix-read-file-bytes
                                       (expand-file-name f elnix--lisp-dir)))
                                    elnix--compiler-files "")))))

;;;; Compiling modules

(defun elnix-compile-file-form (file)
  "The module form of the Nix FILE."
  (let ((parsed (elnix-parse-file file)))
    (elnix-compile-module (car parsed) (cdr parsed) file)))

(defun elnix--write-module (form el-file)
  "Write the module FORM to EL-FILE as a loadable Lisp file."
  (make-directory (file-name-directory el-file) t)
  (with-temp-file el-file
    (set-buffer-multibyte nil)
    (insert ";;; -*- lexical-binding: t; coding: raw-text-unix -*-\n")
    (let ((print-escape-newlines t)
          (print-escape-control-characters t)
          ;; Without print-circle, prin1 refuses forms nested more than
          ;; 200 deep, which long operator chains in Nix produce.
          (print-circle t)
          (print-length nil)
          (print-level nil))
      (prin1 `(setq elnix--loaded-module ,form) (current-buffer)))
    (insert "\n")))

(defvar elnix-optimize-limit 400000
  "Modules whose Lisp file is larger than this many bytes are not optimised.
The byte optimiser is the larger part of compiling a generated package
set of thousands of attributes, and such files hold data, not the code
evaluation spends its time in.")

(defun elnix--byte-compile-file (el-file)
  (let ((byte-compile-warnings nil)
        (gc-cons-threshold (* 128 1024 1024))
        (byte-optimize (and (< (file-attribute-size (file-attributes el-file))
                               elnix-optimize-limit)
                            byte-optimize))
        (byte-compile-verbose nil)
        (inhibit-message t)
        (message-log-max nil)
        (max-lisp-eval-depth (max max-lisp-eval-depth 100000)))
    (unless (byte-compile-file el-file)
      (error "elnix: byte compilation of %s failed" el-file))))

(defun elnix--module-cache-file (file)
  "The cache file stem for the compiled form of the Nix FILE."
  (let ((key (secure-hash 'sha256
                          (concat (elnix--compiler-digest) "\0" file "\0"
                                  (elnix-read-file-bytes file)))))
    (expand-file-name (concat (substring key 0 2) "/" (substring key 2))
                      elnix-cache-dir)))

(defun elnix--compile-to-cache (file stem)
  "Compile the Nix FILE to STEM.elc.
Several processes may compile the same file at once, so each writes
under a name of its own and renames the result into place."
  (let* ((tmp (format "%s.%d" stem (emacs-pid)))
         (el (concat tmp ".el")))
    (unwind-protect
        (progn
          (elnix--write-module (elnix-compile-file-form file) el)
          (elnix--byte-compile-file el)
          (rename-file (concat tmp ".elc") (concat stem ".elc") t))
      (when (file-exists-p el) (delete-file el)))))

(defun elnix-compile-tree (dir &optional shard)
  "Compile every Nix file under DIR into the module cache.
SHARD (K . N) compiles only every Nth file, starting with the Kth, so
that N processes can share the work.  Return the number of failures."
  (let ((files (sort (directory-files-recursively dir "\\.nix\\'") #'string<))
        (i 0) (failures 0))
    (dolist (f files)
      (when (or (null shard) (= (% i (cdr shard)) (car shard)))
        (condition-case err
            (let ((stem (elnix--module-cache-file f)))
              (unless (file-exists-p (concat stem ".elc"))
                (elnix--compile-to-cache f stem)))
          (error
           (setq failures (1+ failures))
           (message "elnix: %s: %s" f (error-message-string err)))))
      (setq i (1+ i)))
    failures))

(defun elnix-load-module (file)
  "The module function of the Nix FILE, compiling it when needed."
  (if (not elnix-byte-compile)
      (eval (elnix-compile-file-form file) t)
    (let* ((stem (elnix--module-cache-file file))
           (elc (concat stem ".elc")))
      (unless (file-exists-p elc)
        (condition-case nil
            (elnix--compile-to-cache file stem)
          ;; An unwritable cache costs speed, not the evaluation: the
          ;; module is compiled in memory instead.
          (file-error nil)))
      (if (not (file-exists-p elc))
          (let ((byte-compile-warnings nil) (inhibit-message t))
            (byte-compile (elnix-compile-file-form file)))
        (let ((elnix--loaded-module nil)
              (load-force-doc-strings nil))
          (load elc nil t t)
          elnix--loaded-module)))))

;;;; Import

(defun elnix--resolve-import (path)
  "The file `import PATH' reads."
  (let ((attrs (file-attributes path)))
    (unless attrs
      (elnix-fail "path '%s' does not exist" path))
    (if (eq (file-attribute-type attrs) t)
        (concat path (if (string-suffix-p "/" path) "" "/") "default.nix")
      path)))

(defun elnix-import-path (path)
  "The value of the Nix file at the absolute PATH, as `import' gives it."
  (let ((file (elnix--resolve-import path)))
    (let ((cached (gethash file elnix--import-cache 'elnix--missing)))
      (if (not (eq cached 'elnix--missing))
          cached
        (when (and (string-prefix-p (concat elnix-store-dir "/") file)
                   (string-suffix-p ".drv" file))
          (elnix-fail "cannot import '%s': importing derivations is not supported"
                      file))
        (let* ((fn (elnix-load-module file))
               (v (elnix-force (funcall fn (directory-file-name
                                            (file-name-directory file))
                                        file))))
          (puthash file v elnix--import-cache)
          v)))))

;;;; Evaluation

(defun elnix-eval-string (src &optional dir name)
  "Evaluate the Nix expression SRC.
Relative paths in SRC resolve against DIR, the current directory by
default; NAME names the source in error messages."
  (let* ((parsed (elnix-parse-string src name))
         (form (elnix-compile-module (car parsed) (cdr parsed) name))
         (fn (if elnix-byte-compile
                 (let ((byte-compile-warnings nil)
                       (inhibit-message t))
                   (byte-compile form))
               (eval form t))))
    (elnix-force (funcall fn (directory-file-name
                              (expand-file-name (or dir default-directory)))
                          (or name "«string»")))))

(defun elnix-eval-file (file)
  "Evaluate the Nix FILE."
  (elnix-import-path (elnix-canon-path (expand-file-name file))))

(defun elnix-reset ()
  "Forget every cached import, derivation and store path."
  (clrhash elnix--import-cache)
  (clrhash elnix-derivation--drvs)
  (clrhash elnix-derivation--hash-cache)
  (clrhash elnix-derivation--text)
  (clrhash elnix-store--pending)
  (clrhash elnix-store--references)
  (clrhash elnix-store--copy-cache)
  (setq elnix--derivation-value nil)
  (elnix-reset-builtins-cache))

;;;; The derivation builtin

(defvar elnix--derivation-value nil)

(defun elnix--derivation-builtin ()
  "The value of `builtins.derivation', defined in corepkgs/derivation.nix."
  (or elnix--derivation-value
      (setq elnix--derivation-value
            (elnix-import-path (expand-file-name "derivation.nix"
                                                 elnix-corepkgs-dir)))))

(setq elnix-derivation-function #'elnix--derivation-builtin)

(provide 'elnix)

;;; elnix.el ends here
