;;; nixel.el --- Nix compiled to Emacs Lisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;; Author: Chris Hodapp
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, nix

;;; Commentary:

;; nixel compiles Nix files to Emacs Lisp and evaluates them, with the
;; derivations Nix itself would compute.  Each Nix file compiles to one
;; Lisp function of its directory and name, byte-compiled and cached by
;; the SHA-256 of its source; `import' loads the compiled function,
;; calls it once and keeps the value, as Nix keeps the value of every
;; file it imports.
;;
;; Entry points: `nixel-eval-string', `nixel-eval-file' and
;; `nixel-import-path'.  `nixel-cli' (bin/nixel) is the command line.

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'nixel-parse)
(require 'nixel-runtime)
(require 'nixel-compile)
(require 'nixel-builtins)
(require 'nixel-store)
(require 'nixel-derivation)
(require 'nixel-json)
(require 'nixel-regex)
(require 'nixel-print)
(require 'nixel-toml)
(require 'nixel-xml)

(defconst nixel-version "0.1.0")

(defconst nixel--lisp-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory holding nixel's Lisp files.")

(setq nixel-corepkgs-dir
      (expand-file-name "../corepkgs/" nixel--lisp-dir))

(defvar nixel-cache-dir
  (or (getenv "NIXEL_CACHE_DIR")
      (expand-file-name "nixel" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
  "Where compiled modules are cached.")

(defvar nixel-byte-compile t
  "Non-nil to byte-compile modules; nil interprets the compiled forms.")

(defvar nixel--import-cache (make-hash-table :test 'equal)
  "The values of imported files by canonical path.")

(defvar nixel--loaded-module nil
  "Set by a compiled module file when it is loaded.")

(defconst nixel--compiler-files
  '("nixel-parse.el" "nixel-compile.el")
  "The files whose contents determine the compiled form of a module.")

(defvar nixel--compiler-digest nil)

(defun nixel--compiler-digest ()
  "A digest of the compiler, so a changed compiler invalidates the cache."
  (or nixel--compiler-digest
      (setq nixel--compiler-digest
            (secure-hash 'sha256
                         (mapconcat (lambda (f)
                                      (nixel-read-file-bytes
                                       (expand-file-name f nixel--lisp-dir)))
                                    nixel--compiler-files "")))))

;;;; Compiling modules

(defun nixel-compile-file-form (file)
  "The module form of the Nix FILE."
  (let ((parsed (nixel-parse-file file)))
    (nixel-compile-module (car parsed) (cdr parsed) file)))

(defun nixel--write-module (form el-file)
  "Write the module FORM to EL-FILE as a loadable Lisp file."
  (make-directory (file-name-directory el-file) t)
  (with-temp-file el-file
    (set-buffer-multibyte nil)
    (insert ";;; -*- lexical-binding: t; coding: raw-text-unix -*-\n")
    (let ((print-escape-newlines t)
          (print-escape-control-characters t)
          (print-circle nil)
          (print-length nil)
          (print-level nil))
      (prin1 `(setq nixel--loaded-module ,form) (current-buffer)))
    (insert "\n")))

(defun nixel--byte-compile-file (el-file)
  (let ((byte-compile-warnings nil)
        (byte-compile-verbose nil)
        (inhibit-message t)
        (message-log-max nil)
        (max-lisp-eval-depth (max max-lisp-eval-depth 100000)))
    (unless (byte-compile-file el-file)
      (error "nixel: byte compilation of %s failed" el-file))))

(defun nixel--module-cache-file (file)
  "The cache file stem for the compiled form of the Nix FILE."
  (let ((key (secure-hash 'sha256
                          (concat (nixel--compiler-digest) "\0" file "\0"
                                  (nixel-read-file-bytes file)))))
    (expand-file-name (concat (substring key 0 2) "/" (substring key 2))
                      nixel-cache-dir)))

(defun nixel-load-module (file)
  "The module function of the Nix FILE, compiling it when needed."
  (if (not nixel-byte-compile)
      (eval (nixel-compile-file-form file) t)
    (let* ((stem (nixel--module-cache-file file))
           (el (concat stem ".el"))
           (elc (concat stem ".elc")))
      (unless (file-exists-p elc)
        (nixel--write-module (nixel-compile-file-form file) el)
        (nixel--byte-compile-file el)
        (delete-file el))
      (let ((nixel--loaded-module nil)
            (load-force-doc-strings nil))
        (load elc nil t t)
        nixel--loaded-module))))

;;;; Import

(defun nixel--resolve-import (path)
  "The file `import PATH' reads."
  (let ((attrs (file-attributes path)))
    (unless attrs
      (nixel-fail "path '%s' does not exist" path))
    (if (eq (file-attribute-type attrs) t)
        (concat path (if (string-suffix-p "/" path) "" "/") "default.nix")
      path)))

(defun nixel-import-path (path)
  "The value of the Nix file at the absolute PATH, as `import' gives it."
  (let ((file (nixel--resolve-import path)))
    (let ((cached (gethash file nixel--import-cache 'nixel--missing)))
      (if (not (eq cached 'nixel--missing))
          cached
        (when (and (string-prefix-p (concat nixel-store-dir "/") file)
                   (string-suffix-p ".drv" file))
          (nixel-fail "cannot import '%s': importing derivations is not supported"
                      file))
        (let* ((fn (nixel-load-module file))
               (v (nixel-force (funcall fn (directory-file-name
                                            (file-name-directory file))
                                        file))))
          (puthash file v nixel--import-cache)
          v)))))

;;;; Evaluation

(defun nixel-eval-string (src &optional dir name)
  "Evaluate the Nix expression SRC.
Relative paths in SRC resolve against DIR, the current directory by
default; NAME names the source in error messages."
  (let* ((parsed (nixel-parse-string src name))
         (form (nixel-compile-module (car parsed) (cdr parsed) name))
         (fn (if nixel-byte-compile
                 (let ((byte-compile-warnings nil)
                       (inhibit-message t))
                   (byte-compile form))
               (eval form t))))
    (nixel-force (funcall fn (directory-file-name
                              (expand-file-name (or dir default-directory)))
                          (or name "«string»")))))

(defun nixel-eval-file (file)
  "Evaluate the Nix FILE."
  (nixel-import-path (nixel-canon-path (expand-file-name file))))

(defun nixel-reset ()
  "Forget every cached import, derivation and store path."
  (clrhash nixel--import-cache)
  (clrhash nixel-derivation--drvs)
  (clrhash nixel-derivation--hash-cache)
  (clrhash nixel-derivation--text)
  (clrhash nixel-store--pending)
  (clrhash nixel-store--references)
  (clrhash nixel-store--copy-cache)
  (setq nixel--derivation-value nil)
  (nixel-reset-builtins-cache))

;;;; The derivation builtin

(defvar nixel--derivation-value nil)

(defun nixel--derivation-builtin ()
  "The value of `builtins.derivation', defined in corepkgs/derivation.nix."
  (or nixel--derivation-value
      (setq nixel--derivation-value
            (nixel-import-path (expand-file-name "derivation.nix"
                                                 nixel-corepkgs-dir)))))

(setq nixel-derivation-function #'nixel--derivation-builtin)

(provide 'nixel)

;;; nixel.el ends here
