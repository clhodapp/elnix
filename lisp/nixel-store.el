;;; nixel-store.el --- Store paths, hashes and NAR serialisation -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Evaluation never writes to the store.  Copying a path to the store
;; computes the path the copy would have, from the NAR serialisation of
;; the source, and records the copy as pending; `toFile' and the `.drv'
;; files of derivations are pending text paths.  The records are kept
;; in `nixel-store--pending' for a later step to add to a real store.
;;
;; The algorithms are the ones Nix uses: the base-32 alphabet that
;; omits e, o, t and u, the folding of a SHA-256 digest to 160 bits,
;; and the `TYPE:sha256:HEX:STOREDIR:NAME' fingerprint a store path
;; hashes.

;;; Code:

(require 'cl-lib)
(require 'nixel-runtime)

(defvar nixel-store-dir)

(defvar nixel-store--pending (make-hash-table :test 'equal)
  "The store paths computed during evaluation: path to (KIND . DATA).
KIND is `text' with DATA (CONTENTS . REFERENCES), or `source' with DATA
\(SOURCE-PATH FILTER-P RECURSIVE).")

(defvar nixel-store--references (make-hash-table :test 'equal)
  "The references of the store paths evaluation produced: path to list.")

(defvar nixel-store--copy-cache (make-hash-table :test 'equal)
  "The store paths of source paths copied without a filter.")

;;;; Hashes

(defconst nixel-store--base32-chars "0123456789abcdfghijklmnpqrsvwxyz")

(defun nixel-store-base32 (bytes)
  "BYTES, a unibyte string, in the base-32 encoding Nix uses."
  (let* ((n (length bytes))
         (len (1+ (/ (1- (* n 8)) 5)))
         (out (make-string len ?0)))
    (dotimes (k len)
      (let* ((i (- len 1 k))
             (b (* i 5))
             (byte (/ b 8))
             (bit (% b 8))
             (c (logior (ash (aref bytes byte) (- bit))
                        (if (>= byte (1- n)) 0
                          (ash (aref bytes (1+ byte)) (- 8 bit))))))
        (aset out k (aref nixel-store--base32-chars (logand c 31)))))
    out))

(defun nixel-store-base32-decode (s size)
  "Decode the base-32 string S into SIZE bytes, or nil when S is invalid."
  (let ((bytes (make-string size 0))
        (len (length s))
        (ok t))
    (dotimes (k len)
      (let* ((c (aref s k))
             (digit (cl-position c nixel-store--base32-chars)))
        (if (null digit)
            (setq ok nil)
          (let* ((n (- len 1 k))
                 (b (* n 5))
                 (i (/ b 8))
                 (j (% b 8)))
            (when (< i size)
              (aset bytes i (logand 255 (logior (aref bytes i) (ash digit j)))))
            (if (< (1+ i) size)
                (aset bytes (1+ i)
                      (logand 255 (logior (aref bytes (1+ i)) (ash digit (- j 8)))))
              (when (/= 0 (ash digit (- j 8))) (setq ok nil)))))))
    (and ok (string-to-unibyte bytes))))

(defun nixel-store-hex (bytes)
  "BYTES in lowercase hexadecimal."
  (mapconcat (lambda (b) (format "%02x" b)) bytes ""))

(defun nixel-store-unhex (s)
  "The bytes of the hexadecimal string S, or nil."
  (and (= 0 (% (length s) 2))
       (string-match-p "\\`[0-9a-fA-F]*\\'" s)
       (let ((out (make-string (/ (length s) 2) 0)))
         (dotimes (i (/ (length s) 2))
           (aset out i (string-to-number (substring s (* 2 i) (+ 2 (* 2 i))) 16)))
         (string-to-unibyte out))))

(defconst nixel-store--hash-sizes
  '(("md5" . 16) ("sha1" . 20) ("sha256" . 32) ("sha512" . 64)))

(defun nixel-store--algo-symbol (algo)
  (pcase algo
    ("md5" 'md5) ("sha1" 'sha1) ("sha256" 'sha256) ("sha512" 'sha512)
    (_ (nixel-fail "unknown hash algorithm '%s', expect 'md5', 'sha1', 'sha256', or 'sha512'"
                   algo))))

(defun nixel-store-hash-bytes (algo s)
  "The ALGO digest of the unibyte string S, as bytes."
  (secure-hash (nixel-store--algo-symbol algo) (nixel--unibyte s) nil nil t))

(defun nixel-hash-string (algo s)
  "The hexadecimal ALGO digest of S, for `builtins.hashString'."
  (nixel-store-hex (nixel-store-hash-bytes algo s)))

(defun nixel-store-sha256 (s) (nixel-store-hash-bytes "sha256" s))

(defun nixel-store-compress-hash (bytes size)
  "Fold BYTES into SIZE bytes by xor, as compressHash in Nix does."
  (let ((out (make-string size 0)))
    (dotimes (i (length bytes))
      (aset out (% i size) (logxor (aref out (% i size)) (aref bytes i))))
    (string-to-unibyte out)))

(defun nixel-store--base64-decode (s)
  "Decode the base-64 string S as Nix does: padding may be missing, and
anything after the padding is ignored."
  (let* ((eq (string-search "=" s))
         (body (if eq (substring s 0 eq) s))
         (padded (concat body (make-string (% (- 4 (% (length body) 4)) 4) ?=))))
    (condition-case nil (base64-decode-string padded) (error nil))))

(defun nixel-store-parse-hash (s algo)
  "Parse the hash string S of ALGO (nil when S names it).
S may be base-16, Nix base-32, base-64, `ALGO:...' or SRI `ALGO-BASE64'.
Return (ALGO . BYTES)."
  (let ((rest s) (a algo) (sri nil))
    (cond
     ((string-match "\\`\\(md5\\|sha1\\|sha256\\|sha512\\):\\(.*\\)\\'" s)
      (setq a (match-string 1 s) rest (match-string 2 s)))
     ((string-match "\\`\\(md5\\|sha1\\|sha256\\|sha512\\)-\\(.*\\)\\'" s)
      (setq a (match-string 1 s) rest (match-string 2 s) sri t)))
    (when (and algo a (not (string= algo a)))
      (nixel-fail "hash '%s' should have type '%s'" s algo))
    (unless a
      (nixel-fail "hash '%s' does not include a type, nor is the type otherwise known from context"
                  s))
    (let* ((size (cdr (assoc a nixel-store--hash-sizes)))
           (bytes
            (cond
             (sri (let ((b (nixel-store--base64-decode rest)))
                    (and b (= (length b) size) b)))
             ((= (length rest) (* 2 size)) (nixel-store-unhex rest))
             ((= (length rest) (1+ (/ (1- (* size 8)) 5)))
              (nixel-store-base32-decode rest size))
             ((= (length rest) (* 4 (/ (+ size 2) 3)))
              (let ((b (nixel-store--base64-decode rest)))
                (and b (= (length b) size) b)))
             (t nil))))
      (unless bytes
        (nixel-fail "hash '%s' has wrong length for hash type '%s'" s a))
      (cons a bytes))))

(defun nixel-store-format-hash (algo bytes format &optional with-prefix)
  "BYTES of ALGO in FORMAT: base16, nix32, base32, base64 or sri."
  (pcase format
    ("sri" (concat algo "-" (base64-encode-string bytes t)))
    (_ (concat (if with-prefix (concat algo ":") "")
               (pcase format
                 ("base16" (nixel-store-hex bytes))
                 ((or "nix32" "base32") (nixel-store-base32 bytes))
                 ("base64" (base64-encode-string bytes t))
                 (_ (nixel-fail "unknown hash format '%s', expect 'base16', 'base32', 'base64', or 'sri'"
                                format)))))))

(defun nixel-convert-hash (hash algo to-format)
  "`builtins.convertHash' of HASH with ALGO (or nil) into TO-FORMAT."
  (let ((parsed (nixel-store-parse-hash hash algo)))
    (nixel-store-format-hash (car parsed) (cdr parsed) to-format)))

;;;; Store paths

(defun nixel-store--check-name (name)
  (when (or (= (length name) 0) (> (length name) 211))
    (nixel-fail "invalid store path name '%s'" name))
  (when (eq (aref name 0) ?.)
    (nixel-fail "store path name '%s' starts with a period" name))
  (unless (string-match-p "\\`[a-zA-Z0-9+._?=-]+\\'" name)
    (nixel-fail "store path name '%s' contains an invalid character" name)))

(defun nixel-store-make-path (type hash-bytes name)
  "The store path of TYPE for the SHA-256 HASH-BYTES and NAME."
  (nixel-store--check-name name)
  (let* ((fingerprint (concat type ":sha256:" (nixel-store-hex hash-bytes) ":"
                              nixel-store-dir ":" name))
         (digest (nixel-store-compress-hash (nixel-store-sha256 fingerprint) 20)))
    (concat nixel-store-dir "/" (nixel-store-base32 digest) "-" name)))

(defun nixel-store--make-type (type refs &optional self)
  (concat type
          (mapconcat (lambda (r) (concat ":" r)) (sort (copy-sequence refs) #'string<) "")
          (if self ":self" "")))

(defun nixel-store-text-path (name contents refs)
  "The store path of a text file NAME with CONTENTS referring to REFS."
  (nixel-store-make-path (nixel-store--make-type "text" refs)
                         (nixel-store-sha256 contents) name))

(defun nixel-store-fixed-path (name method algo bytes &optional refs)
  "The store path of fixed-output NAME: METHOD `flat' or `recursive'."
  (if (and (eq method 'recursive) (string= algo "sha256"))
      (nixel-store-make-path (nixel-store--make-type "source" refs)
                             bytes name)
    (nixel-store-make-path
     "output:out"
     (nixel-store-sha256 (concat "fixed:out:" (if (eq method 'recursive) "r:" "")
                                 algo ":" (nixel-store-hex bytes) ":"))
     name)))

(defun nixel-store-output-path (output hash-bytes drv-name)
  "The store path of OUTPUT of an input-addressed derivation."
  (nixel-store-make-path (concat "output:" output) hash-bytes
                         (if (string= output "out") drv-name
                           (concat drv-name "-" output))))

(defun nixel-store-placeholder (output)
  "The placeholder string standing for OUTPUT of the derivation being built."
  (concat "/" (nixel-store-base32
               (nixel-store-sha256 (concat "nix-output:" output)))))

(defun nixel-store-add-text (name contents refs)
  "Record a text file and return its store path."
  (let ((p (nixel-store-text-path name contents refs)))
    (puthash p (cons 'text (cons contents refs)) nixel-store--pending)
    (puthash p refs nixel-store--references)
    p))

;;;; NAR serialisation

(defun nixel-store--nar-str (s)
  (let* ((n (length s))
         (pad (% (- 8 (% n 8)) 8)))
    (concat (nixel-store--u64 n) s (make-string pad 0))))

(defun nixel-store--u64 (n)
  (let ((out (make-string 8 0)))
    (dotimes (i 8)
      (aset out i (logand 255 (ash n (* -8 i)))))
    (string-to-unibyte out)))

(defun nixel-store--file-name-bytes (name)
  (if (multibyte-string-p name)
      (encode-coding-string name 'utf-8-unix t)
    name))

(defun nixel-store--nar-node (path filter out)
  "Push the NAR serialisation of PATH onto OUT, a list of strings, reversed.
FILTER, when non-nil, decides for each entry under PATH whether it is
included."
  (let* ((attrs (file-attributes path))
         (type (file-attribute-type attrs)))
    (push (nixel-store--nar-str "(") out)
    (cond
     ((stringp type)
      (push (nixel-store--nar-str "type") out)
      (push (nixel-store--nar-str "symlink") out)
      (push (nixel-store--nar-str "target") out)
      (push (nixel-store--nar-str (nixel-store--file-name-bytes type)) out))
     ((eq type t)
      (push (nixel-store--nar-str "type") out)
      (push (nixel-store--nar-str "directory") out)
      (let ((names (mapcar (lambda (n) (cons (nixel-store--file-name-bytes n) n))
                           (directory-files path nil
                                            directory-files-no-dot-files-regexp t))))
        (setq names (sort names (lambda (a b) (string< (car a) (car b)))))
        (dolist (n names)
          (let ((child (concat path "/" (cdr n))))
            (when (or (null filter) (funcall filter child))
              (push (nixel-store--nar-str "entry") out)
              (push (nixel-store--nar-str "(") out)
              (push (nixel-store--nar-str "name") out)
              (push (nixel-store--nar-str (car n)) out)
              (push (nixel-store--nar-str "node") out)
              (setq out (nixel-store--nar-node child filter out))
              (push (nixel-store--nar-str ")") out))))))
     (t
      (push (nixel-store--nar-str "type") out)
      (push (nixel-store--nar-str "regular") out)
      (when (/= 0 (logand (file-modes path 'nofollow) #o100))
        (push (nixel-store--nar-str "executable") out)
        (push (nixel-store--nar-str "") out))
      (push (nixel-store--nar-str "contents") out)
      (push (nixel-store--nar-str
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents-literally path)
               (buffer-string)))
            out)))
    (push (nixel-store--nar-str ")") out)
    out))

(defun nixel-store-nar (path &optional filter)
  "The NAR serialisation of PATH as a unibyte string."
  (unless (file-attributes path)
    (nixel-fail "path '%s' does not exist" path))
  (apply #'concat
         (nreverse (nixel-store--nar-node
                    path filter
                    (list (nixel-store--nar-str "nix-archive-1"))))))

(defun nixel-store-nar-hash (path &optional filter)
  "The SHA-256 of the NAR of PATH."
  (nixel-store-sha256 (nixel-store-nar path filter)))

;;;; Copying sources

(defun nixel-store-copy-path (path)
  "The store path of a copy of PATH, as interpolating a path makes one."
  (or (gethash path nixel-store--copy-cache)
      (let ((name (let ((i (cl-position ?/ path :from-end t)))
                    (substring path (1+ i)))))
        (when (string-suffix-p ".drv" name)
          (nixel-fail "file names are not allowed to end in '.drv'"))
        (let ((p (nixel-store-make-path "source" (nixel-store-nar-hash path) name)))
          (puthash p (list 'source path nil t) nixel-store--pending)
          (puthash p nil nixel-store--references)
          (puthash path p nixel-store--copy-cache)))))

(defun nixel-store-add-filtered (path name filter recursive expected)
  "The store path of PATH copied as NAME through the Nix function FILTER.
RECURSIVE nil copies a single file by its contents.  EXPECTED, a hash
string, is checked against the result when given."
  (let* ((pred (and filter
                    (lambda (child)
                      (let ((ty (let ((attrs (file-attributes child)))
                                  (cond ((eq (file-attribute-type attrs) t) "directory")
                                        ((stringp (file-attribute-type attrs)) "symlink")
                                        (t "regular")))))
                        (nixel-force-bool
                         (nixel-app (nixel-app filter
                                               (nixel-store--file-name-bytes child))
                                    ty))))))
         (p (if recursive
                (let ((h (nixel-store-nar-hash path pred)))
                  (when expected
                    (let ((want (cdr (nixel-store-parse-hash expected "sha256"))))
                      (unless (string= want h)
                        (nixel-fail "hash mismatch in file downloaded from '%s'" path))))
                  (nixel-store-make-path "source" h name))
              (let ((h (nixel-store-sha256
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string)))))
                (nixel-store-fixed-path name 'flat "sha256" h)))))
    (puthash p (list 'source path (and filter t) recursive) nixel-store--pending)
    (puthash p nil nixel-store--references)
    p))

(provide 'nixel-store)

;;; nixel-store.el ends here
