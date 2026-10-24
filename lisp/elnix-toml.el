;;; elnix-toml.el --- builtins.fromTOML -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A TOML v1.0 parser for `builtins.fromTOML'.  It reads the raw UTF-8
;; bytes of a document and returns the Nix value that Nix 2.34 builds
;; from it: a table becomes an attribute set, an array becomes a list,
;; and strings, integers, floats and booleans become the Nix values of
;; the same kind.
;;
;; An offset date-time, a local date-time, a local date or a local
;; time is an error unless `elnix-toml-timestamps' is set, as it is an
;; error in Nix without the parse-toml-timestamps experimental feature.
;; With the variable set, each becomes the attribute set
;; { _type = "timestamp"; value = TEXT; }, where TEXT is the normalised
;; form that Nix prints.  A string that holds a null byte is an error,
;; since a Nix string cannot hold one.
;;
;; The parser builds the document as mutable tables first, because a
;; table header can reopen a table that an earlier header created.
;; Each table records how it came to exist:
;;
;;   implicit  created as the parent of a table named by a header
;;   header    defined by a [table] header, or an element of an array
;;             of tables, or the root table
;;   dotted    created by a dotted key in a key/value pair
;;   inline    written as an inline table, and closed to any addition
;;
;; Those kinds decide which later definitions TOML allows.  The finished
;; tables are converted to Nix values once the whole document is read.

;;; Code:

(require 'cl-lib)
(require 'elnix-runtime)

(defvar elnix-toml-timestamps nil
  "Whether `elnix-from-toml' turns dates and times into values.
Nil makes a date or a time an error, as Nix does without the
parse-toml-timestamps experimental feature.  Non-nil makes each one
the set { _type = \"timestamp\"; value = TEXT; }, as Nix does with
the feature.")

(defvar elnix-toml--s ""
  "The unibyte TOML document being parsed.")

(defvar elnix-toml--i 0
  "The byte offset of the parser in `elnix-toml--s'.")

(defvar elnix-toml--n 0
  "The length of `elnix-toml--s'.")

(cl-defstruct (elnix-toml--table
               (:constructor elnix-toml--table-make (kind))
               (:copier nil))
  "A table under construction.
KIND is one of `implicit', `header', `dotted' and `inline'.  MAP
holds the entries from key strings to values under construction."
  kind
  (map (make-hash-table :test 'equal)))

(cl-defstruct (elnix-toml--aot
               (:constructor elnix-toml--aot-make (tables))
               (:copier nil))
  "An array of tables under construction.
TABLES holds its tables, the last one first."
  tables)

;; A static array is a vector of values under construction.

;;;; Errors and positions

(defun elnix-toml--fail (fmt &rest args)
  "Signal a TOML parse error with the message FMT and ARGS."
  (let ((line (1+ (cl-count ?\n elnix-toml--s
                            :end (min elnix-toml--i elnix-toml--n)))))
    (elnix-fail "while parsing TOML: %s (line number %d)"
                (apply #'format-message fmt args) line)))

(defsubst elnix-toml--peek (&optional k)
  "The byte K bytes past the parser, or nil past the end."
  (let ((j (+ elnix-toml--i (or k 0))))
    (and (< j elnix-toml--n) (aref elnix-toml--s j))))

(defsubst elnix-toml--advance (&optional k)
  "Move the parser K bytes forward, one when K is nil."
  (setq elnix-toml--i (+ elnix-toml--i (or k 1))))

(defsubst elnix-toml--control-p (c)
  "Whether the byte C is a control character TOML text may not hold.
A tab is allowed everywhere text is."
  (or (and (< c #x20) (/= c ?\t)) (= c #x7f)))

(defun elnix-toml--newline ()
  "The length of the newline at the parser, or nil when there is none."
  (let ((c (elnix-toml--peek)))
    (cond ((eq c ?\n) 1)
          ((and (eq c ?\r) (eq (elnix-toml--peek 1) ?\n)) 2))))

(defun elnix-toml--utf8-char ()
  "Move past the UTF-8 sequence at the parser.
Fail unless the bytes there encode one Unicode scalar value in its
shortest form."
  (let* ((b (elnix-toml--peek))
         (len (cond ((<= #xc2 b #xdf) 2)
                    ((<= #xe0 b #xef) 3)
                    ((<= #xf0 b #xf4) 4)))
         (lo (pcase b (#xe0 #xa0) (#xf0 #x90) (_ #x80)))
         (hi (pcase b (#xed #x9f) (#xf4 #x8f) (_ #xbf))))
    (unless len (elnix-toml--fail "invalid UTF-8 byte"))
    (dotimes (k (1- len))
      (let ((c (elnix-toml--peek (1+ k))))
        (unless (and c (if (= k 0) (<= lo c hi) (<= #x80 c #xbf)))
          (elnix-toml--fail "invalid UTF-8 sequence"))))
    (elnix-toml--advance len)))

(defun elnix-toml--utf8 (cp)
  "The unibyte string of the UTF-8 encoding of the code point CP."
  (cond
   ((< cp #x80) (unibyte-string cp))
   ((< cp #x800) (unibyte-string (logior #xc0 (ash cp -6))
                                 (logior #x80 (logand cp #x3f))))
   ((< cp #x10000) (unibyte-string (logior #xe0 (ash cp -12))
                                   (logior #x80 (logand (ash cp -6) #x3f))
                                   (logior #x80 (logand cp #x3f))))
   (t (unibyte-string (logior #xf0 (ash cp -18))
                      (logior #x80 (logand (ash cp -12) #x3f))
                      (logior #x80 (logand (ash cp -6) #x3f))
                      (logior #x80 (logand cp #x3f))))))

;;;; Whitespace, comments and line ends

(defun elnix-toml--skip-ws ()
  "Move past spaces and tabs."
  (while (memq (elnix-toml--peek) '(?\s ?\t))
    (elnix-toml--advance)))

(defun elnix-toml--skip-comment ()
  "Move past the comment at the parser, up to the end of its line.
Do nothing when no comment starts at the parser."
  (when (eq (elnix-toml--peek) ?#)
    (elnix-toml--advance)
    (let (c)
      (while (and (setq c (elnix-toml--peek))
                  (not (elnix-toml--newline)))
        (cond ((>= c #x80) (elnix-toml--utf8-char))
              ((elnix-toml--control-p c)
               (elnix-toml--fail "control character in a comment"))
              (t (elnix-toml--advance)))))))

(defun elnix-toml--end-of-line ()
  "Move past trailing whitespace, a comment and the end of the line.
Fail when something else follows on the line."
  (elnix-toml--skip-ws)
  (elnix-toml--skip-comment)
  (when (< elnix-toml--i elnix-toml--n)
    (let ((nl (elnix-toml--newline)))
      (unless nl (elnix-toml--fail "expected the end of the line"))
      (elnix-toml--advance nl))))

(defun elnix-toml--skip-blank ()
  "Move past whitespace, comments and newlines."
  (let ((done nil))
    (while (not done)
      (elnix-toml--skip-ws)
      (elnix-toml--skip-comment)
      (let ((nl (elnix-toml--newline)))
        (if nl (elnix-toml--advance nl) (setq done t))))))

;;;; Strings

(defun elnix-toml--hex-escape (len)
  "The UTF-8 bytes of the escape of LEN hex digits at the parser."
  (let ((cp 0))
    (dotimes (j len)
      (let* ((c (elnix-toml--peek j))
             (d (and c (cond ((<= ?0 c ?9) (- c ?0))
                             ((<= ?a c ?f) (- c (- ?a 10)))
                             ((<= ?A c ?F) (- c (- ?A 10)))))))
        (unless d (elnix-toml--fail "invalid unicode escape"))
        (setq cp (+ (* cp 16) d))))
    (when (or (> cp #x10ffff) (<= #xd800 cp #xdfff))
      (elnix-toml--fail "escape is not a Unicode scalar value"))
    (elnix-toml--advance len)
    (elnix-toml--utf8 cp)))

(defun elnix-toml--escape (multi)
  "The bytes of the escape after the backslash at the parser.
With MULTI, the string is a multi-line basic string, where a
backslash at the end of a line removes the line end and all
whitespace after it; the result is then the empty string."
  (let ((e (elnix-toml--peek)))
    (if (and multi (memq e '(?\s ?\t ?\n ?\r)))
        (progn
          (elnix-toml--skip-ws)
          (unless (elnix-toml--newline)
            (elnix-toml--fail "invalid escape in a string"))
          (let ((done nil))
            (while (not done)
              (let ((nl (elnix-toml--newline)))
                (cond (nl (elnix-toml--advance nl))
                      ((memq (elnix-toml--peek) '(?\s ?\t))
                       (elnix-toml--advance))
                      (t (setq done t))))))
          "")
      (elnix-toml--advance)
      (pcase e
        (?b "\b") (?t "\t") (?n "\n") (?f "\f") (?r "\r")
        (?\" "\"") (?\\ "\\")
        (?u (elnix-toml--hex-escape 4))
        (?U (elnix-toml--hex-escape 8))
        (_ (elnix-toml--fail "invalid escape in a string"))))))

(defun elnix-toml--string ()
  "The unibyte string of the string literal at the parser.
The literal is basic or literal, single-line or multi-line, as its
opening delimiter says."
  (let* ((q (elnix-toml--peek))
         (basic (eq q ?\"))
         (multi (and (eq (elnix-toml--peek 1) q) (eq (elnix-toml--peek 2) q)))
         (out nil)
         (start nil)
         (done nil))
    (elnix-toml--advance (if multi 3 1))
    (when multi
      (let ((nl (elnix-toml--newline)))
        (when nl (elnix-toml--advance nl))))
    (setq start elnix-toml--i)
    (cl-flet ((flush ()
                (when (> elnix-toml--i start)
                  (push (substring elnix-toml--s start elnix-toml--i) out))))
      (while (not done)
        (let ((c (elnix-toml--peek)))
          (cond
           ((null c) (elnix-toml--fail "unterminated string"))
           ((eq c q)
            (if (not multi)
                (progn (flush) (elnix-toml--advance) (setq done t))
              (let ((k 0))
                (while (eq (elnix-toml--peek k) q) (setq k (1+ k)))
                (cond
                 ((< k 3) (elnix-toml--advance k))
                 ((> k 5) (elnix-toml--fail "too many quotes in a string"))
                 (t (elnix-toml--advance (- k 3))
                    (flush)
                    (elnix-toml--advance 3)
                    (setq done t))))))
           ((and basic (eq c ?\\))
            (flush)
            (elnix-toml--advance)
            (push (elnix-toml--escape multi) out)
            (setq start elnix-toml--i))
           ((elnix-toml--newline)
            (unless multi (elnix-toml--fail "newline in a single-line string"))
            (elnix-toml--advance (elnix-toml--newline)))
           ((>= c #x80) (elnix-toml--utf8-char))
           ((elnix-toml--control-p c)
            (elnix-toml--fail "control character in a string"))
           (t (elnix-toml--advance))))))
    (let ((s (apply #'concat (nreverse out))))
      (when (string-search "\0" s)
        (elnix-toml--fail "string contains a null byte"))
      s)))

;;;; Keys

(defun elnix-toml--bare-key-char-p (c)
  "Whether the byte C may appear in a bare key."
  (and c (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9) (eq c ?_) (eq c ?-))))

(defun elnix-toml--simple-key ()
  "The string of the bare or quoted key at the parser."
  (let ((c (elnix-toml--peek)))
    (cond
     ((memq c '(?\" ?'))
      (when (and (eq (elnix-toml--peek 1) c) (eq (elnix-toml--peek 2) c))
        (elnix-toml--fail "a multi-line string cannot be a key"))
      (elnix-toml--string))
     ((elnix-toml--bare-key-char-p c)
      (let ((start elnix-toml--i))
        (while (elnix-toml--bare-key-char-p (elnix-toml--peek))
          (elnix-toml--advance))
        (substring elnix-toml--s start elnix-toml--i)))
     (t (elnix-toml--fail "expected a key")))))

(defun elnix-toml--key ()
  "The list of the parts of the dotted key at the parser.
Whitespace around the key and around its dots is skipped."
  (let ((parts nil) (done nil))
    (while (not done)
      (elnix-toml--skip-ws)
      (push (elnix-toml--simple-key) parts)
      (elnix-toml--skip-ws)
      (if (eq (elnix-toml--peek) ?.)
          (elnix-toml--advance)
        (setq done t)))
    (nreverse parts)))

(defun elnix-toml--key-text (keys)
  "The dotted key KEYS as text for an error message."
  (mapconcat #'identity keys "."))

;;;; Values

(defun elnix-toml--digits-p (k count)
  "Whether the COUNT bytes K bytes past the parser are all digits."
  (let ((ok t))
    (dotimes (j count)
      (let ((c (elnix-toml--peek (+ k j))))
        (unless (and c (<= ?0 c ?9)) (setq ok nil))))
    ok))

(defun elnix-toml--word-p (w)
  "Whether the text at the parser starts with the word W."
  (eq t (compare-strings elnix-toml--s elnix-toml--i
                         (min elnix-toml--n (+ elnix-toml--i (length w)))
                         w nil nil)))

(defconst elnix-toml--dec-re
  "\\`[-+]?\\(?:0\\|[1-9]\\(?:_?[0-9]\\)*\\)\\'"
  "A decimal integer.")

(defconst elnix-toml--float-re
  (concat "\\`[-+]?\\(?:0\\|[1-9]\\(?:_?[0-9]\\)*\\)"
          "\\(?:\\.[0-9]\\(?:_?[0-9]\\)*\\)?"
          "\\(?:[eE][-+]?[0-9]\\(?:_?[0-9]\\)*\\)?\\'")
  "A decimal float; a decimal integer matches it too.")

(defun elnix-toml--integer (text digits base)
  "The integer of the DIGITS in BASE, whose literal is TEXT.
DIGITS may hold underscores and a sign."
  (let ((n (string-to-number (string-replace "_" "" digits) base)))
    (unless (and (<= n elnix--int-max) (>= n elnix--int-min))
      (elnix-toml--fail "integer %s is out of range" text))
    n))

;;;; Dates and times

(defun elnix-toml--fixed-digits (count)
  "The number of the COUNT digits at the parser, moving past them."
  (unless (elnix-toml--digits-p 0 count)
    (elnix-toml--fail "invalid date or time"))
  (prog1 (string-to-number
          (substring elnix-toml--s elnix-toml--i (+ elnix-toml--i count)))
    (elnix-toml--advance count)))

(defun elnix-toml--expect (c)
  "Move past the byte C at the parser, failing when another is there."
  (unless (eq (elnix-toml--peek) c)
    (elnix-toml--fail "invalid date or time"))
  (elnix-toml--advance))

(defun elnix-toml--date ()
  "The text of the local date at the parser, in the form YYYY-MM-DD."
  (let* ((y (prog1 (elnix-toml--fixed-digits 4) (elnix-toml--expect ?-)))
         (m (prog1 (elnix-toml--fixed-digits 2) (elnix-toml--expect ?-)))
         (d (elnix-toml--fixed-digits 2))
         (leap (and (= 0 (% y 4)) (or (/= 0 (% y 100)) (= 0 (% y 400)))))
         (days (and (<= 1 m 12)
                    (aref (if leap [31 29 31 30 31 30 31 31 30 31 30 31]
                            [31 28 31 30 31 30 31 31 30 31 30 31])
                          (1- m)))))
    (unless (and days (<= 1 d days))
      (elnix-toml--fail "month must be 01-12 and day must exist in the month"))
    (format "%04d-%02d-%02d" y m d)))

(defun elnix-toml--time ()
  "The text of the local time at the parser, as Nix prints it.
Digits of a fraction of a second past the ninth are dropped, and the
fraction is printed as milliseconds, microseconds or nanoseconds,
whichever is the shortest that holds it exactly."
  (let* ((h (prog1 (elnix-toml--fixed-digits 2) (elnix-toml--expect ?:)))
         (m (prog1 (elnix-toml--fixed-digits 2) (elnix-toml--expect ?:)))
         (s (elnix-toml--fixed-digits 2))
         (ns 0))
    (unless (and (<= h 23) (<= m 59))
      (elnix-toml--fail "hour must be 00-23 and minute must be 00-59"))
    (unless (<= s 60)
      (elnix-toml--fail "second must be 00-60"))
    (when (eq (elnix-toml--peek) ?.)
      (elnix-toml--advance)
      (let ((start elnix-toml--i))
        (while (elnix-toml--digits-p 0 1) (elnix-toml--advance))
        (when (= start elnix-toml--i)
          (elnix-toml--fail "expected digits after `.' in a time"))
        (let ((digits (substring elnix-toml--s start
                                 (min elnix-toml--i (+ start 9)))))
          (setq ns (string-to-number
                    (concat digits (make-string (- 9 (length digits)) ?0)))))))
    (concat (format "%02d:%02d:%02d" h m s)
            (cond ((= ns 0) "")
                  ((= 0 (% ns 1000000)) (format ".%03d" (/ ns 1000000)))
                  ((= 0 (% ns 1000)) (format ".%06d" (/ ns 1000)))
                  (t (format ".%09d" ns))))))

(defun elnix-toml--offset ()
  "The text of the time offset at the parser, or nil when there is none.
A zero offset is printed as Z, and minutes past 59 carry into the hour."
  (let ((c (elnix-toml--peek)))
    (cond
     ((memq c '(?Z ?z)) (elnix-toml--advance) "Z")
     ((memq c '(?+ ?-))
      (elnix-toml--advance)
      (let* ((h (prog1 (elnix-toml--fixed-digits 2) (elnix-toml--expect ?:)))
             (m (elnix-toml--fixed-digits 2))
             (total (+ (* 60 h) m)))
        (unless (and (<= h 24) (<= m 60))
          (elnix-toml--fail "too large offset: |hour| <= 24, |minute| <= 60"))
        (if (= total 0)
            "Z"
          (format "%c%02d:%02d" c (/ total 60) (% total 60))))))))

(defun elnix-toml--timestamp ()
  "The value of the date or time at the parser.
Without `elnix-toml-timestamps' this is an error.  With it, the value
is the set { _type = \"timestamp\"; value = TEXT; }, with TEXT the
normalised form Nix prints."
  (unless elnix-toml-timestamps
    (elnix-toml--fail "Dates and times are not supported"))
  (let ((text
         (if (eq (elnix-toml--peek 2) ?:)
             (elnix-toml--time)
           (let ((date (elnix-toml--date))
                 (c (elnix-toml--peek)))
             (if (or (memq c '(?T ?t))
                     (and (eq c ?\s) (elnix-toml--digits-p 1 1)))
                 (progn
                   (elnix-toml--advance)
                   (let* ((time (elnix-toml--time))
                          (offset (elnix-toml--offset)))
                     (concat date "T" time offset)))
               date)))))
    (elnix-attrs-from-alist
     (list (cons '_type "timestamp")
           (cons 'value (elnix--unibyte text))))))

(defun elnix-toml--number ()
  "The integer, float, date or time at the parser."
  (if (or (and (elnix-toml--digits-p 0 4) (eq (elnix-toml--peek 4) ?-))
          (and (elnix-toml--digits-p 0 2) (eq (elnix-toml--peek 2) ?:)))
      (elnix-toml--timestamp)
    (elnix-toml--plain-number)))

(defun elnix-toml--plain-number ()
  "The integer or float at the parser."
  (let ((start elnix-toml--i) c)
    (while (and (setq c (elnix-toml--peek))
                (or (elnix-toml--bare-key-char-p c) (memq c '(?. ?+))))
      (elnix-toml--advance))
    (let ((text (substring elnix-toml--s start elnix-toml--i))
          (case-fold-search nil))
      (cond
       ((string-match "\\`\\([-+]?\\)\\(inf\\|nan\\)\\'" text)
        (let ((v (if (equal (match-string 2 text) "inf") 1.0e+INF 0.0e+NaN)))
          (if (equal (match-string 1 text) "-") (- v) v)))
       ((string-match-p "\\`0x[0-9A-Fa-f]\\(?:_?[0-9A-Fa-f]\\)*\\'" text)
        (elnix-toml--integer text (substring text 2) 16))
       ((string-match-p "\\`0o[0-7]\\(?:_?[0-7]\\)*\\'" text)
        (elnix-toml--integer text (substring text 2) 8))
       ((string-match-p "\\`0b[01]\\(?:_?[01]\\)*\\'" text)
        (elnix-toml--integer text (substring text 2) 2))
       ((string-match-p elnix-toml--dec-re text)
        (elnix-toml--integer text text 10))
       ((string-match-p elnix-toml--float-re text)
        (let ((v (string-to-number (string-replace "_" "" text))))
          (when (or (= v 1.0e+INF) (= v -1.0e+INF))
            (elnix-toml--fail "float %s is out of range" text))
          (float v)))
       (t (elnix-toml--fail "invalid value"))))))

(defun elnix-toml--array ()
  "The vector of the array at the parser."
  (elnix-toml--advance)
  (let ((items nil) (done nil))
    (while (not done)
      (elnix-toml--skip-blank)
      (if (eq (elnix-toml--peek) ?\])
          (progn (elnix-toml--advance) (setq done t))
        (push (elnix-toml--value) items)
        (elnix-toml--skip-blank)
        (pcase (elnix-toml--peek)
          (?, (elnix-toml--advance))
          (?\] (elnix-toml--advance) (setq done t))
          (_ (elnix-toml--fail "expected `,' or `]' in an array")))))
    (vconcat (nreverse items))))

(defun elnix-toml--inline-table ()
  "The table of the inline table at the parser."
  (elnix-toml--advance)
  (let ((tbl (elnix-toml--table-make 'inline)) (done nil))
    (elnix-toml--skip-ws)
    (if (eq (elnix-toml--peek) ?\})
        (elnix-toml--advance)
      (while (not done)
        (elnix-toml--keyval tbl)
        (elnix-toml--skip-ws)
        (pcase (elnix-toml--peek)
          (?, (elnix-toml--advance))
          (?\} (elnix-toml--advance) (setq done t))
          (_ (elnix-toml--fail "expected `,' or `}' in an inline table")))))
    tbl))

(defun elnix-toml--value ()
  "The value at the parser."
  (let ((c (elnix-toml--peek)))
    (cond
     ((null c) (elnix-toml--fail "expected a value"))
     ((memq c '(?\" ?')) (elnix-toml--string))
     ((eq c ?\[) (elnix-toml--array))
     ((eq c ?\{) (elnix-toml--inline-table))
     ((elnix-toml--word-p "true") (elnix-toml--advance 4) t)
     ((elnix-toml--word-p "false") (elnix-toml--advance 5) nil)
     (t (elnix-toml--number)))))

;;;; Building tables

(defun elnix-toml--keyval (tbl)
  "Parse the key/value pair at the parser into the table TBL.
The dotted parts of the key create or extend tables of kind `dotted'
only."
  (let ((keys (elnix-toml--key)))
    (unless (eq (elnix-toml--peek) ?=)
      (elnix-toml--fail "expected `=' after a key"))
    (elnix-toml--advance)
    (elnix-toml--skip-ws)
    (let ((v (elnix-toml--value)))
      (dolist (k (butlast keys))
        (let ((e (gethash k (elnix-toml--table-map tbl) 'elnix-toml--none)))
          (setq tbl
                (cond
                 ((eq e 'elnix-toml--none)
                  (puthash k (elnix-toml--table-make 'dotted)
                           (elnix-toml--table-map tbl)))
                 ((and (elnix-toml--table-p e)
                       (eq (elnix-toml--table-kind e) 'dotted))
                  e)
                 (t (elnix-toml--fail "cannot add key `%s' to the value of `%s'"
                                      (elnix-toml--key-text keys) k))))))
      (let ((k (car (last keys))))
        (unless (eq (gethash k (elnix-toml--table-map tbl) 'elnix-toml--none)
                    'elnix-toml--none)
          (elnix-toml--fail "key `%s' is defined twice"
                            (elnix-toml--key-text keys)))
        (puthash k v (elnix-toml--table-map tbl))))))

(defun elnix-toml--header (root)
  "Parse the table header at the parser and return its table.
ROOT is the root table of the document."
  (let ((aot (eq (elnix-toml--peek 1) ?\[)))
    (elnix-toml--advance (if aot 2 1))
    (let ((keys (elnix-toml--key))
          (tbl root))
      (unless (eq (elnix-toml--peek) ?\])
        (elnix-toml--fail "expected `]' after a table name"))
      (elnix-toml--advance)
      (when aot
        (unless (eq (elnix-toml--peek) ?\])
          (elnix-toml--fail "expected `]]' after a table name"))
        (elnix-toml--advance))
      (dolist (k (butlast keys))
        (let ((e (gethash k (elnix-toml--table-map tbl) 'elnix-toml--none)))
          (setq tbl
                (cond
                 ((eq e 'elnix-toml--none)
                  (puthash k (elnix-toml--table-make 'implicit)
                           (elnix-toml--table-map tbl)))
                 ((and (elnix-toml--table-p e)
                       (not (eq (elnix-toml--table-kind e) 'inline)))
                  e)
                 ((elnix-toml--aot-p e) (car (elnix-toml--aot-tables e)))
                 (t (elnix-toml--fail "cannot define table `%s' inside `%s'"
                                      (elnix-toml--key-text keys) k))))))
      (let* ((k (car (last keys)))
             (map (elnix-toml--table-map tbl))
             (e (gethash k map 'elnix-toml--none)))
        (cond
         (aot
          (let ((new (elnix-toml--table-make 'header)))
            (cond
             ((eq e 'elnix-toml--none)
              (puthash k (elnix-toml--aot-make (list new)) map))
             ((elnix-toml--aot-p e) (push new (elnix-toml--aot-tables e)))
             (t (elnix-toml--fail "`%s' is not an array of tables"
                                  (elnix-toml--key-text keys))))
            new))
         ((eq e 'elnix-toml--none)
          (puthash k (elnix-toml--table-make 'header) map))
         ((and (elnix-toml--table-p e)
               (eq (elnix-toml--table-kind e) 'implicit))
          (setf (elnix-toml--table-kind e) 'header)
          e)
         (t (elnix-toml--fail "table `%s' is defined twice"
                              (elnix-toml--key-text keys))))))))

(defun elnix-toml--finish (v)
  "The Nix value of the value under construction V."
  (cond
   ((elnix-toml--table-p v)
    (let* ((map (elnix-toml--table-map v))
           (attrs (elnix-make-attrs (hash-table-count map))))
      (maphash (lambda (k x) (puthash (intern k) (elnix-toml--finish x) attrs))
               map)
      (elnix--mkattrs attrs)))
   ((elnix-toml--aot-p v)
    (vconcat (mapcar #'elnix-toml--finish
                     (reverse (elnix-toml--aot-tables v)))))
   ((vectorp v) (vconcat (mapcar #'elnix-toml--finish v)))
   (t v)))

(defun elnix-from-toml (s)
  "`builtins.fromTOML' of S, a unibyte string of UTF-8 bytes."
  (let* ((elnix-toml--s (elnix--unibyte s))
         (elnix-toml--i 0)
         (elnix-toml--n (length elnix-toml--s))
         (root (elnix-toml--table-make 'header))
         (current root))
    (when (string-prefix-p "\357\273\277" elnix-toml--s)
      (elnix-toml--advance 3))
    (while (< elnix-toml--i elnix-toml--n)
      (elnix-toml--skip-ws)
      (let ((c (elnix-toml--peek)))
        (cond
         ((eq c ?\[) (setq current (elnix-toml--header root)))
         ((memq c '(nil ?# ?\n ?\r)))
         (t (elnix-toml--keyval current))))
      (elnix-toml--end-of-line))
    (elnix-toml--finish root)))

(provide 'elnix-toml)

;;; elnix-toml.el ends here
