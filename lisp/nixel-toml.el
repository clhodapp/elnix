;;; nixel-toml.el --- builtins.fromTOML -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A TOML v1.0 parser for `builtins.fromTOML'.  It reads the raw UTF-8
;; bytes of a document and returns the Nix value that Nix 2.34 builds
;; from it: a table becomes an attribute set, an array becomes a list,
;; and strings, integers, floats and booleans become the Nix values of
;; the same kind.
;;
;; An offset date-time, a local date-time, a local date or a local
;; time is an error unless `nixel-toml-timestamps' is set, as it is an
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
(require 'nixel-runtime)

(defvar nixel-toml-timestamps nil
  "Whether `nixel-from-toml' turns dates and times into values.
Nil makes a date or a time an error, as Nix does without the
parse-toml-timestamps experimental feature.  Non-nil makes each one
the set { _type = \"timestamp\"; value = TEXT; }, as Nix does with
the feature.")

(defvar nixel-toml--s ""
  "The unibyte TOML document being parsed.")

(defvar nixel-toml--i 0
  "The byte offset of the parser in `nixel-toml--s'.")

(defvar nixel-toml--n 0
  "The length of `nixel-toml--s'.")

(cl-defstruct (nixel-toml--table
               (:constructor nixel-toml--table-make (kind))
               (:copier nil))
  "A table under construction.
KIND is one of `implicit', `header', `dotted' and `inline'.  MAP
holds the entries from key strings to values under construction."
  kind
  (map (make-hash-table :test 'equal)))

(cl-defstruct (nixel-toml--aot
               (:constructor nixel-toml--aot-make (tables))
               (:copier nil))
  "An array of tables under construction.
TABLES holds its tables, the last one first."
  tables)

;; A static array is a vector of values under construction.

;;;; Errors and positions

(defun nixel-toml--fail (fmt &rest args)
  "Signal a TOML parse error with the message FMT and ARGS."
  (let ((line (1+ (cl-count ?\n nixel-toml--s
                            :end (min nixel-toml--i nixel-toml--n)))))
    (nixel-fail "while parsing TOML: %s (line number %d)"
                (apply #'format-message fmt args) line)))

(defsubst nixel-toml--peek (&optional k)
  "The byte K bytes past the parser, or nil past the end."
  (let ((j (+ nixel-toml--i (or k 0))))
    (and (< j nixel-toml--n) (aref nixel-toml--s j))))

(defsubst nixel-toml--advance (&optional k)
  "Move the parser K bytes forward, one when K is nil."
  (setq nixel-toml--i (+ nixel-toml--i (or k 1))))

(defsubst nixel-toml--control-p (c)
  "Whether the byte C is a control character TOML text may not hold.
A tab is allowed everywhere text is."
  (or (and (< c #x20) (/= c ?\t)) (= c #x7f)))

(defun nixel-toml--newline ()
  "The length of the newline at the parser, or nil when there is none."
  (let ((c (nixel-toml--peek)))
    (cond ((eq c ?\n) 1)
          ((and (eq c ?\r) (eq (nixel-toml--peek 1) ?\n)) 2))))

(defun nixel-toml--utf8-char ()
  "Move past the UTF-8 sequence at the parser.
Fail unless the bytes there encode one Unicode scalar value in its
shortest form."
  (let* ((b (nixel-toml--peek))
         (len (cond ((<= #xc2 b #xdf) 2)
                    ((<= #xe0 b #xef) 3)
                    ((<= #xf0 b #xf4) 4)))
         (lo (pcase b (#xe0 #xa0) (#xf0 #x90) (_ #x80)))
         (hi (pcase b (#xed #x9f) (#xf4 #x8f) (_ #xbf))))
    (unless len (nixel-toml--fail "invalid UTF-8 byte"))
    (dotimes (k (1- len))
      (let ((c (nixel-toml--peek (1+ k))))
        (unless (and c (if (= k 0) (<= lo c hi) (<= #x80 c #xbf)))
          (nixel-toml--fail "invalid UTF-8 sequence"))))
    (nixel-toml--advance len)))

(defun nixel-toml--utf8 (cp)
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

(defun nixel-toml--skip-ws ()
  "Move past spaces and tabs."
  (while (memq (nixel-toml--peek) '(?\s ?\t))
    (nixel-toml--advance)))

(defun nixel-toml--skip-comment ()
  "Move past the comment at the parser, up to the end of its line.
Do nothing when no comment starts at the parser."
  (when (eq (nixel-toml--peek) ?#)
    (nixel-toml--advance)
    (let (c)
      (while (and (setq c (nixel-toml--peek))
                  (not (nixel-toml--newline)))
        (cond ((>= c #x80) (nixel-toml--utf8-char))
              ((nixel-toml--control-p c)
               (nixel-toml--fail "control character in a comment"))
              (t (nixel-toml--advance)))))))

(defun nixel-toml--end-of-line ()
  "Move past trailing whitespace, a comment and the end of the line.
Fail when something else follows on the line."
  (nixel-toml--skip-ws)
  (nixel-toml--skip-comment)
  (when (< nixel-toml--i nixel-toml--n)
    (let ((nl (nixel-toml--newline)))
      (unless nl (nixel-toml--fail "expected the end of the line"))
      (nixel-toml--advance nl))))

(defun nixel-toml--skip-blank ()
  "Move past whitespace, comments and newlines."
  (let ((done nil))
    (while (not done)
      (nixel-toml--skip-ws)
      (nixel-toml--skip-comment)
      (let ((nl (nixel-toml--newline)))
        (if nl (nixel-toml--advance nl) (setq done t))))))

;;;; Strings

(defun nixel-toml--hex-escape (len)
  "The UTF-8 bytes of the escape of LEN hex digits at the parser."
  (let ((cp 0))
    (dotimes (j len)
      (let* ((c (nixel-toml--peek j))
             (d (and c (cond ((<= ?0 c ?9) (- c ?0))
                             ((<= ?a c ?f) (- c (- ?a 10)))
                             ((<= ?A c ?F) (- c (- ?A 10)))))))
        (unless d (nixel-toml--fail "invalid unicode escape"))
        (setq cp (+ (* cp 16) d))))
    (when (or (> cp #x10ffff) (<= #xd800 cp #xdfff))
      (nixel-toml--fail "escape is not a Unicode scalar value"))
    (nixel-toml--advance len)
    (nixel-toml--utf8 cp)))

(defun nixel-toml--escape (multi)
  "The bytes of the escape after the backslash at the parser.
With MULTI, the string is a multi-line basic string, where a
backslash at the end of a line removes the line end and all
whitespace after it; the result is then the empty string."
  (let ((e (nixel-toml--peek)))
    (if (and multi (memq e '(?\s ?\t ?\n ?\r)))
        (progn
          (nixel-toml--skip-ws)
          (unless (nixel-toml--newline)
            (nixel-toml--fail "invalid escape in a string"))
          (let ((done nil))
            (while (not done)
              (let ((nl (nixel-toml--newline)))
                (cond (nl (nixel-toml--advance nl))
                      ((memq (nixel-toml--peek) '(?\s ?\t))
                       (nixel-toml--advance))
                      (t (setq done t))))))
          "")
      (nixel-toml--advance)
      (pcase e
        (?b "\b") (?t "\t") (?n "\n") (?f "\f") (?r "\r")
        (?\" "\"") (?\\ "\\")
        (?u (nixel-toml--hex-escape 4))
        (?U (nixel-toml--hex-escape 8))
        (_ (nixel-toml--fail "invalid escape in a string"))))))

(defun nixel-toml--string ()
  "The unibyte string of the string literal at the parser.
The literal is basic or literal, single-line or multi-line, as its
opening delimiter says."
  (let* ((q (nixel-toml--peek))
         (basic (eq q ?\"))
         (multi (and (eq (nixel-toml--peek 1) q) (eq (nixel-toml--peek 2) q)))
         (out nil)
         (start nil)
         (done nil))
    (nixel-toml--advance (if multi 3 1))
    (when multi
      (let ((nl (nixel-toml--newline)))
        (when nl (nixel-toml--advance nl))))
    (setq start nixel-toml--i)
    (cl-flet ((flush ()
                (when (> nixel-toml--i start)
                  (push (substring nixel-toml--s start nixel-toml--i) out))))
      (while (not done)
        (let ((c (nixel-toml--peek)))
          (cond
           ((null c) (nixel-toml--fail "unterminated string"))
           ((eq c q)
            (if (not multi)
                (progn (flush) (nixel-toml--advance) (setq done t))
              (let ((k 0))
                (while (eq (nixel-toml--peek k) q) (setq k (1+ k)))
                (cond
                 ((< k 3) (nixel-toml--advance k))
                 ((> k 5) (nixel-toml--fail "too many quotes in a string"))
                 (t (nixel-toml--advance (- k 3))
                    (flush)
                    (nixel-toml--advance 3)
                    (setq done t))))))
           ((and basic (eq c ?\\))
            (flush)
            (nixel-toml--advance)
            (push (nixel-toml--escape multi) out)
            (setq start nixel-toml--i))
           ((nixel-toml--newline)
            (unless multi (nixel-toml--fail "newline in a single-line string"))
            (nixel-toml--advance (nixel-toml--newline)))
           ((>= c #x80) (nixel-toml--utf8-char))
           ((nixel-toml--control-p c)
            (nixel-toml--fail "control character in a string"))
           (t (nixel-toml--advance))))))
    (let ((s (apply #'concat (nreverse out))))
      (when (string-search "\0" s)
        (nixel-toml--fail "string contains a null byte"))
      s)))

;;;; Keys

(defun nixel-toml--bare-key-char-p (c)
  "Whether the byte C may appear in a bare key."
  (and c (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9) (eq c ?_) (eq c ?-))))

(defun nixel-toml--simple-key ()
  "The string of the bare or quoted key at the parser."
  (let ((c (nixel-toml--peek)))
    (cond
     ((memq c '(?\" ?'))
      (when (and (eq (nixel-toml--peek 1) c) (eq (nixel-toml--peek 2) c))
        (nixel-toml--fail "a multi-line string cannot be a key"))
      (nixel-toml--string))
     ((nixel-toml--bare-key-char-p c)
      (let ((start nixel-toml--i))
        (while (nixel-toml--bare-key-char-p (nixel-toml--peek))
          (nixel-toml--advance))
        (substring nixel-toml--s start nixel-toml--i)))
     (t (nixel-toml--fail "expected a key")))))

(defun nixel-toml--key ()
  "The list of the parts of the dotted key at the parser.
Whitespace around the key and around its dots is skipped."
  (let ((parts nil) (done nil))
    (while (not done)
      (nixel-toml--skip-ws)
      (push (nixel-toml--simple-key) parts)
      (nixel-toml--skip-ws)
      (if (eq (nixel-toml--peek) ?.)
          (nixel-toml--advance)
        (setq done t)))
    (nreverse parts)))

(defun nixel-toml--key-text (keys)
  "The dotted key KEYS as text for an error message."
  (mapconcat #'identity keys "."))

;;;; Values

(defun nixel-toml--digits-p (k count)
  "Whether the COUNT bytes K bytes past the parser are all digits."
  (let ((ok t))
    (dotimes (j count)
      (let ((c (nixel-toml--peek (+ k j))))
        (unless (and c (<= ?0 c ?9)) (setq ok nil))))
    ok))

(defun nixel-toml--word-p (w)
  "Whether the text at the parser starts with the word W."
  (eq t (compare-strings nixel-toml--s nixel-toml--i
                         (min nixel-toml--n (+ nixel-toml--i (length w)))
                         w nil nil)))

(defconst nixel-toml--dec-re
  "\\`[-+]?\\(?:0\\|[1-9]\\(?:_?[0-9]\\)*\\)\\'"
  "A decimal integer.")

(defconst nixel-toml--float-re
  (concat "\\`[-+]?\\(?:0\\|[1-9]\\(?:_?[0-9]\\)*\\)"
          "\\(?:\\.[0-9]\\(?:_?[0-9]\\)*\\)?"
          "\\(?:[eE][-+]?[0-9]\\(?:_?[0-9]\\)*\\)?\\'")
  "A decimal float; a decimal integer matches it too.")

(defun nixel-toml--integer (text digits base)
  "The integer of the DIGITS in BASE, whose literal is TEXT.
DIGITS may hold underscores and a sign."
  (let ((n (string-to-number (string-replace "_" "" digits) base)))
    (unless (and (<= n nixel--int-max) (>= n nixel--int-min))
      (nixel-toml--fail "integer %s is out of range" text))
    n))

;;;; Dates and times

(defun nixel-toml--fixed-digits (count)
  "The number of the COUNT digits at the parser, moving past them."
  (unless (nixel-toml--digits-p 0 count)
    (nixel-toml--fail "invalid date or time"))
  (prog1 (string-to-number
          (substring nixel-toml--s nixel-toml--i (+ nixel-toml--i count)))
    (nixel-toml--advance count)))

(defun nixel-toml--expect (c)
  "Move past the byte C at the parser, failing when another is there."
  (unless (eq (nixel-toml--peek) c)
    (nixel-toml--fail "invalid date or time"))
  (nixel-toml--advance))

(defun nixel-toml--date ()
  "The text of the local date at the parser, in the form YYYY-MM-DD."
  (let* ((y (prog1 (nixel-toml--fixed-digits 4) (nixel-toml--expect ?-)))
         (m (prog1 (nixel-toml--fixed-digits 2) (nixel-toml--expect ?-)))
         (d (nixel-toml--fixed-digits 2))
         (leap (and (= 0 (% y 4)) (or (/= 0 (% y 100)) (= 0 (% y 400)))))
         (days (and (<= 1 m 12)
                    (aref (if leap [31 29 31 30 31 30 31 31 30 31 30 31]
                            [31 28 31 30 31 30 31 31 30 31 30 31])
                          (1- m)))))
    (unless (and days (<= 1 d days))
      (nixel-toml--fail "month must be 01-12 and day must exist in the month"))
    (format "%04d-%02d-%02d" y m d)))

(defun nixel-toml--time ()
  "The text of the local time at the parser, as Nix prints it.
Digits of a fraction of a second past the ninth are dropped, and the
fraction is printed as milliseconds, microseconds or nanoseconds,
whichever is the shortest that holds it exactly."
  (let* ((h (prog1 (nixel-toml--fixed-digits 2) (nixel-toml--expect ?:)))
         (m (prog1 (nixel-toml--fixed-digits 2) (nixel-toml--expect ?:)))
         (s (nixel-toml--fixed-digits 2))
         (ns 0))
    (unless (and (<= h 23) (<= m 59))
      (nixel-toml--fail "hour must be 00-23 and minute must be 00-59"))
    (unless (<= s 60)
      (nixel-toml--fail "second must be 00-60"))
    (when (eq (nixel-toml--peek) ?.)
      (nixel-toml--advance)
      (let ((start nixel-toml--i))
        (while (nixel-toml--digits-p 0 1) (nixel-toml--advance))
        (when (= start nixel-toml--i)
          (nixel-toml--fail "expected digits after `.' in a time"))
        (let ((digits (substring nixel-toml--s start
                                 (min nixel-toml--i (+ start 9)))))
          (setq ns (string-to-number
                    (concat digits (make-string (- 9 (length digits)) ?0)))))))
    (concat (format "%02d:%02d:%02d" h m s)
            (cond ((= ns 0) "")
                  ((= 0 (% ns 1000000)) (format ".%03d" (/ ns 1000000)))
                  ((= 0 (% ns 1000)) (format ".%06d" (/ ns 1000)))
                  (t (format ".%09d" ns))))))

(defun nixel-toml--offset ()
  "The text of the time offset at the parser, or nil when there is none.
A zero offset is printed as Z, and minutes past 59 carry into the hour."
  (let ((c (nixel-toml--peek)))
    (cond
     ((memq c '(?Z ?z)) (nixel-toml--advance) "Z")
     ((memq c '(?+ ?-))
      (nixel-toml--advance)
      (let* ((h (prog1 (nixel-toml--fixed-digits 2) (nixel-toml--expect ?:)))
             (m (nixel-toml--fixed-digits 2))
             (total (+ (* 60 h) m)))
        (unless (and (<= h 24) (<= m 60))
          (nixel-toml--fail "too large offset: |hour| <= 24, |minute| <= 60"))
        (if (= total 0)
            "Z"
          (format "%c%02d:%02d" c (/ total 60) (% total 60))))))))

(defun nixel-toml--timestamp ()
  "The value of the date or time at the parser.
Without `nixel-toml-timestamps' this is an error.  With it, the value
is the set { _type = \"timestamp\"; value = TEXT; }, with TEXT the
normalised form Nix prints."
  (unless nixel-toml-timestamps
    (nixel-toml--fail "Dates and times are not supported"))
  (let ((text
         (if (eq (nixel-toml--peek 2) ?:)
             (nixel-toml--time)
           (let ((date (nixel-toml--date))
                 (c (nixel-toml--peek)))
             (if (or (memq c '(?T ?t))
                     (and (eq c ?\s) (nixel-toml--digits-p 1 1)))
                 (progn
                   (nixel-toml--advance)
                   (let* ((time (nixel-toml--time))
                          (offset (nixel-toml--offset)))
                     (concat date "T" time offset)))
               date)))))
    (nixel-attrs-from-alist
     (list (cons '_type "timestamp")
           (cons 'value (nixel--unibyte text))))))

(defun nixel-toml--number ()
  "The integer, float, date or time at the parser."
  (if (or (and (nixel-toml--digits-p 0 4) (eq (nixel-toml--peek 4) ?-))
          (and (nixel-toml--digits-p 0 2) (eq (nixel-toml--peek 2) ?:)))
      (nixel-toml--timestamp)
    (nixel-toml--plain-number)))

(defun nixel-toml--plain-number ()
  "The integer or float at the parser."
  (let ((start nixel-toml--i) c)
    (while (and (setq c (nixel-toml--peek))
                (or (nixel-toml--bare-key-char-p c) (memq c '(?. ?+))))
      (nixel-toml--advance))
    (let ((text (substring nixel-toml--s start nixel-toml--i))
          (case-fold-search nil))
      (cond
       ((string-match "\\`\\([-+]?\\)\\(inf\\|nan\\)\\'" text)
        (let ((v (if (equal (match-string 2 text) "inf") 1.0e+INF 0.0e+NaN)))
          (if (equal (match-string 1 text) "-") (- v) v)))
       ((string-match-p "\\`0x[0-9A-Fa-f]\\(?:_?[0-9A-Fa-f]\\)*\\'" text)
        (nixel-toml--integer text (substring text 2) 16))
       ((string-match-p "\\`0o[0-7]\\(?:_?[0-7]\\)*\\'" text)
        (nixel-toml--integer text (substring text 2) 8))
       ((string-match-p "\\`0b[01]\\(?:_?[01]\\)*\\'" text)
        (nixel-toml--integer text (substring text 2) 2))
       ((string-match-p nixel-toml--dec-re text)
        (nixel-toml--integer text text 10))
       ((string-match-p nixel-toml--float-re text)
        (let ((v (string-to-number (string-replace "_" "" text))))
          (when (or (= v 1.0e+INF) (= v -1.0e+INF))
            (nixel-toml--fail "float %s is out of range" text))
          (float v)))
       (t (nixel-toml--fail "invalid value"))))))

(defun nixel-toml--array ()
  "The vector of the array at the parser."
  (nixel-toml--advance)
  (let ((items nil) (done nil))
    (while (not done)
      (nixel-toml--skip-blank)
      (if (eq (nixel-toml--peek) ?\])
          (progn (nixel-toml--advance) (setq done t))
        (push (nixel-toml--value) items)
        (nixel-toml--skip-blank)
        (pcase (nixel-toml--peek)
          (?, (nixel-toml--advance))
          (?\] (nixel-toml--advance) (setq done t))
          (_ (nixel-toml--fail "expected `,' or `]' in an array")))))
    (vconcat (nreverse items))))

(defun nixel-toml--inline-table ()
  "The table of the inline table at the parser."
  (nixel-toml--advance)
  (let ((tbl (nixel-toml--table-make 'inline)) (done nil))
    (nixel-toml--skip-ws)
    (if (eq (nixel-toml--peek) ?\})
        (nixel-toml--advance)
      (while (not done)
        (nixel-toml--keyval tbl)
        (nixel-toml--skip-ws)
        (pcase (nixel-toml--peek)
          (?, (nixel-toml--advance))
          (?\} (nixel-toml--advance) (setq done t))
          (_ (nixel-toml--fail "expected `,' or `}' in an inline table")))))
    tbl))

(defun nixel-toml--value ()
  "The value at the parser."
  (let ((c (nixel-toml--peek)))
    (cond
     ((null c) (nixel-toml--fail "expected a value"))
     ((memq c '(?\" ?')) (nixel-toml--string))
     ((eq c ?\[) (nixel-toml--array))
     ((eq c ?\{) (nixel-toml--inline-table))
     ((nixel-toml--word-p "true") (nixel-toml--advance 4) t)
     ((nixel-toml--word-p "false") (nixel-toml--advance 5) nil)
     (t (nixel-toml--number)))))

;;;; Building tables

(defun nixel-toml--keyval (tbl)
  "Parse the key/value pair at the parser into the table TBL.
The dotted parts of the key create or extend tables of kind `dotted'
only."
  (let ((keys (nixel-toml--key)))
    (unless (eq (nixel-toml--peek) ?=)
      (nixel-toml--fail "expected `=' after a key"))
    (nixel-toml--advance)
    (nixel-toml--skip-ws)
    (let ((v (nixel-toml--value)))
      (dolist (k (butlast keys))
        (let ((e (gethash k (nixel-toml--table-map tbl) 'nixel-toml--none)))
          (setq tbl
                (cond
                 ((eq e 'nixel-toml--none)
                  (puthash k (nixel-toml--table-make 'dotted)
                           (nixel-toml--table-map tbl)))
                 ((and (nixel-toml--table-p e)
                       (eq (nixel-toml--table-kind e) 'dotted))
                  e)
                 (t (nixel-toml--fail "cannot add key `%s' to the value of `%s'"
                                      (nixel-toml--key-text keys) k))))))
      (let ((k (car (last keys))))
        (unless (eq (gethash k (nixel-toml--table-map tbl) 'nixel-toml--none)
                    'nixel-toml--none)
          (nixel-toml--fail "key `%s' is defined twice"
                            (nixel-toml--key-text keys)))
        (puthash k v (nixel-toml--table-map tbl))))))

(defun nixel-toml--header (root)
  "Parse the table header at the parser and return its table.
ROOT is the root table of the document."
  (let ((aot (eq (nixel-toml--peek 1) ?\[)))
    (nixel-toml--advance (if aot 2 1))
    (let ((keys (nixel-toml--key))
          (tbl root))
      (unless (eq (nixel-toml--peek) ?\])
        (nixel-toml--fail "expected `]' after a table name"))
      (nixel-toml--advance)
      (when aot
        (unless (eq (nixel-toml--peek) ?\])
          (nixel-toml--fail "expected `]]' after a table name"))
        (nixel-toml--advance))
      (dolist (k (butlast keys))
        (let ((e (gethash k (nixel-toml--table-map tbl) 'nixel-toml--none)))
          (setq tbl
                (cond
                 ((eq e 'nixel-toml--none)
                  (puthash k (nixel-toml--table-make 'implicit)
                           (nixel-toml--table-map tbl)))
                 ((and (nixel-toml--table-p e)
                       (not (eq (nixel-toml--table-kind e) 'inline)))
                  e)
                 ((nixel-toml--aot-p e) (car (nixel-toml--aot-tables e)))
                 (t (nixel-toml--fail "cannot define table `%s' inside `%s'"
                                      (nixel-toml--key-text keys) k))))))
      (let* ((k (car (last keys)))
             (map (nixel-toml--table-map tbl))
             (e (gethash k map 'nixel-toml--none)))
        (cond
         (aot
          (let ((new (nixel-toml--table-make 'header)))
            (cond
             ((eq e 'nixel-toml--none)
              (puthash k (nixel-toml--aot-make (list new)) map))
             ((nixel-toml--aot-p e) (push new (nixel-toml--aot-tables e)))
             (t (nixel-toml--fail "`%s' is not an array of tables"
                                  (nixel-toml--key-text keys))))
            new))
         ((eq e 'nixel-toml--none)
          (puthash k (nixel-toml--table-make 'header) map))
         ((and (nixel-toml--table-p e)
               (eq (nixel-toml--table-kind e) 'implicit))
          (setf (nixel-toml--table-kind e) 'header)
          e)
         (t (nixel-toml--fail "table `%s' is defined twice"
                              (nixel-toml--key-text keys))))))))

(defun nixel-toml--finish (v)
  "The Nix value of the value under construction V."
  (cond
   ((nixel-toml--table-p v)
    (let* ((map (nixel-toml--table-map v))
           (attrs (nixel-make-attrs (hash-table-count map))))
      (maphash (lambda (k x) (puthash (intern k) (nixel-toml--finish x) attrs))
               map)
      (nixel--mkattrs attrs)))
   ((nixel-toml--aot-p v)
    (vconcat (mapcar #'nixel-toml--finish
                     (reverse (nixel-toml--aot-tables v)))))
   ((vectorp v) (vconcat (mapcar #'nixel-toml--finish v)))
   (t v)))

(defun nixel-from-toml (s)
  "`builtins.fromTOML' of S, a unibyte string of UTF-8 bytes."
  (let* ((nixel-toml--s (nixel--unibyte s))
         (nixel-toml--i 0)
         (nixel-toml--n (length nixel-toml--s))
         (root (nixel-toml--table-make 'header))
         (current root))
    (when (string-prefix-p "\357\273\277" nixel-toml--s)
      (nixel-toml--advance 3))
    (while (< nixel-toml--i nixel-toml--n)
      (nixel-toml--skip-ws)
      (let ((c (nixel-toml--peek)))
        (cond
         ((eq c ?\[) (setq current (nixel-toml--header root)))
         ((memq c '(nil ?# ?\n ?\r)))
         (t (nixel-toml--keyval current))))
      (nixel-toml--end-of-line))
    (nixel-toml--finish root)))

(provide 'nixel-toml)

;;; nixel-toml.el ends here
