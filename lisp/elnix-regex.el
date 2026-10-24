;;; elnix-regex.el --- builtins.match and builtins.split -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Nix compiles its regular expressions with std::regex in POSIX
;; extended mode, as libstdc++ implements it.  This library parses such
;; an expression, rejects what libstdc++ rejects, and writes the
;; equivalent Emacs regexp, which runs on the unibyte strings that Nix
;; strings are.
;;
;; The translation:
;;   ( ) | { }        become \( \) \| \{ \}, groups numbered as in ERE
;;   \. \[ \\ \( \) \* \+ \? \{ \| \^ \$   become the literal character;
;;                    any other escape is an error, as in libstdc++
;;   .                becomes [^\0], since libstdc++ matches a newline
;;   ^ $              become \` \', anchors at the ends of the string
;;   [...]            is rebuilt: classes, collating names such as
;;                    [.hyphen.] and equivalence classes such as [=a=]
;;                    are resolved to the characters they stand for
;;
;; libstdc++ explores a pattern depth first, alternatives left to right
;; and repetitions longest first, and reports the first match it finds,
;; both for `match', which must cover the whole string, and for the
;; searches of `split'.  Emacs backtracks in the same order, so
;; `string-match' finds the same match with the same captures.

;;; Code:

(require 'cl-lib)
(require 'elnix-runtime)

(defvar elnix-regex--cache (make-hash-table :test 'equal)
  "Translated regular expressions by source text: (REGEXP . GROUPS).")

(defun elnix-regex--invalid (re why)
  (elnix-fail "invalid regular expression '%s': %s" re why))

;; The names of the POSIX portable character set, indexed by character
;; code, which libstdc++ accepts in [.NAME.] and [=NAME=].
(defconst elnix-regex--collating-names
  ["NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "alert" "backspace" "tab"
   "newline" "vertical-tab" "form-feed" "carriage-return" "SO" "SI" "DLE"
   "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN" "EM" "SUB" "ESC" "IS4"
   "IS3" "IS2" "IS1" "space" "exclamation-mark" "quotation-mark"
   "number-sign" "dollar-sign" "percent-sign" "ampersand" "apostrophe"
   "left-parenthesis" "right-parenthesis" "asterisk" "plus-sign" "comma"
   "hyphen" "period" "slash" "zero" "one" "two" "three" "four" "five" "six"
   "seven" "eight" "nine" "colon" "semicolon" "less-than-sign"
   "equals-sign" "greater-than-sign" "question-mark" "commercial-at"
   "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
   "S" "T" "U" "V" "W" "X" "Y" "Z" "left-square-bracket" "backslash"
   "right-square-bracket" "circumflex" "underscore" "grave-accent"
   "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r"
   "s" "t" "u" "v" "w" "x" "y" "z" "left-curly-bracket" "vertical-line"
   "right-curly-bracket" "tilde" "DEL"])

;; The class names libstdc++ knows, and the Emacs set contents for each.
(defconst elnix-regex--classes
  '(("alnum" . "[:alnum:]") ("alpha" . "[:alpha:]") ("blank" . "[:blank:]")
    ("cntrl" . "[:cntrl:]") ("digit" . "[:digit:]") ("graph" . "[:graph:]")
    ("lower" . "[:lower:]") ("print" . "[:print:]") ("punct" . "[:punct:]")
    ("space" . "[:space:]") ("upper" . "[:upper:]") ("xdigit" . "[:xdigit:]")
    ("d" . "[:digit:]") ("s" . "[:space:]") ("w" . "[:alnum:]_")))

(defconst elnix-regex--escapable ".[\\()*+?{|^$")

(defun elnix-regex--signed (c)
  "The byte C as a signed char, which is how libstdc++ orders ranges."
  (if (>= c 128) (- c 256) c))

(defun elnix-regex--bracket (re i)
  "Parse the bracket expression of RE that starts at I, the `['.
Return (EMACS-SET . NEXT-I)."
  (let ((n (length re)) (negate nil) (chars nil) (classes nil)
        (first t) (done nil)
        (prev nil))                     ; the last single character, for ranges
    (setq i (1+ i))
    (when (and (< i n) (eq (aref re i) ?^)) (setq negate t i (1+ i)))
    (cl-labels
        ((element ()
           ;; One element at I: (KIND . VALUE) with KIND char, class, coll
           ;; or equiv.  Moves I past it.
           (let ((c (aref re i)))
             (if (and (eq c ?\[) (< (1+ i) n) (memq (aref re (1+ i)) '(?: ?. ?=)))
                 (let* ((kind (aref re (1+ i)))
                        (close (string-search (string kind ?\]) re (+ i 2))))
                   (unless close (elnix-regex--invalid re "unterminated bracket element"))
                   (let ((name (substring re (+ i 2) close)))
                     (setq i (+ close 2))
                     (pcase kind
                       (?: (let ((cl (assoc name elnix-regex--classes)))
                             (unless cl (elnix-regex--invalid re "unknown class"))
                             (cons 'class (cdr cl))))
                       (_ (let ((code (cl-position name elnix-regex--collating-names
                                                   :test #'string=)))
                            (unless code
                              (elnix-regex--invalid re "unknown collating element"))
                            (cons (if (eq kind ?.) 'coll 'equiv) code))))))
               (setq i (1+ i))
               (cons 'char c)))))
      (while (not done)
        (when (>= i n) (elnix-regex--invalid re "unterminated bracket expression"))
        (let ((c (aref re i)))
          (cond
           ((and (eq c ?\]) (not first))
            (setq i (1+ i) done t))
           (t
            (let ((start i)
                  (e (element)))
              (ignore start)
              ;; A range: X-Y where the - is not last.
              (if (and (< (1+ i) n) (eq (aref re i) ?-) (not (eq (aref re (1+ i)) ?\])))
                  (progn
                    (unless (memq (car e) '(char coll))
                      (elnix-regex--invalid re "invalid range start"))
                    (setq i (1+ i))
                    (let ((e2 (element)))
                      (unless (eq (car e2) 'char)
                        (elnix-regex--invalid re "invalid range end"))
                      (when (> (elnix-regex--signed (cdr e)) (elnix-regex--signed (cdr e2)))
                        (elnix-regex--invalid re "invalid range"))
                      ;; A - after a range is an error: [a-c-e].
                      (when (and (< (1+ i) n) (eq (aref re i) ?-)
                                 (not (eq (aref re (1+ i)) ?\])))
                        (elnix-regex--invalid re "invalid range"))
                      (let ((lo (cdr e)) (hi (cdr e2)))
                        (if (and (< lo 128) (>= hi 128))
                            ;; Signed order: a range from ASCII into the
                            ;; high bytes is already rejected above.
                            nil
                          (cl-loop for x from lo to hi do (push x chars))))
                      (setq prev nil)))
                (pcase (car e)
                  ((or 'char 'coll 'equiv) (push (cdr e) chars) (setq prev (cdr e)))
                  ('class (push (cdr e) classes) (setq prev nil))))))))
        (setq first nil)))
    (ignore prev)
    ;; Write the Emacs set: ] first, - last, ^ not first.
    (let* ((uniq (delete-dups chars))
           (has-rb (memq ?\] uniq))
           (has-dash (memq ?- uniq))
           (has-caret (memq ?^ uniq))
           (rest (cl-remove-if (lambda (x) (memq x '(?\] ?- ?^))) uniq))
           (body (concat (if has-rb "]" "")
                         (apply #'unibyte-string (sort rest #'<))
                         (apply #'concat classes)
                         (if has-caret "^" "")
                         (if has-dash "-" ""))))
      (when (string= body "")
        (elnix-regex--invalid re "empty bracket expression"))
      (when (and (string= body "^") (not has-rb))
        ;; [^] alone would read as a negation in Emacs.
        (setq body "\\^"))
      (cons (if (string= body "\\^")
                (if negate "[^^]" "\\^")
              (concat "[" (if negate "^" "") body "]"))
            i))))

(defun elnix-regex--translate (re)
  "Parse the ERE RE.  Return (EMACS-REGEXP . GROUP-COUNT)."
  (let ((n (length re)) (i 0) (out nil) (depth 0) (groups 0)
        ;; Whether the previous item can take a quantifier.
        (atom nil))
    (while (< i n)
      (let ((c (aref re i)))
        (cond
         ((eq c ?\\)
          (when (>= (1+ i) n) (elnix-regex--invalid re "trailing backslash"))
          (let ((d (aref re (1+ i))))
            (unless (string-search (string d) elnix-regex--escapable)
              (elnix-regex--invalid re "invalid escape"))
            (push (if (memq d '(?. ?* ?+ ?? ?\[ ?^ ?$ ?\\))
                      (concat "\\" (string d))
                    (string d))
                  out)
            (setq i (+ i 2) atom t)))
         ((eq c ?\()
          (when (and (< (1+ i) n) (eq (aref re (1+ i)) ??))
            (elnix-regex--invalid re "invalid group"))
          (setq depth (1+ depth) groups (1+ groups))
          (push "\\(" out) (setq i (1+ i) atom nil))
         ((eq c ?\))
          (when (= depth 0) (elnix-regex--invalid re "unmatched parenthesis"))
          (setq depth (1- depth))
          (push "\\)" out) (setq i (1+ i) atom t))
         ((eq c ?|) (push "\\|" out) (setq i (1+ i) atom nil))
         ((memq c '(?* ?+ ??))
          (unless atom (elnix-regex--invalid re "nothing to repeat"))
          (push (string c) out) (setq i (1+ i)))
         ((eq c ?{)
          (unless atom (elnix-regex--invalid re "nothing to repeat"))
          (let ((close (string-search "}" re i)))
            (unless close (elnix-regex--invalid re "unterminated repetition"))
            (let ((body (substring re (1+ i) close)))
              (unless (string-match "\\`\\([0-9]+\\)\\(,\\([0-9]*\\)\\)?\\'" body)
                (elnix-regex--invalid re "invalid repetition"))
              (let ((lo (string-to-number (match-string 1 body)))
                    (hi (and (match-beginning 3) (> (length (match-string 3 body)) 0)
                             (string-to-number (match-string 3 body)))))
                (when (or (> lo 65535) (and hi (> hi 65535)))
                  (elnix-regex--invalid re "repetition too large"))
                (when (and hi (< hi lo))
                  (elnix-regex--invalid re "invalid repetition range")))
              (push (concat "\\{" body "\\}") out)
              (setq i (1+ close)))))
         ((eq c ?}) (push "}" out) (setq i (1+ i) atom t))
         ((eq c ?.) (push "[^\0]" out) (setq i (1+ i) atom t))
         ((eq c ?^) (push "\\`" out) (setq i (1+ i) atom nil))
         ((eq c ?$) (push "\\'" out) (setq i (1+ i) atom nil))
         ((eq c ?\[)
          (let ((r (elnix-regex--bracket re i)))
            (push (car r) out)
            (setq i (cdr r) atom t)))
         ((eq c ?\]) (push "]" out) (setq i (1+ i) atom t))
         (t (push (unibyte-string c) out) (setq i (1+ i) atom t)))))
    (unless (= depth 0) (elnix-regex--invalid re "unmatched parenthesis"))
    (let ((s (apply #'concat (nreverse out))))
      (cons (if (multibyte-string-p s) (string-to-unibyte s) s) groups))))

(defun elnix-regex-translate (re)
  "The Emacs regexp and group count of the POSIX extended expression RE."
  (or (gethash re elnix-regex--cache)
      (puthash re (elnix-regex--translate (elnix--unibyte re)) elnix-regex--cache)))

(defun elnix-regex--groups (s count)
  "The vector of the COUNT groups of the last match on S, null when unset."
  (let ((v (make-vector count :null)))
    (dotimes (i count)
      (when (match-beginning (1+ i))
        (aset v i (substring s (match-beginning (1+ i)) (match-end (1+ i))))))
    v))

(defun elnix-regex-match (re s)
  "`builtins.match' of RE on S."
  (let* ((case-fold-search nil)
         (tr (elnix-regex-translate re))
         (s (elnix--unibyte s)))
    (if (string-match (concat "\\`\\(?:" (car tr) "\\)\\'") s)
        (elnix-regex--groups s (cdr tr))
      :null)))

(defun elnix-regex-split (re s)
  "`builtins.split' of RE on S."
  (let* ((case-fold-search nil)
         (tr (elnix-regex-translate re))
         (regexp (car tr))
         (count (cdr tr))
         (s (elnix--unibyte s))
         (n (length s))
         (out nil)
         (prev 0)            ; end of the previous match
         (pos 0)             ; where the next search starts
         (found nil)
         (continue t))
    (while continue
      (if (not (and (<= pos n) (string-match regexp s pos)))
          (setq continue nil)
        (let ((mb (match-beginning 0)) (me (match-end 0)))
          (setq found t)
          (push (substring s prev mb) out)
          (push (elnix-regex--groups s count) out)
          (setq prev me)
          (if (/= mb me)
              (setq pos me)
            ;; After an empty match, std::regex_iterator looks for a
            ;; non-empty match starting at the same place, then moves on.
            (if (and (< me n)
                     (elnix-regex--nonempty-at regexp s me))
                (let ((me2 (match-end 0)))
                  (push "" out)
                  (push (elnix-regex--groups s count) out)
                  (setq prev me2 pos me2))
              (setq pos (1+ me))
              (when (> pos n) (setq continue nil)))))))
    (if (not found)
        (vector s)
      (push (substring s prev) out)
      (vconcat (nreverse out)))))

(defun elnix-regex--nonempty-at (regexp s pos)
  "Whether REGEXP has a non-empty match starting at POS in S.
On success the match data describes it, positions relative to S."
  (let ((sub (substring s pos)))
    ;; A non-empty match is the empty pattern ruled out at the start:
    ;; the first match the backtracking order finds among the matches
    ;; anchored at POS that consume a character.
    (when (string-match (concat "\\`\\(?:" regexp "\\)") sub)
      (if (> (match-end 0) 0)
          (progn
            (set-match-data (mapcar (lambda (x) (and x (+ x pos))) (match-data t)))
            t)
        ;; The first anchored match is empty; try for one that is not by
        ;; asking for at least one character.
        (let ((found nil) (e (length sub)))
          (while (and (not found) (> e 0))
            (when (string-match (concat "\\`\\(?:" regexp "\\)\\'") (substring sub 0 e))
              (setq found t))
            (unless found (setq e (1- e))))
          (when found
            (set-match-data (mapcar (lambda (x) (and x (+ x pos))) (match-data t))))
          found)))))

(provide 'elnix-regex)

;;; elnix-regex.el ends here
