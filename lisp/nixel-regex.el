;;; nixel-regex.el --- builtins.match and builtins.split -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Nix compiles its regular expressions with std::regex in POSIX
;; extended mode, as libstdc++ implements it.  This library translates
;; such an expression into Emacs regexp syntax and runs it on the
;; unibyte strings that Nix strings are.
;;
;; The translation:
;;   ( ) | { }        become \( \) \| \{ \}, groups numbered as in ERE
;;   \( \) \| \{ \} and other escapes   become the literal character
;;   .                becomes [^\0], since libstdc++ matches a newline
;;   ^ $              become \` \', anchors at the ends of the string
;;   [...]            is copied, a backslash inside it being literal in
;;                    both dialects
;;
;; `match' must cover the whole string; ordinary backtracking with both
;; ends anchored finds the same captures libstdc++ reports, because both
;; explore the alternatives of a pattern in the same depth-first order
;; and keep the first that succeeds.
;;
;; `split' searches, and libstdc++ in POSIX mode keeps the longest match
;; at the leftmost position where any match starts, with the captures of
;; the first path in that order to reach that length.  Backtracking
;; finds the leftmost start but may stop at a shorter match there, so
;; for a pattern whose matches at one place can differ in length, the
;; ends are tried from the longest down with both ends anchored; the
;; first that succeeds is the one libstdc++ reports.  (Emacs offers
;; `posix-string-match' for this, but Emacs 31.1 aborts in it on
;; patterns such as (a|ab)(c|bcd).)

;;; Code:

(require 'cl-lib)
(require 'nixel-runtime)

(defvar nixel-regex--cache (make-hash-table :test 'equal)
  "Translated regular expressions by source text.")

(defun nixel-regex--translate-bracket (re i out)
  "Copy the bracket expression of RE starting at I onto OUT.
Return (I . OUT) with I after the closing bracket."
  (let ((n (length re)) (start i))
    (setq i (1+ i))
    (when (and (< i n) (eq (aref re i) ?^)) (setq i (1+ i)))
    ;; A closing bracket first in the set is literal.
    (when (and (< i n) (eq (aref re i) ?\])) (setq i (1+ i)))
    (while (and (< i n) (not (eq (aref re i) ?\])))
      (if (and (eq (aref re i) ?\[) (< (1+ i) n)
               (memq (aref re (1+ i)) '(?: ?. ?=)))
          (let* ((kind (aref re (1+ i)))
                 (close (string-search (string kind ?\]) re (+ i 2))))
            (unless close (nixel-fail "invalid regex '%s': unterminated class" re))
            (setq i (+ close 2)))
        (setq i (1+ i))))
    (unless (< i n) (nixel-fail "invalid regular expression '%s'" re))
    (push (substring re start (1+ i)) out)
    (cons (1+ i) out)))

(defun nixel-regex-translate (re)
  "The Emacs regexp equivalent to the POSIX extended expression RE."
  (or (gethash re nixel-regex--cache)
      (puthash
       re
       (let ((n (length re)) (i 0) (out nil) (depth 0))
         (while (< i n)
           (let ((c (aref re i)))
             (cond
              ((eq c ?\\)
               (when (>= (1+ i) n)
                 (nixel-fail "invalid regular expression '%s': trailing backslash" re))
               (let ((d (aref re (1+ i))))
                 (push (if (memq d '(?. ?* ?+ ?? ?\[ ?\] ?^ ?$ ?\\))
                           (concat "\\" (string d))
                         (string d))
                       out))
               (setq i (+ i 2)))
              ((eq c ?\()
               (setq depth (1+ depth))
               (push "\\(" out) (setq i (1+ i)))
              ((eq c ?\))
               (setq depth (1- depth))
               (push "\\)" out) (setq i (1+ i)))
              ((eq c ?|) (push "\\|" out) (setq i (1+ i)))
              ((eq c ?{)
               (let ((close (string-search "}" re i)))
                 (if (and close (string-match-p "\\`[0-9]+\\(,[0-9]*\\)?\\'"
                                                (substring re (1+ i) close)))
                     (progn
                       (push (concat "\\{" (substring re (1+ i) close) "\\}") out)
                       (setq i (1+ close)))
                   (nixel-fail "invalid regular expression '%s': bad repetition" re))))
              ((eq c ?})
               (push "}" out) (setq i (1+ i)))
              ((eq c ?.) (push "[^\0]" out) (setq i (1+ i)))
              ((eq c ?^) (push "\\`" out) (setq i (1+ i)))
              ((eq c ?$) (push "\\'" out) (setq i (1+ i)))
              ((eq c ?\[)
               (let ((r (nixel-regex--translate-bracket re i out)))
                 (setq i (car r) out (cdr r))))
              (t (push (string c) out) (setq i (1+ i))))))
         (unless (= depth 0)
           (nixel-fail "invalid regular expression '%s': unbalanced parentheses" re))
         (let ((s (apply #'concat (nreverse out))))
           (if (multibyte-string-p s) (string-to-unibyte s) s)))
       nixel-regex--cache)))

(defun nixel-regex--groups (s count)
  "The vector of the COUNT groups of the last match on S, null when unset."
  (let ((v (make-vector count :null)))
    (dotimes (i count)
      (when (match-beginning (1+ i))
        (aset v i (substring s (match-beginning (1+ i)) (match-end (1+ i))))))
    v))

(defun nixel-regex--group-count (re)
  "The number of capture groups of the translated RE."
  (let ((count 0) (i 0) (n (length re)))
    (while (< i n)
      (cond
       ((and (eq (aref re i) ?\\) (< (1+ i) n) (eq (aref re (1+ i)) ?\())
        (setq count (1+ count) i (+ i 2)))
       ((eq (aref re i) ?\\) (setq i (+ i 2)))
       ((eq (aref re i) ?\[)
        ;; Skip the bracket expression.
        (setq i (1+ i))
        (when (and (< i n) (eq (aref re i) ?^)) (setq i (1+ i)))
        (when (and (< i n) (eq (aref re i) ?\])) (setq i (1+ i)))
        (while (and (< i n) (not (eq (aref re i) ?\])))
          (if (and (eq (aref re i) ?\[) (< (1+ i) n) (memq (aref re (1+ i)) '(?: ?. ?=)))
              (setq i (+ 2 (string-search (string (aref re (1+ i)) ?\]) re (+ i 2))))
            (setq i (1+ i))))
        (setq i (1+ i)))
       (t (setq i (1+ i)))))
    count))

(defun nixel-regex-match (re s)
  "`builtins.match' of RE on S."
  (let* ((case-fold-search nil)
         (tr (nixel-regex-translate re))
         (count (nixel-regex--group-count tr))
         (s (nixel--unibyte s)))
    (if (string-match (concat "\\`\\(?:" tr "\\)\\'") s)
        (nixel-regex--groups s count)
      :null)))

(defun nixel-regex--ambiguous-p (re)
  "Whether the matches of the ERE RE at one place can differ in length.
True when RE has an alternation or repeats a group; a pattern of
literals and repeated single characters or sets always has one longest
match, and backtracking finds it first."
  (let ((i 0) (n (length re)) (result nil))
    (while (and (< i n) (not result))
      (let ((c (aref re i)))
        (cond
         ((eq c ?\\) (setq i (+ i 2)))
         ((eq c ?\[)
          (setq i (car (nixel-regex--translate-bracket re i nil))))
         ((eq c ?|) (setq result t))
         ((and (eq c ?\)) (< (1+ i) n) (memq (aref re (1+ i)) '(?* ?+ ?? ?{)))
          (setq result t))
         (t (setq i (1+ i))))))
    result))

(defun nixel-regex--search (tr s pos ambiguous anchored)
  "Find the match libstdc++ reports for TR in S at or after POS.
AMBIGUOUS selects the search for the longest match at the leftmost
start; ANCHORED is TR anchored at both ends.  Return (BEG . END) with
the match data set, or nil."
  (when (string-match tr s pos)
    (let ((mb (match-beginning 0)) (me (match-end 0)))
      (if (not ambiguous)
          (cons mb me)
        (let ((e (length s)) (found nil))
          (while (and (not found) (> e me))
            (when (string-match anchored (substring s mb e))
              (setq found e))
            (setq e (1- e)))
          (if (not found)
              (progn (string-match tr s pos) (cons mb me))
            ;; Shift the match data of the substring back into S.
            (set-match-data (mapcar (lambda (x) (and x (+ x mb))) (match-data t)))
            (cons mb found)))))))

(defun nixel-regex-split (re s)
  "`builtins.split' of RE on S."
  (let* ((case-fold-search nil)
         (tr (nixel-regex-translate re))
         (anchored (concat "\\`\\(?:" tr "\\)\\'"))
         (ambiguous (nixel-regex--ambiguous-p re))
         (count (nixel-regex--group-count tr))
         (s (nixel--unibyte s))
         (n (length s))
         (out nil)
         (prev 0)            ; end of the previous match
         (pos 0)             ; where the next search starts
         (found nil)
         (continue t))
    (while continue
      (let ((m (and (<= pos n) (nixel-regex--search tr s pos ambiguous anchored))))
        (if (null m)
            (setq continue nil)
          (let ((mb (car m)) (me (cdr m)))
            (setq found t)
            (push (substring s prev mb) out)
            (push (nixel-regex--groups s count) out)
            (setq prev me)
            (if (/= mb me)
                (setq pos me)
              ;; After an empty match, std::regex_iterator looks for a
              ;; non-empty match at the same place, then moves on.
              (let ((m2 (and (< me n)
                             (nixel-regex--search tr s me ambiguous anchored))))
                (if (and m2 (= (car m2) me) (> (cdr m2) me))
                    (progn
                      (push (substring s prev (car m2)) out)
                      (push (nixel-regex--groups s count) out)
                      (setq prev (cdr m2) pos (cdr m2)))
                  (setq pos (1+ me))
                  (when (> pos n) (setq continue nil)))))))))
    (if (not found)
        (vector s)
      (push (substring s prev) out)
      (vconcat (nreverse out)))))

(provide 'nixel-regex)

;;; nixel-regex.el ends here
