;;; nixel-parse.el --- Nix lexer and parser -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A recursive-descent parser for the Nix language, producing an
;; abstract syntax tree of plain lists.  The source is held in a
;; unibyte buffer, so every string the parser produces is a byte
;; string, which is what a Nix string is.
;;
;; Every node is a list whose first element names the node kind and
;; whose second element is the node's position, a byte offset into the
;; source (1-based, a buffer position).  `nixel-parse-line-col' turns
;; an offset into a line number and a column.
;;
;;   (int POS N)                     integer literal
;;   (float POS F)                   float literal
;;   (str POS PARTS)                 string; PARTS are strings and nodes
;;   (path POS ANCHOR PARTS)         ANCHOR is relative, absolute, home
;;                                   or search
;;   (var POS NAME)                  NAME is a string
;;   (select POS SUBJECT ATTRPATH DEFAULT)   DEFAULT is a node or nil
;;   (has POS SUBJECT ATTRPATH)
;;   (lambda POS PARAM FORMALS BODY) PARAM is a string or nil; FORMALS
;;                                   is nil or (ENTRIES . ELLIPSIS),
;;                                   each entry (NAME POS DEFAULT)
;;   (call POS FN ARG)
;;   (list POS ELEMENTS)
;;   (attrs POS REC BINDINGS)
;;   (let POS BINDINGS BODY)
;;   (with POS SCOPE BODY)
;;   (assert POS CONDITION BODY)
;;   (if POS CONDITION THEN ELSE)
;;   (not POS OPERAND)
;;   (neg POS OPERAND)
;;   (binop POS OP LEFT RIGHT)       OP is the operator's string
;;   (inherit-from POS FROM NAME)    the value of `inherit (FROM) NAME'
;;
;; An attribute name in an ATTRPATH is (static POS NAME) or
;; (dynamic POS EXPR).  A binding is (ATTRNAME VALUE KIND), where KIND
;; is nil for `a = v;', `inherit' for `inherit a;' (VALUE is then the
;; variable reference) and `inherit-from' for `inherit (e) a;'.  All
;; names of one `inherit (e)' share the eq-identical node FROM.

;;; Code:

(require 'cl-lib)

(define-error 'nixel-error "Nix evaluation error")
(define-error 'nixel-parse-error "Nix syntax error" 'nixel-error)

(defvar nixel-parse--file nil
  "The name of the file being parsed, for error messages.")

(defvar nixel-parse--tok nil
  "The lookahead token, a vector [KIND TEXT BEG END], or nil.")

(defvar nixel-parse--line-starts nil
  "A vector of the buffer positions at which each line starts.")

(defconst nixel-parse--keywords
  '("if" "then" "else" "assert" "with" "let" "in" "rec" "inherit" "or"))

;; Longest first, because the scanner takes the first that matches.
(defconst nixel-parse--operators
  '("..." "==" "!=" "<=" ">=" "&&" "||" "->" "//" "++" "${" "|>" "<|"
    "." ";" "," "(" ")" "{" "}" "[" "]" "=" ":" "?" "!" "-" "+" "*" "/"
    "<" ">" "@"))

(defconst nixel-parse--re-id "[a-zA-Z_][a-zA-Z0-9_'-]*")
(defconst nixel-parse--re-int "[0-9]+")
(defconst nixel-parse--re-float
  "\\(?:[1-9][0-9]*\\.[0-9]*\\|0?\\.[0-9]+\\)\\(?:[Ee][+-]?[0-9]+\\)?")
(defconst nixel-parse--re-path
  "[a-zA-Z0-9._+-]*\\(?:/[a-zA-Z0-9._+-]+\\)+/?")
(defconst nixel-parse--re-path-seg "[a-zA-Z0-9._+-]*/")
(defconst nixel-parse--re-hpath "~\\(?:/[a-zA-Z0-9._+-]+\\)+/?")
(defconst nixel-parse--re-hpath-start "~/")
(defconst nixel-parse--re-spath
  "<[a-zA-Z0-9._+-]+\\(?:/[a-zA-Z0-9._+-]+\\)*>")
(defconst nixel-parse--re-uri
  "[a-zA-Z][a-zA-Z0-9+.-]*:[a-zA-Z0-9%/?:@&=+$,_.!~*'-]+")
(defconst nixel-parse--re-path-cont
  "[a-zA-Z0-9._+-]+\\(?:/[a-zA-Z0-9._+-]+\\)*/?\\|\\(?:/[a-zA-Z0-9._+-]+\\)+/?")
;; The text run of an indented string, after Nix's lexer rule
;; ([^$']|$[^{']|'[^'$])+.
(defconst nixel-parse--re-ind-text
  "\\(?:[^$']\\|\\$[^{']\\|'[^'$]\\)+")

;;;; Positions

(defun nixel-parse--compute-line-starts ()
  "Return a vector of the positions at which the current buffer's lines start."
  (save-excursion
    (goto-char (point-min))
    (let ((starts (list (point-min))))
      (while (search-forward "\n" nil t)
        (push (point) starts))
      (vconcat (nreverse starts)))))

(defun nixel-parse-line-col (pos &optional line-starts)
  "Return (LINE . COLUMN) of buffer position POS, both 1-based.
LINE-STARTS defaults to the table of the parse in progress."
  (let* ((starts (or line-starts nixel-parse--line-starts))
         (lo 0)
         (hi (1- (length starts))))
    ;; The last line whose start is at or before POS.
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (if (<= (aref starts mid) pos)
            (setq lo mid)
          (setq hi (1- mid)))))
    (cons (1+ lo) (1+ (- pos (aref starts lo))))))

(defun nixel-parse--fail (pos fmt &rest args)
  "Signal a parse error at POS with the message FMT and ARGS."
  (let ((lc (nixel-parse-line-col pos)))
    (signal 'nixel-parse-error
            (list (format "%s at %s:%d:%d"
                          (apply #'format fmt args)
                          (or nixel-parse--file "«string»")
                          (car lc) (cdr lc))))))

;;;; Lexer

(defun nixel-parse--skip-trivia ()
  "Move point past whitespace and comments."
  (let (done)
    (while (not done)
      (skip-chars-forward " \t\r\n")
      (cond
       ((eq (char-after) ?#)
        (skip-chars-forward "^\n"))
       ((and (eq (char-after) ?/) (eq (char-after (1+ (point))) ?*))
        (let ((start (point)))
          (forward-char 2)
          (unless (search-forward "*/" nil t)
            (nixel-parse--fail start "unterminated comment"))))
       (t (setq done t))))))

(defsubst nixel-parse--match-len (re)
  "The length of the match of RE at point, or 0."
  (if (looking-at re) (- (match-end 0) (point)) 0))

(defun nixel-parse--scan ()
  "Scan the next token at point and return it."
  (nixel-parse--skip-trivia)
  (let ((beg (point))
        (c (char-after)))
    (cond
     ((null c) (vector 'eof "" beg beg))
     ((eq c ?\")
      (forward-char 1)
      (vector 'string-start "\"" beg (point)))
     ((and (eq c ?') (eq (char-after (1+ beg)) ?'))
      (forward-char 2)
      ;; The opening quotes swallow the rest of their line when it is
      ;; blank, as Nix's lexer rule ''( *\n)? does.
      (when (looking-at " *\n")
        (goto-char (match-end 0)))
      (vector 'indent-start "''" beg (point)))
     ((not (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))
               (and (>= c ?0) (<= c ?9))
               (memq c '(?_ ?. ?/ ?~ ?< ?- ?+))))
      ;; No literal starts with this character: an operator.
      (nixel-parse--scan-op beg c))
     (t
      ;; Tokens that start with an identifier, number, path or URI
      ;; character compete on length, as the flex scanner decides.
      ;; Only the classes that can start with C are tried.
      (let ((best nil) (best-len 0)
            (letter (or (and (>= c ?a) (<= c ?z)) (and (>= c ?A) (<= c ?Z))))
            (digit (and (>= c ?0) (<= c ?9))))
        (dolist (cand (cond
                       (letter `((id . ,nixel-parse--re-id)
                                 (uri . ,nixel-parse--re-uri)))
                       ((eq c ?_) `((id . ,nixel-parse--re-id)))
                       (digit `((int . ,nixel-parse--re-int)
                                (float . ,nixel-parse--re-float)))
                       ((eq c ?.) `((float . ,nixel-parse--re-float)))
                       ((eq c ?<) `((spath . ,nixel-parse--re-spath)))
                       (t nil)))
          (let ((len (nixel-parse--match-len (cdr cand))))
            (when (> len best-len)
              (setq best (car cand) best-len len))))
        (let ((plen (max (nixel-parse--match-len nixel-parse--re-path)
                         (nixel-parse--match-len nixel-parse--re-hpath))))
          (when (> plen best-len)
            (setq best 'path-start best-len plen)))
        ;; `./${x}' and `~/${x}': a segment ending in a slash followed by
        ;; an interpolation starts a path too.
        (unless (eq best 'path-start)
          (let ((slen (max (nixel-parse--match-len nixel-parse--re-path-seg)
                           (nixel-parse--match-len
                            nixel-parse--re-hpath-start))))
            (when (and (> slen 0)
                       (> slen best-len)
                       (eq (char-after (+ beg slen)) ?$)
                       (eq (char-after (+ beg slen 1)) ?{))
              (setq best 'path-start best-len slen))))
        (cond
         ((eq best 'path-start)
          ;; The parser scans the path itself; the token only announces
          ;; it and is not consumed.
          (vector 'path-start "" beg beg))
         (best
          (let ((text (buffer-substring-no-properties beg (+ beg best-len))))
            (goto-char (+ beg best-len))
            (if (and (eq best 'id) (member text nixel-parse--keywords))
                (vector 'keyword text beg (point))
              (vector best text beg (point)))))
         (t (nixel-parse--scan-op beg c))))))))

(defconst nixel-parse--two-char-ops
  '("==" "!=" "<=" ">=" "&&" "||" "->" "//" "++" "${" "|>" "<|"))

(defconst nixel-parse--one-char-ops ".;,(){}[]=:?!-+*/<>@")

(defun nixel-parse--scan-op (beg c)
  "Scan the operator that starts with C at BEG."
  (let ((d (char-after (1+ beg))))
    (cond
     ((and (eq c ?.) (eq d ?.) (eq (char-after (+ 2 beg)) ?.))
      (forward-char 3)
      (vector 'op "..." beg (point)))
     ((and d (member (string c d) nixel-parse--two-char-ops))
      (forward-char 2)
      (vector 'op (string c d) beg (point)))
     ((string-search (string c) nixel-parse--one-char-ops)
      (forward-char 1)
      (vector 'op (string c) beg (point)))
     (t (nixel-parse--fail beg "unexpected character '%c'" c)))))

(defsubst nixel-parse--peek ()
  "Return the lookahead token, scanning it if necessary."
  (or nixel-parse--tok (setq nixel-parse--tok (nixel-parse--scan))))

(defsubst nixel-parse--next ()
  "Consume and return the lookahead token."
  (prog1 (nixel-parse--peek) (setq nixel-parse--tok nil)))

(defsubst nixel-parse--kind (tok) (aref tok 0))
(defsubst nixel-parse--text (tok) (aref tok 1))
(defsubst nixel-parse--beg (tok) (aref tok 2))

(defun nixel-parse--op-p (text)
  "Whether the lookahead token is the operator TEXT."
  (let ((tok (nixel-parse--peek)))
    (and (eq (aref tok 0) 'op) (string= (aref tok 1) text))))

(defun nixel-parse--keyword-p (text)
  "Whether the lookahead token is the keyword TEXT."
  (let ((tok (nixel-parse--peek)))
    (and (eq (aref tok 0) 'keyword) (string= (aref tok 1) text))))

(defun nixel-parse--describe (tok)
  "A description of TOK for an error message."
  (pcase (aref tok 0)
    ('eof "end of file")
    ('op (format "'%s'" (aref tok 1)))
    ('keyword (format "keyword '%s'" (aref tok 1)))
    ('string-start "a string")
    ('indent-start "an indented string")
    ('path-start "a path")
    (k (format "%s '%s'" k (aref tok 1)))))

(defun nixel-parse--expect-op (text)
  "Consume the operator TEXT or fail."
  (let ((tok (nixel-parse--next)))
    (unless (and (eq (aref tok 0) 'op) (string= (aref tok 1) text))
      (nixel-parse--fail (aref tok 2) "syntax error, expected '%s', got %s"
                         text (nixel-parse--describe tok)))
    tok))

(defun nixel-parse--expect-keyword (text)
  "Consume the keyword TEXT or fail."
  (let ((tok (nixel-parse--next)))
    (unless (and (eq (aref tok 0) 'keyword) (string= (aref tok 1) text))
      (nixel-parse--fail (aref tok 2) "syntax error, expected '%s', got %s"
                         text (nixel-parse--describe tok)))
    tok))

(defun nixel-parse--expect-id ()
  "Consume an identifier or fail."
  (let ((tok (nixel-parse--next)))
    (unless (eq (aref tok 0) 'id)
      (nixel-parse--fail (aref tok 2)
                         "syntax error, expected an identifier, got %s"
                         (nixel-parse--describe tok)))
    tok))

(defun nixel-parse--unescape (c)
  "The character an escape sequence of C stands for."
  (pcase c (?n ?\n) (?r ?\r) (?t ?\t) (_ c)))

(defun nixel-parse--scan-string ()
  "Scan the text of a double-quoted string up to `${' or the closing quote.
Return (TEXT . STOP) where STOP is `interp' or `end'."
  (let ((out nil) (stop nil))
    (while (not stop)
      (let ((run-start (point)))
        (skip-chars-forward "^\"\\\\$\r")
        (when (> (point) run-start)
          (push (buffer-substring-no-properties run-start (point)) out)))
      (let ((c (char-after)))
        (cond
         ((null c) (nixel-parse--fail (point) "unterminated string"))
         ((eq c ?\") (forward-char 1) (setq stop 'end))
         ((eq c ?\\)
          (let ((e (char-after (1+ (point)))))
            (unless e (nixel-parse--fail (point) "unterminated string"))
            (cond
             ;; Nix normalises a carriage return, alone or before a line
             ;; feed, to a line feed, escaped or not.
             ((eq e ?\r)
              (push "\n" out)
              (forward-char 2)
              (when (eq (char-after) ?\n) (forward-char 1)))
             (t
              (push (string (nixel-parse--unescape e)) out)
              (forward-char 2)))))
         ((eq c ?\r)
          (push "\n" out)
          (forward-char 1)
          (when (eq (char-after) ?\n) (forward-char 1)))
         ((eq c ?$)
          (let ((d (char-after (1+ (point)))))
            (cond
             ((eq d ?{) (forward-char 2) (setq stop 'interp))
             ;; `$$' is two literal dollars; it is how `$${' is written.
             ((eq d ?$) (push "$$" out) (forward-char 2))
             (t (push "$" out) (forward-char 1))))))))
    (cons (apply #'concat (nreverse out)) stop)))

(defun nixel-parse--scan-indented ()
  "Scan the text of an indented string up to `${' or the closing quotes.
Return (SEGMENTS . STOP).  Each segment is (TEXT . ESCAPED); an escaped
segment is one Nix's indentation stripping treats like an interpolation."
  (let ((segs nil) (stop nil))
    (while (not stop)
      (let ((c (char-after))
            (d (char-after (1+ (point)))))
        (cond
         ((null c) (nixel-parse--fail (point) "unterminated indented string"))
         ((and (eq c ?') (eq d ?'))
          (let ((e (char-after (+ 2 (point)))))
            (cond
             ((eq e ?$) (push (cons "$" t) segs) (forward-char 3))
             ((eq e ?') (push (cons "''" t) segs) (forward-char 3))
             ((eq e ?\\)
              (let ((f (char-after (+ 3 (point)))))
                (unless f
                  (nixel-parse--fail (point) "unterminated indented string"))
                (cond
                 ((eq f ?\r)
                  (push (cons "\n" t) segs)
                  (forward-char 4)
                  (when (eq (char-after) ?\n) (forward-char 1)))
                 (t
                  (push (cons (string (nixel-parse--unescape f)) t) segs)
                  (forward-char 4)))))
             (t (forward-char 2) (setq stop 'end)))))
         ((and (eq c ?$) (eq d ?{))
          (forward-char 2)
          (setq stop 'interp))
         ((looking-at nixel-parse--re-ind-text)
          (push (cons (match-string-no-properties 0) nil) segs)
          (goto-char (match-end 0)))
         ;; A lone `$' or `'' that no rule above takes is its own token
         ;; in Nix's lexer, without indentation.
         (t
          (push (cons (string c) t) segs)
          (forward-char 1)))))
    (cons (nreverse segs) stop)))

(defun nixel-parse--scan-path (first)
  "Scan a run of path text; FIRST is non-nil at the start of the path.
Return (TEXT . STOP)."
  (let ((len (if first
                 (max (nixel-parse--match-len nixel-parse--re-path)
                      (nixel-parse--match-len nixel-parse--re-path-seg)
                      (nixel-parse--match-len nixel-parse--re-hpath)
                      (nixel-parse--match-len nixel-parse--re-hpath-start))
               (nixel-parse--match-len nixel-parse--re-path-cont))))
    (let ((text (buffer-substring-no-properties (point) (+ (point) len))))
      (forward-char len)
      (cond
       ((and (eq (char-after) ?$) (eq (char-after (1+ (point))) ?{))
        (forward-char 2)
        (cons text 'interp))
       (t
        (when (and (> len 0) (eq (aref text (1- len)) ?/))
          (nixel-parse--fail (point) "path has a trailing slash"))
        (cons text 'end))))))

;;;; Parser

(defun nixel-parse--expr ()
  "Parse expr_function: lambdas, assert, with, let, then an if or operators."
  (let ((tok (nixel-parse--peek)))
    (pcase (aref tok 0)
      ('id
       (nixel-parse--next)
       (cond
        ((nixel-parse--op-p ":")
         (nixel-parse--next)
         (list 'lambda (aref tok 2) (aref tok 1) nil (nixel-parse--expr)))
        ((nixel-parse--op-p "@")
         (nixel-parse--next)
         (nixel-parse--expect-op "{")
         (let ((formals (nixel-parse--formals)))
           (nixel-parse--expect-op ":")
           (list 'lambda (aref tok 2) (aref tok 1) formals
                 (nixel-parse--expr))))
        (t (nixel-parse--continue-from
            (list 'var (aref tok 2) (aref tok 1))))))
      ('op
       (if (and (string= (aref tok 1) "{") (nixel-parse--formals-ahead-p))
           (progn
             (nixel-parse--next)
             (let ((formals (nixel-parse--formals))
                   (param nil))
               (when (nixel-parse--op-p "@")
                 (nixel-parse--next)
                 (setq param (aref (nixel-parse--expect-id) 1)))
               (nixel-parse--expect-op ":")
               (list 'lambda (aref tok 2) param formals (nixel-parse--expr))))
         (nixel-parse--binary 0)))
      ('keyword
       (pcase (aref tok 1)
         ("assert"
          (nixel-parse--next)
          (let ((c (nixel-parse--expr)))
            (nixel-parse--expect-op ";")
            (list 'assert (aref tok 2) c (nixel-parse--expr))))
         ("with"
          (nixel-parse--next)
          (let ((s (nixel-parse--expr)))
            (nixel-parse--expect-op ";")
            (list 'with (aref tok 2) s (nixel-parse--expr))))
         ("let"
          (nixel-parse--next)
          (if (nixel-parse--op-p "{")
              ;; The legacy `let { body = e; ... }' is (rec { ... }).body.
              (progn
                (nixel-parse--next)
                (let ((attrs (nixel-parse--attrs (aref tok 2) t)))
                  (list 'select (aref tok 2) attrs
                        (list (list 'static (aref tok 2) "body")) nil)))
            (let ((bindings (nixel-parse--bindings 'in)))
              (nixel-parse--expect-keyword "in")
              (dolist (b bindings)
                (when (eq (car (car b)) 'dynamic)
                  (nixel-parse--fail (nth 1 (car b))
                                     "dynamic attributes not allowed in let")))
              (list 'let (aref tok 2) bindings (nixel-parse--expr)))))
         ("if"
          (nixel-parse--next)
          (let ((c (nixel-parse--expr)))
            (nixel-parse--expect-keyword "then")
            (let ((then (nixel-parse--expr)))
              (nixel-parse--expect-keyword "else")
              (list 'if (aref tok 2) c then (nixel-parse--expr)))))
         (_ (nixel-parse--binary 0))))
      (_ (nixel-parse--binary 0)))))

(defun nixel-parse--formals-ahead-p ()
  "With `{' as the lookahead, whether the braces open lambda formals.
Decided from the tokens after the brace: `}' then `:' or `@', `...', or
an identifier followed by `,', `?' or `}'."
  (let ((saved-point (point))
        (saved-tok nixel-parse--tok))
    (unwind-protect
        (progn
          (nixel-parse--next)
          (let ((t1 (nixel-parse--next)))
            (cond
             ((and (eq (aref t1 0) 'op) (string= (aref t1 1) "}"))
              (let ((t2 (nixel-parse--next)))
                (and (eq (aref t2 0) 'op)
                     (member (aref t2 1) '(":" "@")))))
             ((and (eq (aref t1 0) 'op) (string= (aref t1 1) "..."))
              t)
             ((eq (aref t1 0) 'id)
              (let ((t2 (nixel-parse--next)))
                (and (eq (aref t2 0) 'op)
                     (member (aref t2 1) '("," "?" "}")))))
             (t nil))))
      (goto-char saved-point)
      (setq nixel-parse--tok saved-tok))))

(defun nixel-parse--continue-from (first)
  "Finish parsing an operator expression whose first simple term is FIRST."
  (let* ((selected (nixel-parse--select-rest first))
         (applied (nixel-parse--app-rest selected))
         (tested (nixel-parse--has-rest applied)))
    (nixel-parse--binary-rest tested 0)))

;; Binary operator levels, lowest precedence first, after Nix's
;; parser.y.  `!' sits at level 7, `?' and unary `-' above the last.
(defconst nixel-parse--levels
  [(("|>") . left)
   (("<|") . right)
   (("->") . right)
   (("||") . left)
   (("&&") . left)
   (("==" "!=") . none)
   (("<" ">" "<=" ">=") . none)
   (("//") . right)
   (("+" "-") . left)
   (("*" "/") . left)
   (("++") . right)])

(defconst nixel-parse--not-level 8)
(defconst nixel-parse--has-level (length nixel-parse--levels))
(defconst nixel-parse--neg-level (1+ (length nixel-parse--levels)))

(defun nixel-parse--binary (level)
  "Parse an operator expression whose operators bind at least as tight as LEVEL."
  (cond
   ((and (= level nixel-parse--not-level) (nixel-parse--op-p "!"))
    (let ((tok (nixel-parse--next)))
      (nixel-parse--level-rest
       (list 'not (aref tok 2) (nixel-parse--binary nixel-parse--not-level))
       level)))
   ((= level nixel-parse--neg-level)
    (if (nixel-parse--op-p "-")
        (let ((tok (nixel-parse--next)))
          (list 'neg (aref tok 2) (nixel-parse--binary nixel-parse--neg-level)))
      (nixel-parse--app)))
   ((= level nixel-parse--has-level)
    (nixel-parse--has-rest (nixel-parse--binary nixel-parse--neg-level)))
   (t
    (nixel-parse--level-rest (nixel-parse--binary (1+ level)) level))))

(defun nixel-parse--has-rest (left)
  "Parse any `? attrpath' suffixes of LEFT."
  (while (nixel-parse--op-p "?")
    (let ((tok (nixel-parse--next)))
      (setq left (list 'has (aref tok 2) left (nixel-parse--attrpath)))))
  left)

(defun nixel-parse--binary-rest (left level)
  "Continue an operator expression from LEFT, at levels LEVEL and up."
  (let ((l (1- (length nixel-parse--levels))))
    (while (>= l level)
      (setq left (nixel-parse--level-rest left l))
      (setq l (1- l))))
  left)

(defun nixel-parse--level-op (level)
  "The operator of LEVEL that is the lookahead token, or nil."
  (let ((tok (nixel-parse--peek)))
    (and (eq (aref tok 0) 'op)
         (car (member (aref tok 1) (car (aref nixel-parse--levels level)))))))

(defun nixel-parse--level-rest (left level)
  "Parse the operators of LEVEL that follow LEFT."
  (let* ((spec (aref nixel-parse--levels level))
         (assoc (cdr spec))
         (op (nixel-parse--level-op level)))
    (cond
     ((null op) left)
     ((eq assoc 'right)
      (let ((tok (nixel-parse--next)))
        (list 'binop (aref tok 2) op left (nixel-parse--binary level))))
     (t
      (while op
        (let ((tok (nixel-parse--next)))
          (setq left (list 'binop (aref tok 2) op left
                           (nixel-parse--binary (1+ level))))
          (setq op (nixel-parse--level-op level))
          (when (and op (eq assoc 'none))
            (nixel-parse--fail (aref (nixel-parse--peek) 2)
                               "syntax error, unexpected '%s'" op))))
      left))))

(defun nixel-parse--app ()
  "Parse a function application."
  (nixel-parse--app-rest (nixel-parse--select)))

(defun nixel-parse--simple-start-p ()
  "Whether the lookahead token can begin a simple expression."
  (let ((tok (nixel-parse--peek)))
    (pcase (aref tok 0)
      ((or 'id 'int 'float 'uri 'spath 'path-start 'string-start 'indent-start)
       t)
      ('keyword (string= (aref tok 1) "rec"))
      ('op (member (aref tok 1) '("(" "{" "[")))
      (_ nil))))

(defun nixel-parse--app-rest (fn)
  "Parse the arguments applied to FN."
  (while (nixel-parse--simple-start-p)
    (setq fn (list 'call (nth 1 fn) fn (nixel-parse--select))))
  fn)

(defun nixel-parse--select ()
  "Parse a simple expression and any selection from it."
  (nixel-parse--select-rest (nixel-parse--simple)))

(defun nixel-parse--select-rest (subject)
  "Parse a selection `.a.b or d' from SUBJECT, if one follows."
  (if (not (nixel-parse--op-p "."))
      subject
    (let* ((tok (nixel-parse--next))
           (path (nixel-parse--attrpath)))
      (if (nixel-parse--keyword-p "or")
          (progn
            (nixel-parse--next)
            (list 'select (aref tok 2) subject path (nixel-parse--select)))
        (list 'select (aref tok 2) subject path nil)))))

(defun nixel-parse--attrpath ()
  "Parse an attribute path."
  (let ((path (list (nixel-parse--attrname))))
    (while (nixel-parse--op-p ".")
      (nixel-parse--next)
      (push (nixel-parse--attrname) path))
    (nreverse path)))

(defun nixel-parse--static-string (parts)
  "The text of string PARTS when they hold no interpolation, else nil."
  (and (cl-every #'stringp parts) (apply #'concat parts)))

(defun nixel-parse--attrname ()
  "Parse one attribute name."
  (let ((tok (nixel-parse--peek)))
    (cond
     ((or (eq (aref tok 0) 'id)
          (and (eq (aref tok 0) 'keyword) (string= (aref tok 1) "or")))
      (nixel-parse--next)
      (list 'static (aref tok 2) (aref tok 1)))
     ((eq (aref tok 0) 'string-start)
      (nixel-parse--next)
      (let* ((parts (nixel-parse--string-parts))
             (static (nixel-parse--static-string parts)))
        (if static
            (list 'static (aref tok 2) static)
          (list 'dynamic (aref tok 2) (list 'str (aref tok 2) parts)))))
     ((and (eq (aref tok 0) 'op) (string= (aref tok 1) "${"))
      (nixel-parse--next)
      (let ((e (nixel-parse--expr)))
        (nixel-parse--expect-op "}")
        ;; `${"a"}' with a constant string is the static name a.
        (let ((static (and (eq (car e) 'str)
                           (nixel-parse--static-string (nth 2 e)))))
          (if static
              (list 'static (aref tok 2) static)
            (list 'dynamic (aref tok 2) e)))))
     (t (nixel-parse--fail (aref tok 2)
                           "syntax error, expected an attribute name, got %s"
                           (nixel-parse--describe tok))))))

(defun nixel-parse--simple ()
  "Parse a simple expression: a literal, variable, or bracketed form."
  (let* ((tok (nixel-parse--next))
         (pos (aref tok 2)))
    (pcase (aref tok 0)
      ('id (list 'var pos (aref tok 1)))
      ('int
       (let ((n (string-to-number (aref tok 1))))
         (when (> n 9223372036854775807)
           (nixel-parse--fail pos "invalid integer '%s'" (aref tok 1)))
         (list 'int pos n)))
      ('float (list 'float pos (float (string-to-number (aref tok 1)))))
      ('uri (list 'str pos (list (aref tok 1))))
      ('spath (list 'path pos 'search
                    (list (substring (aref tok 1) 1 -1))))
      ('string-start (list 'str pos (nixel-parse--string-parts)))
      ('indent-start (list 'str pos (nixel-parse--indented-parts)))
      ('path-start (nixel-parse--path pos))
      ('keyword
       (if (string= (aref tok 1) "rec")
           (progn
             (nixel-parse--expect-op "{")
             (nixel-parse--attrs pos t))
         (nixel-parse--fail pos "syntax error, unexpected %s"
                            (nixel-parse--describe tok))))
      ('op
       (pcase (aref tok 1)
         ("(" (prog1 (nixel-parse--expr) (nixel-parse--expect-op ")")))
         ("{" (nixel-parse--attrs pos nil))
         ("["
          (let ((elems nil))
            (while (not (nixel-parse--op-p "]"))
              (push (nixel-parse--select) elems))
            (nixel-parse--next)
            (list 'list pos (nreverse elems))))
         (_ (nixel-parse--fail pos "syntax error, unexpected %s"
                               (nixel-parse--describe tok)))))
      (_ (nixel-parse--fail pos "syntax error, unexpected %s"
                            (nixel-parse--describe tok))))))

(defun nixel-parse--string-parts ()
  "Parse the parts of a double-quoted string, after its opening quote."
  (let ((parts nil) (done nil))
    (while (not done)
      (let ((r (nixel-parse--scan-string)))
        (when (> (length (car r)) 0) (push (car r) parts))
        (if (eq (cdr r) 'end)
            (setq done t)
          (push (nixel-parse--expr) parts)
          (nixel-parse--expect-op "}"))))
    (nreverse parts)))

(defun nixel-parse--indented-parts ()
  "Parse the parts of an indented string, after its opening quotes."
  (let ((parts nil) (done nil))
    (while (not done)
      (let ((r (nixel-parse--scan-indented)))
        (dolist (s (car r)) (push s parts))
        (if (eq (cdr r) 'end)
            (setq done t)
          (push (nixel-parse--expr) parts)
          (nixel-parse--expect-op "}"))))
    (nixel-parse--strip-indentation (nreverse parts))))

(defun nixel-parse--strip-indentation (parts)
  "Apply Nix's indentation stripping to indented-string PARTS.
A part is (TEXT . ESCAPED) or an expression node.  The least
indentation of any line with content is removed from every line; a line
that starts with an interpolation or an escape counts with the spaces
before it; only spaces count; a final line of only spaces is dropped."
  (let ((at-start t) (min-indent most-positive-fixnum) (cur 0))
    (dolist (p parts)
      (if (or (not (stringp (car-safe p))) (cdr p))
          ;; An interpolation or an escaped character.
          (when at-start
            (setq at-start nil)
            (setq min-indent (min min-indent cur)))
        (let ((s (car p)))
          (dotimes (i (length s))
            (let ((c (aref s i)))
              (if at-start
                  (cond
                   ((eq c ?\s) (setq cur (1+ cur)))
                   ((eq c ?\n) (setq cur 0))
                   (t (setq at-start nil)
                      (setq min-indent (min min-indent cur))))
                (when (eq c ?\n)
                  (setq at-start t cur 0))))))))
    (let ((out nil) (dropped 0) (n (length parts)) (i 0))
      (setq at-start t)
      (cl-flet ((push-text (s)
                  (when (> (length s) 0)
                    (if (stringp (car out))
                        (setcar out (concat (car out) s))
                      (push s out)))))
        (dolist (p parts)
          (setq i (1+ i))
          (cond
           ((not (stringp (car-safe p)))
            (setq at-start nil dropped 0)
            (push p out))
           ((cdr p)
            (setq at-start nil dropped 0)
            (push-text (car p)))
           (t
            (let ((s (car p)) (acc nil))
              (dotimes (j (length s))
                (let ((c (aref s j)))
                  (if at-start
                      (cond
                       ((eq c ?\s)
                        (when (>= dropped min-indent) (push c acc))
                        (setq dropped (1+ dropped)))
                       ((eq c ?\n) (setq dropped 0) (push c acc))
                       (t (setq at-start nil dropped 0) (push c acc)))
                    (push c acc)
                    (when (eq c ?\n) (setq at-start t)))))
              (let ((str (apply #'unibyte-string (nreverse acc))))
                (when (= i n)
                  ;; Drop a final line of nothing but spaces.
                  (let ((nl (cl-position ?\n str :from-end t)))
                    (when (and nl (string-match-p "\\` *\\'"
                                                  (substring str (1+ nl))))
                      (setq str (substring str 0 (1+ nl))))))
                (push-text str)))))))
      (nreverse out))))

(defun nixel-parse--path (pos)
  "Parse a path literal starting at POS."
  (setq nixel-parse--tok nil)
  (goto-char pos)
  (let ((anchor 'relative) (parts nil) (first t) (done nil))
    (while (not done)
      (let ((r (nixel-parse--scan-path first)))
        (when first
          (let ((text (car r)))
            (cond
             ((and (> (length text) 0) (eq (aref text 0) ?/))
              (setq anchor 'absolute))
             ((and (> (length text) 0) (eq (aref text 0) ?~))
              (setq anchor 'home))))
          (setq first nil))
        (when (> (length (car r)) 0) (push (car r) parts))
        (if (eq (cdr r) 'end)
            (setq done t)
          (push (nixel-parse--expr) parts)
          (nixel-parse--expect-op "}"))))
    (list 'path pos anchor (nreverse parts))))

(defun nixel-parse--formals ()
  "Parse lambda formals, after the opening brace."
  (let ((entries nil) (ellipsis nil) (seen nil) (done nil))
    (while (and (not done) (not (nixel-parse--op-p "}")))
      (if (nixel-parse--op-p "...")
          (progn
            (nixel-parse--next)
            (setq ellipsis t done t)
            (unless (nixel-parse--op-p "}")
              (nixel-parse--fail (aref (nixel-parse--peek) 2)
                                 "syntax error, '...' must be last")))
        (let ((id (nixel-parse--expect-id)))
          (when (member (aref id 1) seen)
            (nixel-parse--fail (aref id 2)
                               "duplicate formal function argument '%s'"
                               (aref id 1)))
          (push (aref id 1) seen)
          (if (nixel-parse--op-p "?")
              (progn
                (nixel-parse--next)
                (push (list (aref id 1) (aref id 2) (nixel-parse--expr))
                      entries))
            (push (list (aref id 1) (aref id 2) nil) entries))
          (cond
           ((nixel-parse--op-p ",") (nixel-parse--next))
           ((not (nixel-parse--op-p "}"))
            (nixel-parse--fail (aref (nixel-parse--peek) 2)
                               "syntax error, expected ',' or '}', got %s"
                               (nixel-parse--describe (nixel-parse--peek))))))))
    (nixel-parse--expect-op "}")
    (cons (nreverse entries) ellipsis)))

(defun nixel-parse--attrs (pos recursive)
  "Parse an attribute set at POS after its brace; RECURSIVE for `rec'."
  (let ((bindings (nixel-parse--bindings '})))
    (nixel-parse--expect-op "}")
    (list 'attrs pos recursive bindings)))

;;;; Merging attribute paths

;; A merger collects the bindings of one set.  `a.b = 1; a.c = 2;' puts
;; b and c inside one set a; `a = { b = 1; }; a.c = 2;' extends the
;; literal set; naming an attribute twice otherwise is an error.
;; Dynamic names keep their order after the static ones.
;;
;; A merger is [TABLE STATICS DYNAMICS]: TABLE maps a name to its entry
;; [NAME BINDING NESTED RECURSIVE], STATICS lists the entries newest
;; first, and DYNAMICS the dynamic bindings, newest first.  The table
;; keeps a set of many thousand bindings, such as the Haskell package
;; set, from costing a scan per name.

(defun nixel-parse--merger ()
  (vector (make-hash-table :test 'equal) nil nil))

(defun nixel-parse--merger-find (m name)
  (gethash name (aref m 0)))

(defun nixel-parse--merger-push (m entry)
  (puthash (aref entry 0) entry (aref m 0))
  (aset m 1 (cons entry (aref m 1))))

(defun nixel-parse--wrap-path (path value)
  "Nest VALUE in single-attribute sets along PATH."
  (if (null path)
      value
    (let ((head (car path)))
      (list 'attrs (nth 1 head) nil
            (list (list head (nixel-parse--wrap-path (cdr path) value) nil))))))

(defun nixel-parse--merger-from-attrs (attrs)
  (let ((m (nixel-parse--merger)))
    (dolist (b (nth 3 attrs))
      (nixel-parse--merger-add m (list (car b)) (nth 1 b) (nth 2 b)))
    m))

(defun nixel-parse--merger-add (m path value kind)
  "Add to merger M the binding of PATH to VALUE, of KIND."
  (let ((head (car path))
        (rest (cdr path)))
    (if (eq (car head) 'dynamic)
        (aset m 2 (cons (list head (nixel-parse--wrap-path rest value) kind)
                        (aref m 2)))
      (let* ((name (nth 2 head))
             (existing (nixel-parse--merger-find m name)))
        (cond
         ((null rest)
          (when existing
            (if (and (aref existing 2) (eq (car value) 'attrs) (null kind))
                (progn
                  (dolist (b (nth 3 value))
                    (nixel-parse--merger-add (aref existing 2) (list (car b))
                                             (nth 1 b) (nth 2 b)))
                  (setq value nil))
              (nixel-parse--fail (nth 1 head) "attribute '%s' already defined"
                                 name)))
          (when value
            (nixel-parse--merger-push
             m (vector name (list head value kind)
                       (and (eq (car value) 'attrs) (null kind)
                            (nixel-parse--merger-from-attrs value))
                       (and (eq (car value) 'attrs) (nth 2 value))))))
         ((null existing)
          (let ((nested (nixel-parse--merger)))
            (nixel-parse--merger-add nested rest value kind)
            (nixel-parse--merger-push
             m (vector name
                       (list head (list 'attrs (nth 1 head) nil nil) nil)
                       nested nil))))
         ((null (aref existing 2))
          (nixel-parse--fail (nth 1 head) "attribute '%s' already defined"
                             name))
         (t (nixel-parse--merger-add (aref existing 2) rest value kind)))))))

(defun nixel-parse--merger-finish (m)
  "The bindings collected by merger M."
  (let ((out nil))
    (dolist (e (reverse (aref m 1)))
      (let ((binding (aref e 1))
            (nested (aref e 2)))
        (if (null nested)
            (push binding out)
          (let* ((value (nth 1 binding))
                 (pos (if (eq (car value) 'attrs)
                          (nth 1 value)
                        (nth 1 (car binding)))))
            (push (list (car binding)
                        (list 'attrs pos (aref e 3)
                              (nixel-parse--merger-finish nested))
                        (nth 2 binding))
                  out)))))
    (nconc (nreverse out) (reverse (aref m 2)))))

(defun nixel-parse--bindings (until)
  "Parse bindings up to the closing UNTIL, `}' or `in'."
  (let ((m (nixel-parse--merger)))
    (while (not (if (eq until '})
                    (nixel-parse--op-p "}")
                  (nixel-parse--keyword-p "in")))
      (if (nixel-parse--keyword-p "inherit")
          (progn
            (nixel-parse--next)
            (if (nixel-parse--op-p "(")
                (let ((tok (nixel-parse--next))
                      (from nil))
                  (setq from (nixel-parse--expr))
                  (nixel-parse--expect-op ")")
                  (ignore tok)
                  (while (not (nixel-parse--op-p ";"))
                    (let ((name (nixel-parse--attrname)))
                      (when (eq (car name) 'dynamic)
                        (nixel-parse--fail (nth 1 name)
                                           "dynamic attributes not allowed in inherit"))
                      (nixel-parse--merger-add
                       m (list name)
                       (list 'inherit-from (nth 1 name) from (nth 2 name))
                       'inherit-from))))
              (while (not (nixel-parse--op-p ";"))
                (let ((name (nixel-parse--attrname)))
                  (when (eq (car name) 'dynamic)
                    (nixel-parse--fail (nth 1 name)
                                       "dynamic attributes not allowed in inherit"))
                  (nixel-parse--merger-add
                   m (list name) (list 'var (nth 1 name) (nth 2 name))
                   'inherit))))
            (nixel-parse--expect-op ";"))
        (let ((path (nixel-parse--attrpath)))
          (nixel-parse--expect-op "=")
          (let ((value (nixel-parse--expr)))
            (nixel-parse--expect-op ";")
            (nixel-parse--merger-add m path value nil)))))
    (nixel-parse--merger-finish m)))

;;;; Entry points

(defun nixel-parse-buffer (&optional file)
  "Parse the current buffer, which must be unibyte, as a Nix expression.
FILE names the source in error messages.  Return (AST . LINE-STARTS)."
  (let ((case-fold-search nil)
        (nixel-parse--file file)
        (nixel-parse--tok nil)
        (nixel-parse--line-starts (nixel-parse--compute-line-starts)))
    (goto-char (point-min))
    (let ((e (nixel-parse--expr))
          (tok (nixel-parse--peek)))
      (unless (eq (aref tok 0) 'eof)
        (nixel-parse--fail (aref tok 2) "syntax error, unexpected %s"
                           (nixel-parse--describe tok)))
      (cons e nixel-parse--line-starts))))

(defun nixel-parse-file (file)
  "Parse FILE as a Nix expression.  Return (AST . LINE-STARTS)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (nixel-parse-buffer file)))

(defun nixel-parse-string (string &optional name)
  "Parse STRING as a Nix expression.  Return (AST . LINE-STARTS).
NAME names the source in error messages."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert (if (multibyte-string-p string)
                (encode-coding-string string 'utf-8)
              string))
    (nixel-parse-buffer name)))

(provide 'nixel-parse)

;;; nixel-parse.el ends here
