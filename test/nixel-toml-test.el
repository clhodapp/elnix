;;; nixel-toml-test.el --- Tests of builtins.fromTOML -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Each expected text is what `nix-instantiate --eval --strict' of Nix
;; 2.34 prints for `builtins.fromTOML' of the same document.

;;; Code:

(require 'ert)
(require 'nixel)
(require 'nixel-toml)
(require 'nixel-print)

(defun nixel-toml-test--print (doc)
  "The printed Nix value of the TOML document DOC, decoded from UTF-8."
  (decode-coding-string
   (nixel-print-value (nixel-from-toml (encode-coding-string doc 'utf-8-unix t)) t)
   'utf-8-unix))

(defmacro nixel-toml-test--fails (doc)
  "Assert that parsing the TOML document DOC is an error."
  `(should-error (nixel-from-toml (encode-coding-string ,doc 'utf-8-unix t))
                 :type 'nixel-eval-error))

(ert-deftest nixel-toml-test-keys ()
  (should (equal (nixel-toml-test--print
                  "a.b.c = 1\na . d = 2\n\"q k\" = 3\n'' = 4\n3.14 = 5")
                 "{ \"\" = 4; \"3\" = { \"14\" = 5; }; a = { b = { c = 1; }; d = 2; }; \"q k\" = 3; }"))
  (nixel-toml-test--fails "a = 1\na = 2")
  (nixel-toml-test--fails "a.b = 1\na = 2")
  (nixel-toml-test--fails "\"\"\"a\"\"\" = 1"))

(ert-deftest nixel-toml-test-strings ()
  (should (equal (nixel-toml-test--print
                  "a = \"\\t\\u00e9\\U0001F600\"\nb = 'c:\\\\x'\nc = \"\"\"\nx \\\n   y\"\"\"\"\nd = '''\nl'''")
                 "{ a = \"\\té😀\"; b = \"c:\\\\\\\\x\"; c = \"x y\\\"\"; d = \"l\"; }"))
  (nixel-toml-test--fails "a = \"\\e\"")
  (nixel-toml-test--fails "a = \"\\u0000\"")
  (nixel-toml-test--fails "a = \"\\uD800\"")
  (nixel-toml-test--fails "a = \"line\nbreak\""))

(ert-deftest nixel-toml-test-numbers ()
  (should (equal (nixel-toml-test--print
                  "a = 0xff\nb = 0o17\nc = 0b101\nd = -1_000\ne = 1e3\nf = -inf\ng = nan\nh = 9223372036854775807")
                 "{ a = 255; b = 15; c = 5; d = -1000; e = 1000; f = -inf; g = nan; h = 9223372036854775807; }"))
  (nixel-toml-test--fails "a = 9223372036854775808")
  (nixel-toml-test--fails "a = 0x8000000000000000")
  (nixel-toml-test--fails "a = 012")
  (nixel-toml-test--fails "a = 1e400")
  (nixel-toml-test--fails "a = 1__0"))

(ert-deftest nixel-toml-test-tables ()
  (should (equal (nixel-toml-test--print
                  "[a.b]\nx = 1\n[a]\ny = [1, 'z', {w = true}]\n[[t]]\n[[t]]\nv = false")
                 "{ a = { b = { x = 1; }; y = [ 1 \"z\" { w = true; } ]; }; t = [ { } { v = false; } ]; }"))
  (should (equal (nixel-toml-test--print
                  "[f]\napple.color = 1\n[f.apple.texture]\nsmooth = 2")
                 "{ f = { apple = { color = 1; texture = { smooth = 2; }; }; }; }"))
  (nixel-toml-test--fails "[a]\n[a]")
  (nixel-toml-test--fails "[f]\napple.color = 1\n[f.apple]")
  (nixel-toml-test--fails "a = {b = 1}\na.c = 2")
  (nixel-toml-test--fails "a = [1]\n[[a]]")
  (nixel-toml-test--fails "a = { x = 1, }"))

(ert-deftest nixel-toml-test-dates ()
  (nixel-toml-test--fails "a = 1979-05-27T07:32:00Z")
  (nixel-toml-test--fails "a = 1979-05-27T07:32:00")
  (nixel-toml-test--fails "a = 1979-05-27")
  (nixel-toml-test--fails "a = 07:32:00"))

(defun nixel-toml-test--timestamp (text)
  "The printed value Nix with parse-toml-timestamps gives TOML value TEXT."
  (let* ((nixel-toml-timestamps t)
         (out (nixel-toml-test--print (concat "a = " text))))
    (if (string-match "\\`{ a = { _type = \"timestamp\"; value = \"\\(.*\\)\"; }; }\\'" out)
        (match-string 1 out)
      out)))

(ert-deftest nixel-toml-test-timestamps ()
  (dolist (c '(("1979-05-27 07:32:00Z" . "1979-05-27T07:32:00Z")
               ("1979-05-27t07:32:00z" . "1979-05-27T07:32:00Z")
               ("1979-05-27T07:32:00-00:00" . "1979-05-27T07:32:00Z")
               ("1979-05-27T07:32:00+05:60" . "1979-05-27T07:32:00+06:00")
               ("1979-05-27T00:32:00.999999-07:00" . "1979-05-27T00:32:00.999999-07:00")
               ("1979-05-27T07:32:00.000Z" . "1979-05-27T07:32:00Z")
               ("1979-05-27t00:32:00.1234" . "1979-05-27T00:32:00.123400")
               ("1980-02-29" . "1980-02-29")
               ("07:32:00.5" . "07:32:00.500")
               ("07:32:00.100000" . "07:32:00.100")
               ("07:32:00.999999" . "07:32:00.999999")
               ("07:32:00.1234567" . "07:32:00.123456700")
               ("07:32:00.9999999999" . "07:32:00.999999999")
               ("23:59:60" . "23:59:60")))
    (should (equal (nixel-toml-test--timestamp (car c)) (cdr c))))
  (let ((nixel-toml-timestamps t))
    (nixel-toml-test--fails "a = 1979-02-29")
    (nixel-toml-test--fails "a = 24:00:00")
    (nixel-toml-test--fails "a = 23:59:61")
    (nixel-toml-test--fails "a = 1979-05-27T07:32:00+25:00")
    (nixel-toml-test--fails "a = 1979-05-27T07:32")
    (nixel-toml-test--fails "a = 07:32:00Z")))

(ert-deftest nixel-toml-test-builtin ()
  (should (equal (nixel-print-value
                  (nixel-eval-string "builtins.fromTOML \"a.b = [ 1 ]\"") t)
                 "{ a = { b = [ 1 ]; }; }")))

(provide 'nixel-toml-test)

;;; nixel-toml-test.el ends here
