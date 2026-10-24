;;; elnix-toml-test.el --- Tests of builtins.fromTOML -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Each expected text is what `nix-instantiate --eval --strict' of Nix
;; 2.34 prints for `builtins.fromTOML' of the same document.

;;; Code:

(require 'ert)
(require 'elnix)
(require 'elnix-toml)
(require 'elnix-print)

(defun elnix-toml-test--print (doc)
  "The printed Nix value of the TOML document DOC, decoded from UTF-8."
  (decode-coding-string
   (elnix-print-value (elnix-from-toml (encode-coding-string doc 'utf-8-unix t)) t)
   'utf-8-unix))

(defmacro elnix-toml-test--fails (doc)
  "Assert that parsing the TOML document DOC is an error."
  `(should-error (elnix-from-toml (encode-coding-string ,doc 'utf-8-unix t))
                 :type 'elnix-eval-error))

(ert-deftest elnix-toml-test-keys ()
  (should (equal (elnix-toml-test--print
                  "a.b.c = 1\na . d = 2\n\"q k\" = 3\n'' = 4\n3.14 = 5")
                 "{ \"\" = 4; \"3\" = { \"14\" = 5; }; a = { b = { c = 1; }; d = 2; }; \"q k\" = 3; }"))
  (elnix-toml-test--fails "a = 1\na = 2")
  (elnix-toml-test--fails "a.b = 1\na = 2")
  (elnix-toml-test--fails "\"\"\"a\"\"\" = 1"))

(ert-deftest elnix-toml-test-strings ()
  (should (equal (elnix-toml-test--print
                  "a = \"\\t\\u00e9\\U0001F600\"\nb = 'c:\\\\x'\nc = \"\"\"\nx \\\n   y\"\"\"\"\nd = '''\nl'''")
                 "{ a = \"\\té😀\"; b = \"c:\\\\\\\\x\"; c = \"x y\\\"\"; d = \"l\"; }"))
  (elnix-toml-test--fails "a = \"\\e\"")
  (elnix-toml-test--fails "a = \"\\u0000\"")
  (elnix-toml-test--fails "a = \"\\uD800\"")
  (elnix-toml-test--fails "a = \"line\nbreak\""))

(ert-deftest elnix-toml-test-numbers ()
  (should (equal (elnix-toml-test--print
                  "a = 0xff\nb = 0o17\nc = 0b101\nd = -1_000\ne = 1e3\nf = -inf\ng = nan\nh = 9223372036854775807")
                 "{ a = 255; b = 15; c = 5; d = -1000; e = 1000; f = -inf; g = nan; h = 9223372036854775807; }"))
  (elnix-toml-test--fails "a = 9223372036854775808")
  (elnix-toml-test--fails "a = 0x8000000000000000")
  (elnix-toml-test--fails "a = 012")
  (elnix-toml-test--fails "a = 1e400")
  (elnix-toml-test--fails "a = 1__0"))

(ert-deftest elnix-toml-test-tables ()
  (should (equal (elnix-toml-test--print
                  "[a.b]\nx = 1\n[a]\ny = [1, 'z', {w = true}]\n[[t]]\n[[t]]\nv = false")
                 "{ a = { b = { x = 1; }; y = [ 1 \"z\" { w = true; } ]; }; t = [ { } { v = false; } ]; }"))
  (should (equal (elnix-toml-test--print
                  "[f]\napple.color = 1\n[f.apple.texture]\nsmooth = 2")
                 "{ f = { apple = { color = 1; texture = { smooth = 2; }; }; }; }"))
  (elnix-toml-test--fails "[a]\n[a]")
  (elnix-toml-test--fails "[f]\napple.color = 1\n[f.apple]")
  (elnix-toml-test--fails "a = {b = 1}\na.c = 2")
  (elnix-toml-test--fails "a = [1]\n[[a]]")
  (elnix-toml-test--fails "a = { x = 1, }"))

(ert-deftest elnix-toml-test-dates ()
  (elnix-toml-test--fails "a = 1979-05-27T07:32:00Z")
  (elnix-toml-test--fails "a = 1979-05-27T07:32:00")
  (elnix-toml-test--fails "a = 1979-05-27")
  (elnix-toml-test--fails "a = 07:32:00"))

(defun elnix-toml-test--timestamp (text)
  "The printed value Nix with parse-toml-timestamps gives TOML value TEXT."
  (let* ((elnix-toml-timestamps t)
         (out (elnix-toml-test--print (concat "a = " text))))
    (if (string-match "\\`{ a = { _type = \"timestamp\"; value = \"\\(.*\\)\"; }; }\\'" out)
        (match-string 1 out)
      out)))

(ert-deftest elnix-toml-test-timestamps ()
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
    (should (equal (elnix-toml-test--timestamp (car c)) (cdr c))))
  (let ((elnix-toml-timestamps t))
    (elnix-toml-test--fails "a = 1979-02-29")
    (elnix-toml-test--fails "a = 24:00:00")
    (elnix-toml-test--fails "a = 23:59:61")
    (elnix-toml-test--fails "a = 1979-05-27T07:32:00+25:00")
    (elnix-toml-test--fails "a = 1979-05-27T07:32")
    (elnix-toml-test--fails "a = 07:32:00Z")))

(ert-deftest elnix-toml-test-builtin ()
  (should (equal (elnix-print-value
                  (elnix-eval-string "builtins.fromTOML \"a.b = [ 1 ]\"") t)
                 "{ a = { b = [ 1 ]; }; }")))

(provide 'elnix-toml-test)

;;; elnix-toml-test.el ends here
