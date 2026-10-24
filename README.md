# elnix

elnix compiles Nix to Emacs Lisp and evaluates it, computing the same
derivations Nix computes. It is the Emacs Lisp counterpart of
[jinx](https://github.com/fzakaria/jinx), which does the same for
TypeScript, and follows the design jinx describes where the two
languages allow it.

```zsh
$ bin/elnix eval -E '(import <nixpkgs> { }).hello.drvPath'
"/nix/store/9xqckz64rmps2vcjbmzjlg3nc2nw2103-hello-2.12.3.drv"
```

Each Nix file compiles to one Emacs Lisp function. `bin/elnix compile
FILE` prints it:

```zsh
$ echo '{ a ? 1, b }: a + b' > f.nix && bin/elnix compile f.nix
```

The compiled functions are byte-compiled and cached by the SHA-256 of
their source, so a second evaluation of nixpkgs loads them instead of
compiling again.

## Requirements

Emacs 31 or later. `bin/elnix` runs `emacs --batch`; `ELNIX_EMACS`
names another Emacs and `ELNIX_CACHE_DIR` moves the cache from
`~/.cache/elnix`.

## From Emacs Lisp

```elisp
(require 'elnix)
(elnix-print-value (elnix-eval-string "builtins.map (x: x * 2) [ 1 2 3 ]") t)
;; => "[ 2 4 6 ]"
```

`elnix-eval-string`, `elnix-eval-file` and `elnix-import-path` return
elnix values; `docs/development/tech-stack.md` describes how each Nix
type is represented.

## What it does not do

Evaluation only: elnix never writes to the store and does not build.
Fetching builtins, `scopedImport`, flakes, import from derivation, and
content-addressed and impure derivations are errors. The known
deviations from Nix are listed in `docs/development/tech-stack.md`.

## Documentation

- `docs/development/product.md`: scope and goal
- `docs/development/tech-stack.md`: design and value representation
- `docs/development/testing.md`: how elnix is compared with Nix
- `docs/development/commands.md`: the command line

## License

MIT. The design follows jinx (MIT, Farid Zakaria); the code is written
for Emacs Lisp from the semantics of Nix.
