# elnix: commands

Run from the project directory. `bin/elnix` runs `emacs --batch`; set
`ELNIX_EMACS` to use an Emacs that is not first on `PATH`, and
`ELNIX_CACHE_DIR` to move the module cache from `~/.cache/elnix`.

## Build

Byte-compile the Lisp files (the `.elc` files are git-ignored):

```zsh
emacs --batch -Q -L lisp -f batch-byte-compile lisp/*.el
```

## Evaluate

```zsh
bin/elnix eval -E '1 + 2'
bin/elnix eval --strict file.nix
bin/elnix eval --json -A hello.name /path/to/nixpkgs
bin/elnix drv-path -A hello /path/to/nixpkgs
```

`eval` takes the `nix-instantiate --eval` flags: `--strict`, `--json`,
`--xml`, `-A`, `--arg`, `--argstr`, `-I`, `--impure`, `--pure-eval` and
`--system`. Like `nix-instantiate`, it is impure unless given
`--pure-eval`.

## Inspect the compiled Lisp

```zsh
bin/elnix compile file.nix
```

prints the Lisp form a Nix file compiles to.

## Compare with Nix

```zsh
bin/elnix drv-paths --shard 0/24 pkgs.nix
bin/elnix compile-tree /path/to/nixpkgs 0/24
```

See [testing.md](testing.md).
