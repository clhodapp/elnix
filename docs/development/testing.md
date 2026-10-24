# nixel: testing

Three levels, from a single builtin to all of nixpkgs. Each compares
nixel with Nix 2.34 itself.

## Unit tests (ERT)

`test/*-test.el` hold ERT tests, run with:

```zsh
emacs --batch -Q -L lisp -l test/nixel-toml-test.el -f ert-run-tests-batch-and-exit
```

## The language: nix-conformance

[numtide/nix-conformance](https://github.com/numtide/nix-conformance)
packages the language tests of Nix (`tests/functional/lang`) with the
answers of Nix 2.34.8. `test/conformance-adapter` implements its adapter
protocol for the `eval`, `eval-xml` and `check` operations and exits 3
(skip) for the others. From a checkout of nix-conformance:

```zsh
./run /path/to/nixel/test/conformance-adapter lang
```

The adapter runs `bin/nixel`, which finds Emacs through `NIXEL_EMACS` or
`PATH` and caches compiled modules in `NIXEL_CACHE_DIR`.

## nixpkgs: drv-paths against nix-eval-jobs

`nixel drv-paths FILE` walks the set FILE evaluates to as
`nix-eval-jobs` does (every top-level attribute, then sets with
`recurseForDerivations = true`) and prints one JSON line per attribute:
`attr`, then `drvPath` or `error`, then `ms`, the milliseconds since its
top-level attribute began. `--shard K/N` takes every Nth top-level
attribute starting with the Kth, so N processes cover the set together.

Running `nix-eval-jobs` and N nixel shards over the same file and
joining the two outputs on `attr` sorts every attribute into one of:
the same drvPath, a different drvPath, an error on one side only, or an
error on both. The bar is that only the first and the last occur.

Compile the tree first, so that the shards do not compile the same
files in parallel:

```zsh
nixel compile-tree /path/to/nixpkgs 0/1
```
