# elnix: testing

Three levels, from a single builtin to all of nixpkgs. Each compares
elnix with Nix 2.34 itself.

## Unit tests (ERT)

`test/*-test.el` hold ERT tests, run with:

```zsh
emacs --batch -Q -L lisp -l test/elnix-toml-test.el -f ert-run-tests-batch-and-exit
```

## The language: nix-conformance

[numtide/nix-conformance](https://github.com/numtide/nix-conformance)
packages the language tests of Nix (`tests/functional/lang`) with the
answers of Nix 2.34.8. `test/conformance-adapter` implements its adapter
protocol for the `eval`, `eval-xml` and `check` operations and exits 3
(skip) for the others. From a checkout of nix-conformance:

```zsh
./run /path/to/elnix/test/conformance-adapter lang
```

The adapter runs `bin/elnix`, which finds Emacs through `ELNIX_EMACS` or
`PATH` and caches compiled modules in `ELNIX_CACHE_DIR`.

## nixpkgs: drv-paths against nix-eval-jobs

`elnix drv-paths FILE` walks the set FILE evaluates to as
`nix-eval-jobs` does (every top-level attribute, then sets with
`recurseForDerivations = true`) and prints one JSON line per attribute:
`attr`, then `drvPath` or `error`, then `ms`, the milliseconds since its
top-level attribute began. `--shard K/N` takes every Nth top-level
attribute starting with the Kth, so N processes cover the set together.

Running `nix-eval-jobs` and N elnix shards over the same file and
joining the two outputs on `attr` sorts every attribute into one of:
the same drvPath, a different drvPath, an error on one side only, or an
error on both. The bar is that only the first and the last occur.

Large recursed sets (`python313Packages`, `haskellPackages` and the
like) are shared between shards with `--split NAME,...`: every shard
walks them and evaluates only the children whose path hashes to it.
`--sub J/M` reruns part of one shard.

### Result on nixpkgs 5adc5083 (2026-10-24)

The package set was `import nixpkgs { system = "x86_64-linux"; config
= { allowAliases = false; allowUnfree = true; }; }`. `nix-eval-jobs`
(Nix 2.34) reported 113,175 attributes. For all 98,630 attributes that
Nix evaluates to a derivation, elnix computes the same `drvPath`. The
other 14,545 attributes fail in both. No attribute differs, and none
fails in only one of the two.

The elnix run used 256 shards, at most 8 processes at a time, each held
to 5 GB of address space by `ulimit -v`. elnix needs about four times
the memory Nix needs for the same evaluation (2.1 GB against a 436 MB
heap for `haskell.package-list`), because each thunk is a closure with
its own constant vector and each set is a hash table.

Compile the tree first, so that the shards do not compile the same
files in parallel:

```zsh
elnix compile-tree /path/to/nixpkgs 0/1
```
