# nixel: product

nixel compiles Nix files to Emacs Lisp and evaluates them inside Emacs,
computing the same derivations Nix computes. It is the Emacs Lisp
counterpart of [jinx](https://github.com/fzakaria/jinx), which compiles
Nix to TypeScript; the design follows jinx where the two languages allow
it, and the code is written from the Nix semantics, not translated.

## Goal

For every attribute of nixpkgs, nixel either computes the `drvPath`
that Nix computes, or fails where Nix fails. The bar is measured by
running nixel and `nix-eval-jobs` over the same package set and joining
the results on the attribute path (see [testing.md](testing.md)).

## Why it is in this workspace

The editor is the resident shell of this workspace (the operating model
in `docs/development/agent-integration.md` of the workspace).
An evaluator that runs in the same Lisp image as the editor can answer
questions about the Nix code being edited (what a package's attributes
are, which derivation an expression produces) without starting a Nix
process, and it is a second, independent implementation of the
derivation hashing the trust work depends on.

## Scope

- Evaluation only. nixel computes `.drv` paths and output paths and
  never writes to the store; it does not build.
- Pure Nix semantics as Nix 2.34 has them: all builtins, string
  contexts, `derivationStrict`, path copying by NAR hash, fixed-output
  and input-addressed derivations.
- Out of scope: fetching (`fetchTarball`, `fetchGit`, `fetchTree` and
  the builtin `fetchurl` fail), `scopedImport`, content-addressed and
  impure derivations, import from derivation, flakes.
