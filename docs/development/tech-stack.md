# nixel: tech stack

Emacs Lisp on Emacs 31 or later, with lexical binding throughout. No
packages outside Emacs itself.

## Pipeline

| Stage | File | What it does |
|---|---|---|
| Parse | `lisp/nixel-parse.el` | Nix source in a unibyte buffer to a syntax tree of plain lists |
| Compile | `lisp/nixel-compile.el` | Syntax tree to one Lisp form per file: a function of the file's directory and name |
| Cache | `lisp/nixel.el` | The form byte-compiled to `NIXEL_CACHE_DIR`, keyed by the SHA-256 of the compiler and the source |
| Run | `lisp/nixel-runtime.el`, `lisp/nixel-builtins.el` | The values and operations compiled code calls |

## Values

| Nix | Emacs Lisp |
|---|---|
| int | integer, checked to the signed 64-bit range after each operation |
| float | float |
| bool | `t` and `nil` |
| null | `:null` |
| string | unibyte string, or a `nixel--cstr` record when it has a context |
| path | `nixel--path` record |
| list | vector |
| attrs | `nixel--attrs` record around an `eq` hash table keyed by interned symbols |
| lambda | closure of one argument; `nixel--lam` record when it has formals |
| primop | `nixel--primop` record holding the function, arity and arguments so far |

A thunk is a cons cell, because no value is one: `consp` is a bytecode
opcode, so forcing a value that is not a thunk costs one type check.
`false` is `nil` so that a compiled `if` tests a Boolean directly; lists
are vectors so that `nil` never has to mean the empty list.

Strings are unibyte so that `stringLength`, `substring`, regular
expressions and hashing all act on bytes, as they do in Nix. Module
files are read back as raw bytes, which makes a string literal holding
bytes outside ASCII multibyte again, so the compiler wraps such literals
in `string-to-unibyte`.

## Decisions

- **A thunk that fails is put back.** Nix resets a thunk whose
  evaluation throws, so a later force throws the same error rather than
  reporting infinite recursion. nixel keeps the stack of thunks being
  forced in `nixel--forcing`, and the handlers that catch errors
  (`tryEval`, the per-attribute loop of `drv-paths`) restore the thunks
  above their own depth. This costs a cons per force instead of a
  `condition-case`, which would cost a `setjmp`.
- **Recursive bindings are found by marking scopes.** A `let` or `rec`
  compiles to a plain `let` unless a lookup of one of its names resolved
  through its scope frame while its values were compiled; then it
  compiles to a `let` of the names and a `setq` per value.
- **Globals win over `with`.** The builtins are lexical bindings of the
  outermost scope in Nix, so `with { map = 1; }; map` is still
  `builtins.map`.
- **`posix-string-match` is not used.** Emacs 31.1 aborts in it on
  patterns such as `(a|ab)(c|bcd)`. `split` finds the longest match at
  the leftmost start by anchored matching instead; see the commentary of
  `lisp/nixel-regex.el`.
- **The `derivation` builtin is Nix code** (`corepkgs/derivation.nix`),
  as it is in Nix. `<nix/fetchurl.nix>` resolves to
  `corepkgs/fetchurl.nix`.

## Known deviations

- A lambda with no free variables is a byte-compiled constant, so two
  evaluations of the same `x: x` are one value and `[ (x: x) ] == [ (x:
  x) ]` is true; in Nix it is false.
- Nix stores lists of up to two elements inside each value, so a copy of
  such a list is a new list to the printer's `«repeated»` check; nixel
  prints `«repeated»` one level earlier in those cases.
- `parseFlakeRef`, `flakeRefToString` and BLAKE3 hashes are not
  implemented.
