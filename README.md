# clangml: OCaml bindings for clang.

clangml provides bindings for all versions of clang, from 3.9.0 to
7.0.1.

## Installation

clangml is installable via `opam`: `opam install clangml`. 

Manual installation requires a bootstrapped source directory.
Commits from branch `releases` are bootstrapped.
To initialize the repository from a development branch (e.g., `master`),
execute `./bootstrap.sh`.

clangml's `configure` relies on `llvm-config` to find clang's library.
By default, `llvm-config` is searched in PATH, or you may
specify a path with `./configure --with-llvm-config=...'.

clangml requires some dependencies:
`opam install dune stdcompat ppx_deriving visitors`.
Additionnally, to run `make tests`: `opam install ocamlcodoc`.

## Usage

The module `Clang` provides direct bindings to most of the [symbols
defined by libclang][1] to match OCaml conventions, camel-case symbols
have been renamed to lower-case symbols with underscores, and `clang_`
prefixes have been removed. Additional bindings have been defined in
[`libclang_extensions.h`][2] for some parts of clang's API that have
not been covered by libclang.

[1]: https://clang.llvm.org/doxygen/group__CINDEX.html
[2]: https://gitlab.inria.fr/tmartine/clangml/blob/master/clangml/libclang_extensions.h

The module `Clang.Ast` provides a higher-level interface to clang's AST.

## Generating a new seed

Three files, `clang_stubs.c`, `clang__bindings.ml` and
`clang__bindings.mli`, are generated for each version of LLVM by the
`stubgen` tool (sub-directory stubgen).

To generate these files for a given version of LLVM, you may run:
`stubgen --llvm-config=$PATH_TO_LLVM_CONFIG $TARGET_PATH`.

`stubgen` depends on `pcre` and `cmdliner`.