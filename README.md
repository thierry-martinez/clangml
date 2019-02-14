# clangml: OCaml bindings for clang.

clangml provides bindings for all versions of clang, from 3.4 to
8.0.0.

## Installation

clangml is installable via `opam`: `opam install clangml`. 

Manual installation requires a bootstrapped source directory.
Commits from branch `releases` are bootstrapped.
To initialize the repository from a development branch (e.g., `master`),
execute `./bootstrap.sh`.

clangml's `configure` relies on `llvm-config` to find clang's library.
By default, `llvm-config` is searched in PATH, or you may
specify a path with `./configure --with-llvm-config=...`.

clangml requires some dependencies:
`opam install dune stdcompat ppx_deriving visitors`.
Additionnally, to run `make tests`: `opam install ocamlcodoc`.

## Usage

The module [`Clang`][1] provides direct bindings to most of the [symbols
defined by libclang][2] to match OCaml conventions, camel-case symbols
have been renamed to lower-case symbols with underscores, and `clang_`
prefixes have been removed. Additional bindings have been defined in
[`libclang_extensions.h`][3] for some parts of clang's API that have
not been covered by libclang.

[1]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/index.html
[2]: https://clang.llvm.org/doxygen/group__CINDEX.html
[3]: https://gitlab.inria.fr/tmartine/clangml/blob/master/clangml/libclang_extensions.h

The module [`Clang.Ast`][4] provides a higher-level interface to clang's AST.
The function [`Clang.Ast.parse_file`][17] returns the AST from a file
and [`Clang.Ast.parse_string`][18] returns the AST from a string.
You may try these functions in OCaml toplevel to discover the resulting data
structure.

[17]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html#val-parse_file
[18]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html#val-parse_string

The module [`Clang.Ast`][4] includes in particular the module [`Clang__ast`][19]
which declares the algebraic data types that represent the AST.
The module [`Clang__ast`][19] uses [ppx_deriving][5] and [visitors][6] to make the
data structure comparable, showable and visitable.
The documentation of most of the nodes contains examples that can be used as references
for how syntactic constructions are parsed, and that are extracted with [ocamlcodoc][20]
and serve as unit tests with `dune runtest` (or, equivalently, `make tests`).

[19]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang__ast/index.html
[20]: https://gitlab.inria.fr/tmartine/ocamlcodoc

Modules [`Clang.Type`][7], [`Clang.Expr`][8], [`Clang.Stmt`][9],
[`Clang.Decl`][10] and [`Clang.Enum_constant`][11] provides sub-modules
`Set` and `Map` as well as high-level abstractions to some libclang's bindings.

[4]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html
[5]: https://github.com/ocaml-ppx/ppx_deriving
[6]: https://gitlab.inria.fr/fpottier/visitors
[7]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html
[8]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Expr/index.html
[9]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Stmt/index.html
[10]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Decl/index.html
[11]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Enum_constant/index.html

In particular:

- given an expression node `e : Clang.Expr.t`, the type of `e` can be obtained by [`Clang.Type.of_node e`][12];
- given a type `t : Clang.Type.t`, the alignment and the size of `t` can be obtained by [`Clang.Type.get_align_of t`][13] and [`Clang.Type.get_size_of t`][14] respectively;
- if `t : Clang.Type.t` is a `typedef`, the underlying type declared for `t` can be obtained by [`Clang.Type.get_typedef_underlying_type`][15]
- if `t : Clang.Type.t` is a record (`struct` or `union`), the list of fields can by [`Clang.Type.list_of_fields`][16];

[12]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-of_node
[13]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_align_of
[14]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_size_of
[15]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_typedef_underlying_type
[16]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-list_of_fields

## Generating a new seed

Three files, `clang_stubs.c`, `clang__bindings.ml` and
`clang__bindings.mli`, are generated for each version of LLVM by the
`stubgen` tool (sub-directory stubgen).

To generate these files for a given version of LLVM, you may run:
`stubgen --llvm-config=$PATH_TO_LLVM_CONFIG $TARGET_PATH`.

`stubgen` depends on `pcre` and `cmdliner`.