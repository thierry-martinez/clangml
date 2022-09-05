(lang dune 3.0)

(generate_opam_files true)

(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (github "thierry-martinez/clangml"))
(homepage "https://github.com/thierry-martinez/clangml/")
(bug_reports "https://github.com/thierry-martinez/clangml/issues")
(documentation "https://github.com/thierry-martinez/clangml/blob/v4.8.0/README.md")
(version "4.8.0")

(package
 (name clangml)
 (synopsis "OCaml bindings for Clang API")
 (description "\
clangml provides bindings to call the Clang API from OCaml.
")
 (depends
  conf-libclang
  conf-ncurses
  conf-zlib
  (dune (>= 3.0))
  (stdcompat (>= 19))
  (ocaml (>= 4.08.0))
  (ocamlfind (and :build (>= 1.8.0)))
  (ocamlcodoc (and :with-test (>= 1.0.1)))
  (pattern (and :with-test (>= 0.2.0)))
  (ppxlib (>= 0.23))
  (metapp (>= 0.4.4))
  (metaquot (>= 0.4.0))
  (refl (>= 0.4.0))
  (odoc (and :with-doc (>= 1.5.1)))
  (ocaml-migrate-parsetree (>= 2.2.0))))
