(lang dune 1.10)

(generate_opam_files true)

(license BSD)
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://gitlab.inria.fr/memcad/clangml"))
(homepage "https://gitlab.inria.fr/memcad/clangml")
(bug_reports "https://gitlab.inria.fr/memcad/clangml")
(documentation "https://gitlab.inria.fr/memcad/clangml")

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
  (ocamlfind (and :build (>= 1.8.0)))
  (dune (>= 1.10.0))
  (ppxlib (>= 0.6.0))
  (stdcompat (>= 10))
  (ppx_show (>= 0.1.0))
  (ppx_compare (>= v0.12.0))
  (override (>= 0.1.0))
  (ocaml (>= 4.04.0))))
