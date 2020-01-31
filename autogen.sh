#!/usr/bin/env bash
set -e -x
aclocal
autoreconf
touch config/clangml_config.ml
dune build clangml.opam
