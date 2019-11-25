#!/usr/bin/env bash
set -e -x
autoreconf
rm -rf bootstrap
git checkout origin/bootstrap bootstrap
git reset HEAD bootstrap
touch config/clangml_config.ml
dune build clangml.opam
