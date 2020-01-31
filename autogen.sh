#!/usr/bin/env bash
set -e -x
autoreconf
touch config/clangml_config.ml
dune build clangml.opam
