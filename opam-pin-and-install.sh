#!/usr/bin/env bash
set -ex
URL="$1"
opam update
opam pin add -yn "$URL"
opam depext -yi clangml
