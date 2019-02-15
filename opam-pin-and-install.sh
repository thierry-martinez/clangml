#!/usr/bin/env bash
set -ex
URL="$1"
( cd opam-repository && git pull )
opam update
opam pin add -yn "$URL"
opam depext -yi clangml
