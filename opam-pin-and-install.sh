#!/usr/bin/env bash
set -ex
URL="$1"
if [ -z "$URL" ]; then
    URL="file://$(readlink -f \"$0\")"
fi
opam update
opam pin add -yn "$URL"
opam depext -yi clangml
