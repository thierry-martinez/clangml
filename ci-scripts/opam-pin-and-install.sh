#!/usr/bin/env bash
set -ex
if [ "$#" -ne 1 ]; then
    >&2 echo "Usage: $0 URL"
    exit 1
fi
URL="$1"
git pull
opam update
opam pin add -yn "$URL"
opam depext -yi clangml
