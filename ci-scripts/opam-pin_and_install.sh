#!/usr/bin/env bash
if [ "$#" -ne 1 ]; then
    >&2 echo "Usage: $0 URL"
    exit 1
fi
set -ex
URL="$1"
cd ~/opam-repository
git pull
opam update
opam pin add --yes --no-action https://gitlab.inria.fr/tmartine/override.git
opam pin add --yes --no-action https://gitlab.inria.fr/tmartine/pattern.git
opam pin add --yes --no-action "$URL"
opam depext --yes --install clangml
