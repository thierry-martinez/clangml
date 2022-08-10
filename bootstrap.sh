#!/usr/bin/env bash
set -e -x
remote="$1"
if [ -z "$remote" ]; then
	remote=origin
fi
rm -rf bootstrap
git checkout "$remote"/bootstrap bootstrap
git reset HEAD bootstrap
m4/download.sh
./autogen.sh
