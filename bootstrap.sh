#!/usr/bin/env bash
set -e -x
rm -rf bootstrap
git checkout origin/bootstrap bootstrap
git reset HEAD bootstrap
./autogen.sh
m4/download.sh
