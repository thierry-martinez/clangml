#!/usr/bin/env bash
set -ex
commit=$(git rev-parse HEAD)
git checkout snapshot
git reset --hard master
git reset --soft origin/snapshot
git clean -d --force -x
grep -q AM_MAINTAINER_MODE configure.ac || \
sed -i /AC_OUTPUT/iAM_MAINTAINER_MODE configure.ac
./bootstrap.sh
dune build clangml.opam
git add -f configure.ac Makefile.in aclocal.m4 configure bootstrap clangml.opam
if git commit -m "bootstrapped repository for commit $commit"; then
  git push origin snapshot
fi
git checkout master
