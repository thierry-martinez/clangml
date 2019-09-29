#!/usr/bin/env bash
set -ex
commit=$(git rev-parse HEAD)
git checkout snapshot
git reset --hard master
git reset --soft origin/snapshot
grep -q AM_MAINTAINER_MODE configure.ac || \
sed -i /AC_OUTPUT/iAM_MAINTAINER_MODE configure.ac
( cd src && ./bootstrap.sh )
cp src/Makefile.in src/aclocal.m4 src/configure src/bootstrap src/clangml.opam .
git add -f configure.ac Makefile.in aclocal.m4 configure bootstrap clangml.opam
if git commit -m "bootstrapped repository for commit $commit"; then
  git push origin snapshot
fi
git checkout master
