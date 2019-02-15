#!/usr/bin/env bash
set -ex
commit=$(git rev-parse HEAD)
git fetch origin
git checkout releases
git reset --hard origin/master
git reset --soft origin/releases
grep -q AM_MAINTAINER_MODE configure.ac || \
echo AM_MAINTAINER_MODE >>configure.ac
./bootstrap.sh
git add -f configure.ac Makefile.in aclocal.m4 configure bootstrap
if git commit -m "bootstrapped repository for commit $commit"; then
  git push origin HEAD:releases
  git tag -f -a "devel" -m "Development version"
  git push -f origin devel
fi
git checkout master
