#!/bin/bash
set -e
version="`cat VERSION`"
tagname="v$version"
commit="`git rev-parse HEAD`"
git config --global user.email "Thierry.Martinez@inria.fr"
git config --global user.name "Thierry Martinez"
cd ~/clangml
git pull origin master
if [[ "`git rev-parse HEAD`" != "$commit" ]]; then
    echo "Too recent commit!"
    exit 1
fi
git log -1 --format=%B >commit_message
git pull origin releases
git merge origin/master
git fetch origin bootstrap
echo AM_MAINTAINER_MODE >>configure.ac
aclocal
autoreconf
./bootstrap.sh
git add configure.ac Makefile.in aclocal.m4 configure bootstrap
git commit -a -m "Version $version"
git tag -f -a "$tagname" -m "Version $version"
git push origin "$tagname"
archive="clangml-$tagname.tar.gz"
url="https://gitlab.inria.fr/tmartine/clangml/-/archive/$tagname/$archive"
wget "$url"
md5=`md5sum "$archive" | cut -d " " -f 1`
cd ~/opam-repository
git pull origin master
branch="clangml.$version"
git checkout -b "$branch"
repo="packages/clangml/clangml.$version"
mkdir -p "$repo"
opamfile="$repo/opam"
cp ~/clangml/clangml.opam "$opamfile"
cat >>$opamfile <<EOF
url {
  src: "$url"
  checksum: "md5=$md5"
}
EOF
git add "$opamfile"
git commit -F ~/clangml/commit_message
git push perso "$branch"
git checkout master
