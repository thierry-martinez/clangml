#!/bin/bash
set -e
version="`cat VERSION`"
tagname="v$version"
git config --global user.email "Thierry.Martinez@inria.fr"
git config --global user.name "Thierry Martinez"
current_dir="`pwd`"
[ -f commit_message ] || git log -1 --format=%B >commit_message
git pull origin releases
git rebase origin/master
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
git checkout "$branch" || git pull origin master && git pull origin master
branch="clangml.$version"
git checkout -b "$branch"
repo="packages/clangml/clangml.$version"
mkdir -p "$repo"
opamfile="$repo/opam"
cp "$current_dir/clangml.opam" "$opamfile"
cat >>"$opamfile" <<EOF
url {
  src: "$url"
  checksum: "md5=$md5"
}
EOF
git add "$opamfile"
git commit -F "$current_dir/commit_message"
git push perso "$branch"
git checkout master
