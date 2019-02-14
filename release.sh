#!/bin/bash
set -ex
version="`cat VERSION`"
tagname="v$version"
git config --global user.email "Thierry.Martinez@inria.fr"
git config --global user.name "Thierry Martinez"
current_dir="`pwd`"
[ -f commit_message ] || git log -1 --format=%B >commit_message
git checkout -B releases
git pull origin releases
git fetch origin master
git rebase origin/master
git fetch origin bootstrap
grep -q AM_MAINTAINER_MODE configure.ac || \
echo AM_MAINTAINER_MODE >>configure.ac
./bootstrap.sh
git add -f configure.ac Makefile.in aclocal.m4 configure bootstrap
git commit -a -m "Version $version"
git tag -f -a "$tagname" -m "Version $version"
git push -f origin "$tagname"
archive="clangml-$tagname.tar.gz"
url="https://gitlab.inria.fr/tmartine/clangml/-/archive/$tagname/$archive"
wget -O "$archive" "$url"
if which md5 >/dev/null; then
    md5=`md5 -q "$archive"`
else
    md5=`md5sum "$archive" | cut -d " " -f 1`
fi
git checkout master
cd ~/opam-repository
branch="clangml.$version"
git checkout "$branch" || ( git pull origin master && git checkout -b "$branch" )
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
#git commit -F "$current_dir/commit_message"
#git push perso "$branch"
#git checkout master
