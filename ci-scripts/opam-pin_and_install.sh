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

## Pin dependencies

opam depext --yes --verbose --install dune

for package in metapp metaquot traverse refl pattern; do
    git clone https://github.com/thierry-martinez/"$package".git
    ( cd "$package" && dune build "$package".opam )
    opam pin add --yes --no-action -k path "$package"
done

opam pin add --yes --no-action "$URL"
# --allow-releaseinfo-change-suite to circumvent error
#   E: Repository 'http://deb.debian.org/debian buster InRelease' changed its 'Suite' value from 'stable' to 'oldstable'
# (see https://stackoverflow.com/questions/68802802/repository-http-security-debian-org-debian-security-buster-updates-inrelease)
sudo apt-get update --allow-releaseinfo-change-suite
opam depext --yes --verbose clangml
opam install --yes --with-test clangml
