#!/usr/bin/env bash
set -ex
version="$1"
if [ -z "$version" ]; then
    echo Missing version argument. >/dev/fd/2
    exit 1
fi
if [ "$version" "<" 3.4.1 ]; then
    cfe=clang
    dir_suffix=
else
    cfe=cfe
    dir_suffix=.src
fi
if [ "$version" "<" 3.5 ]; then
    suffix=.tar.gz
else
    suffix=.tar.xz
fi
if [ "$version" "<" 3.6 ]; then
    if which gcc-4.9 >/dev/null 2>/dev/null; then
        CC=gcc-4.9
        CXX+=g++-4.9
    else
        CC=gcc-4.8
        CXX=g++-4.8
    fi
else
    CC=gcc
    CXX=g++
fi
case "$version" in
*rc*)
    base_version="${version%rc*}"
    rc="${version#*rc}"
    base_url="https://prereleases.llvm.org/$base_version/rc$rc"
    ;;
*)
    base_url="http://releases.llvm.org/$version"
esac
wget "$base_url/llvm-$version.src$suffix"
tar -xf llvm-$version.src$suffix
wget "$base_url/$cfe-$version.src$suffix"
tar -xf $cfe-$version.src$suffix
mv $cfe-$version$dir_suffix llvm-$version$dir_suffix/tools/clang
mkdir llvm-$version.build
pushd llvm-$version.build
    cmake ../llvm-$version$dir_suffix \
        -DCMAKE_C_COMPILER=$CC -DCMAKE_CXX_COMPILER=$CXX -DLLVM_ENABLE_PIC=ON
    cmake --build .
popd