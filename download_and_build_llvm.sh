#!/usr/bin/env bash
set -e
version="$1"
if [ -z "$version" ]; then
    echo Missing version argument. >/dev/fd/2
    exit 1
fi
if [ "$version" "<" 3.5 ]; then
    suffix=.tar.gz
else
    suffix=.tar.xz
fi
case $version in
    3.5*)
        CC=gcc-4.8
        CXX=g++-4.8
        ;;
esac
wget http://releases.llvm.org/$version/llvm-$version.src$suffix
tar -xf llvm-$version.src$suffix
wget http://releases.llvm.org/$version/cfe-$version.src$suffix
tar -xf cfe-$version.src$suffix
mv cfe-$version.src llvm-$version.src/tools/clang
mkdir llvm-$version.build
pushd llvm-$version.build
    cmake ../llvm-$version.src -DCMAKE_C_COMPILER=$CC -DCMAKE_CXX_COMPILER=$CXX
    cmake --build .
popd