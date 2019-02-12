#!/usr/bin/env bash
set -e
version="$1"
if [ -z "$version" ]; then
    echo Missing version argument. >/dev/fd/2
    exit 1
fi
if [ "$version" "<" 3.4.1 ]; then
    cfe=clang
else
    cfe=cfe
fi
if [ "$version" "<" 3.5 ]; then
    suffix=.tar.gz
else
    suffix=.tar.xz
fi
if [ "$version" "<" 3.6 ]; then
    if which -s gcc-4.9; then
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
wget http://releases.llvm.org/$version/llvm-$version.src$suffix
tar -xf llvm-$version.src$suffix
wget http://releases.llvm.org/$version/$cfe-$version.src$suffix
tar -xf $cfe-$version.src$suffix
mv $cfe-$version.src llvm-$version.src/tools/clang
mkdir llvm-$version.build
pushd llvm-$version.build
    cmake ../llvm-$version.src -DCMAKE_C_COMPILER=$CC -DCMAKE_CXX_COMPILER=$CXX
    cmake --build .
popd
