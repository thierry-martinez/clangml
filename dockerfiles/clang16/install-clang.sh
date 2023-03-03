set -ex
cd llvm-project/build
cmake -DCMAKE_INSTALL_PREFIX=$HOME/llvm -P cmake_install.cmake
