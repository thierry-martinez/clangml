set -ex
cd llvm_project/build
cmake -DCMAKE_INSTALL_PREFIX=$HOME/llvm -P cmake_install.cmake
