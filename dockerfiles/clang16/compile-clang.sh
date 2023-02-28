set -ex
cd llvm-project
git checkout llvmorg-16.0.0-rc3
cmake -S llvm -B build -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Debug -DLLVM_ENABLE_PROJECTS="clang"
cmake --build build
