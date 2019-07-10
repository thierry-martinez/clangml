#!/usr/bin/env bash
set -ex

$build_dir/_build/default/tools/norm_extractor/norm_extractor.exe \
  --trigraphs -x c++ --std c++14 -i -o $build_dir/norm_cxx14.ml \
  `sed -n -e 's/^\\\\include{\\([^}]*\\)}/\\1.tex/p' std.tex`
