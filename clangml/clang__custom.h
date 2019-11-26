#include <caml/mlvalues.h>
#include <clang-c/Index.h>

int
clang_ext_compare_cursor(value v1, value v2);

intnat
clang_ext_hash_cursor(value v);
