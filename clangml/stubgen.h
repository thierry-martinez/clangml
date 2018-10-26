#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

static void *
xmalloc(size_t size)
{
    void *ptr = malloc(size);
    if (!ptr) {
        failwith("Virtual memory exhausted");
    }
    return ptr;
}

static void
failwith_fmt(const char* format, ...)
{
    va_list argptr;
    ssize_t length = 255;
    char buffer[256];
/*
    ssize_t length;
    va_start(argptr, format);
    length = vsnprintf(NULL, 0, format, argptr);
    if (length < 0) {
      failwith("Unable to measure error format");
    }
    buffer = xmalloc(length + 1);
*/
    if (vsnprintf(buffer, length + 1, format, argptr) < 0) {
      failwith("Unable to format error");
    }
    va_end(argptr);
    failwith(buffer);
}

#define OCAML_OF_CXSTRING(S) caml_copy_string(clang_getCString(S))

#define DECLARE_OPAQUE(C_TYPE, OCAML_TYPE, C_OF_OCAML, OCAML_OF_C)      \
  struct custom_operations OCAML_TYPE##_ops = {                         \
    #C_TYPE,                                                            \
    custom_finalize_default,                                            \
    custom_compare_default,                                             \
    custom_compare_ext_default,                                         \
    custom_hash_default,                                                \
    custom_serialize_default,                                           \
    custom_deserialize_default                                          \
  };                                                                    \
                                                                        \
  static value                                                          \
  OCAML_OF_C(C_TYPE v)                                                  \
  {                                                                     \
    CAMLparam0();                                                       \
    CAMLlocal1(ocaml);                                                  \
    ocaml = caml_alloc_custom(                                          \
      &OCAML_TYPE##_ops, sizeof(C_TYPE), 100, 10000);                   \
    *((C_TYPE *) Data_custom_val(ocaml)) = v;                           \
    CAMLreturn(ocaml);                                                  \
  }                                                                     \
                                                                        \
  static C_TYPE                                                         \
  C_OF_OCAML(value ocaml)                                               \
  {                                                                     \
    CAMLparam1(ocaml);                                                  \
    CAMLreturnT(C_TYPE, *((C_TYPE *) Data_custom_val(ocaml)));          \
  }

#define Not_bool_val(X) (!Bool_val(X))

#define Val_not_bool(X) (Val_bool(!(X)))
