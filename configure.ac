AC_INIT([[clangml]], [1], [[Thierry.Martinez@inria.fr]], [[clangml]],
  [[https://github.com/thierry.martinez/clangml/]])
AM_INIT_AUTOMAKE

AC_PROG_LN_S

AC_MSG_CHECKING([for bootstrap/ sub-directory])
AS_IF([test -d $srcdir/bootstrap],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])
   AC_MSG_ERROR([bootstrap/ sub-directory not found. Please run $srcdir/bootstrap.sh first.])])

AC_DEFUN([AC_OCAMLFIND_PKG],
  [AC_MSG_CHECKING([for ocamlfind package $2])
   AS_IF([$OCAMLFIND query $2 >/dev/null 2>&1],
     [AC_MSG_RESULT([yes])
      AC_SUBST([$1], [$2])],
     [AC_MSG_RESULT([no])
      AC_SUBST([$1], [])])])

AC_DEFUN([AC_CHECK_OCAMLFIND_PKG],
  [AC_MSG_CHECKING([for ocamlfind package $2])
   AS_IF([$OCAMLFIND query $2 >/dev/null 2>&1],
     [AC_MSG_RESULT([yes])
      AC_SUBST([$1], [$2])],
     [AC_MSG_ERROR([not found])])])

AC_CHECK_TOOL([OCAMLFIND], [ocamlfind], [])
AS_IF([test "x$OCAMLFIND" = "x"],
  [AC_CHECK_TOOL([OCAMLC], [ocamlc.opt], [])
   AS_IF([test "x$OCAMLC" = "x"],
     [AC_CHECK_TOOL([OCAMLC], [ocamlc], [])])
   AC_CHECK_TOOL([OCAMLOPT], [ocamlopt.opt], [])
   AS_IF([test "x$OCAMLOPT" = "x"],
     [AC_CHECK_TOOL([OCAMLOPT], [ocamlopt], [])])
   AC_CHECK_TOOL([OCAMLDOC], [ocamldoc.opt], [])
   AS_IF([test "x$OCAMLDOC" = "x"],
     [AC_CHECK_TOOL([OCAMLDOC], [ocamldoc], [])])
   AC_CHECK_TOOL([OCAMLDEP], [ocamldep.opt], [])
   AS_IF([test "x$OCAMLDEP" = "x"],
     [AC_CHECK_TOOL([OCAMLDEP], [ocamldep], [])])
   AC_CHECK_TOOL([OCAMLMKLIB], [ocamlmklib.opt], [])
   AS_IF([test "x$OCAMLMKLIB" = "x"],
     [AC_CHECK_TOOL([OCAMLMKLIB], [ocamlmklib], [])])
   AC_SUBST([RESULT_PKG], [])
   AC_SUBST([SEQ_PKG], [])
   AC_SUBST([UCHAR_PKG], [])],
  [AC_SUBST([OCAMLC], [["$OCAMLFIND ocamlc"]])
   AC_SUBST([OCAMLOPT], [["$OCAMLFIND ocamlopt"]])
   AC_SUBST([OCAMLDOC], [["$OCAMLFIND ocamldoc"]])
   AC_SUBST([OCAMLDEP], [["$OCAMLFIND ocamldep"]])
   AC_SUBST([OCAMLMKLIB], [["$OCAMLFIND ocamlmklib"]])
   AC_CHECK_OCAMLFIND_PKG([stdcompat])])

AS_IF([test "x$OCAMLOPT" = "x"],
  [AS_IF([test "x$OCAMLC" = "x"],
     [AC_MSG_ERROR([[No OCaml compiler available]])],
     [AC_SUBST([OCAMLBEST], [["$OCAMLC"]])
      AC_SUBST([CMOX], [["cmo"]])
      AC_SUBST([CMAX], [["cma"]])])],
  [AC_SUBST([OCAMLBEST], [["$OCAMLOPT"]])
   AC_SUBST([CMOX], [["cmx"]])
   AC_SUBST([CMAX], [["cmxa"]])])

AS_IF([test "x$OCAMLDEP" = "x"],
  [AC_MSG_ERROR([[No ocamldep available]])])

AC_MSG_CHECKING([OCaml version])
AC_SUBST([OCAMLVERSION],
  [[`$OCAMLBEST -v | sed -n 's/^.*version \(.*\)$/\1/p'`]])
AC_MSG_RESULT([$OCAMLVERSION])

AM_CONDITIONAL(OCAML_SUPPORTS_BYTECODE, [test "x$OCAMLC" != "x"])

AM_CONDITIONAL(OCAML_SUPPORTS_NATIVE, [test "x$OCAMLOPT" != "x"])

AC_CHECK_TOOL([DUNE], [dune], [])

AS_IF([test "x$DUNE" = "x"],
  [AC_MSG_ERROR([[dune not available]])])

AC_CHECK_TOOL([OCAMLCODOC], [ocamlcodoc], [])

AC_CHECK_TOOL([INSTALL_NAME_TOOL], [install_name_tool], [])

AC_MSG_CHECKING([for llvm-config])
AC_ARG_WITH([llvm-config],
  AS_HELP_STRING([--with-llvm-config[=PATH]],
    [Set the path to the llvm-config tool.]),
  [AC_SUBST([LLVM_CONFIG], [$with_llvm_config])
   AC_MSG_RESULT([[$LLVM_CONFIG (--with-llvm-config=)]])],
  [AC_SUBST([LLVM_CONFIG], [[`which llvm-config`]])
   AS_IF([test "x$LLVM_CONFIG" = "x"],
     [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-7`]])
      AS_IF([test "x$LLVM_CONFIG" = "x"],
        [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-6`]])
         AS_IF([test "x$LLVM_CONFIG" = "x"],
           [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-5`]])
            AS_IF([test "x$LLVM_CONFIG" = "x"],
              [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-4`]])
               AS_IF([test "x$LLVM_CONFIG" = "x"],
                 [AC_SUBST([LLVM_CONFIG], [[`which llvm-config-3`]])])])])])])
   AS_IF([test "x$LLVM_CONFIG" = "x"],
     [AC_MSG_RESULT([[not found in PATH]])
      AC_MSG_ERROR(
        [[llvm-config not available (you may use --with-llvm-config=)]])])
   AC_MSG_RESULT([[$LLVM_CONFIG (found in PATH)]])])

AC_MSG_CHECKING([llvm-config version])
AC_SUBST([LLVM_VERSION], [[`$LLVM_CONFIG --version`]])
AS_IF([test "x$LLVM_VERSION" = "x"],
  [AC_MSG_ERROR([[unable to run $LLVM_CONFIG --version]])])
AC_MSG_RESULT([[$LLVM_VERSION]])

AC_MSG_NOTICE(initialize build tree)

AC_DEFUN([AC_CP],
  [AS_IF([cp $1 $2],
    [true],
    [AC_MSG_ERROR([[cannot copy file]])])])

AS_IF([test -d stubgen],
  [true],
  [AS_IF([mkdir stubgen],
    [AC_CP([$srcdir/stubgen/dune], [stubgen/])
     AC_CP([$srcdir/stubgen/stubgen.ml], [stubgen/])],
    [AC_MSG_ERROR([[cannot create directory stubgen/]])])])

AS_IF([test -d clangml],
  [true],
  [AS_IF([mkdir clangml],
    [AC_CP([$srcdir/clangml/clang.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang.mli], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast.ml], [clangml/])
     AC_CP([$srcdir/clangml/clang__ast.mli], [clangml/])
     AC_CP([$srcdir/clangml/dune], [clangml/])
     AC_CP([$srcdir/clangml/stubgen.h], [clangml/])
     AC_CP([$srcdir/clangml/libclang_extensions.cpp], [clangml/])
     AC_CP([$srcdir/clangml/libclang_extensions.h], [clangml/])],
    [AC_MSG_ERROR([[cannot create directory clangml/]])])])

cp $srcdir/dune-project .
cp $srcdir/clangml.opam .

AC_CP([$srcdir/bootstrap/$LLVM_VERSION/clang__bindings.ml], [clangml/clang__bindings.ml])

AC_CP([$srcdir/bootstrap/$LLVM_VERSION/clang__bindings.mli], [clangml/clang__bindings.mli])

AC_CP([$srcdir/bootstrap/$LLVM_VERSION/clang_stubs.c], [clangml/clang_stubs.c])

AC_SUBST([LLVM_CFLAGS], [[`$LLVM_CONFIG --cflags `]])

AC_SUBST([LLVM_LDFLAGS], [[`$LLVM_CONFIG --ldflags`]])

AC_SUBST([LLVM_LIBS], [[`$LLVM_CONFIG --libs`]])

AC_SUBST([LLVM_LIBDIR], [[`$LLVM_CONFIG --libdir`]])

AC_SUBST([CFLAGS], [["$LLVM_CFLAGS"]])

AC_SUBST([STRIPPED_LLVM_LDFLAGS], [[`echo $LLVM_LDFLAGS | sed 's/-Wl,-search_paths_first //' | sed 's/-Wl,-headerpad_max_install_names//'`]])

AC_SUBST([LDFLAGS], [["$STRIPPED_LLVM_LDFLAGS -Wl,-rpath,$LLVM_LIBDIR"]])

AC_SUBST([LIBS], [["-lclang -lclangAST -lclangLex -lclangBasic $LLVM_LIBS -lpthread -lcurses -lstdc++"]])

AC_CONFIG_FILES([
  Makefile
  config/cflags.sexp
  config/ldflags.sexp
  config/ldflags_quoted.sexp
  config/libs.sexp
  config/libs_quoted.sexp
])

AC_OUTPUT
