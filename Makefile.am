mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

CLANGML_CMXA = _build/default/clangml/clangml.cmxa

LIBCLANGML_STUBS_A = _build/default/clangml/libclangml_stubs.a

STUBGEN_EXE = _build/default/stubgen/stubgen.exe

mypkg_SCRIPTS = $(CLANGML_CMXA) $(LIBCLANGML_STUBS_A)

.PHONY : clangml
clangml : $(CLANGML_CMXA) $(LIBCLANGML_STUBS_A)

.PHONY : stubgen
stubgen : $(STUBGEN_EXE)

$(CLANGML_CMXA) : clangml/clang.ml clangml/clang__ast.mli clangml/clang__bindings.ml
	$(DUNE) build clangml/clangml.cmxa

$(LIBCLANGML_STUBS_A) : clangml/clang_stubs.c clangml/libclang_extensions.cpp clangml/libclang_extensions.h
	$(DUNE) build clangml/libclangml_stubs.a

$(STUBGEN_EXE) : $(CLANGML_CMXA) stubgen/stubgen.ml
	$(DUNE) build stubgen/stubgen.exe

current/clang__bindings.ml : $(STUBGEN_EXE)
	$(STUBGEN_EXE) $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml
