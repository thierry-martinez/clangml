mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

STUBGEN_EXE = _build/default/stubgen/stubgen.exe

mypkg_SCRIPTS = $(STUBGEN_EXE)

$(STUBGEN_EXE) : _build/default/clangml/clangml.cmxa stubgen/stubgen.ml
	$(DUNE) build stubgen/stubgen.exe

_build/default/clangml/clangml.cmxa : \
		clangml/clang.ml clangml/clang__ast.mli clangml/clang__bindings.ml
	$(DUNE) build clangml/clangml.cmxa

current/clang__bindings.ml : $(STUBGEN_EXE)
	$(STUBGEN_EXE) $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml
