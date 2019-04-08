mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

DUNE_PREFIX = _build/default

CLANGML_CMXA = clangml/clangml.cmxa

LIBCLANGML_STUBS_A = clangml/libclangml_stubs.a

STUBGEN_EXE = stubgen/stubgen.exe

mypkg_SCRIPTS = \
	$(DUNE_PREFIX)/$(CLANGML_CMXA) \
	$(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A)

.PHONY : clangml
clangml : $(DUNE_PREFIX)/$(CLANGML_CMXA) $(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A)

.PHONY : stubgen
stubgen : $(DUNE_PREFIX)/$(STUBGEN_EXE)

.PHONY : tests
tests :
	dune runtest

.PHONY : doc
doc :
	dune build @doc

$(DUNE_PREFIX)/$(CLANGML_CMXA) : \
		clangml/clang.ml clangml/clang.mli \
		clangml/clang__bindings.ml clangml/clang__ast.ml \
		clangml/clang__utils.ml clangml/clang__ast_utils.ml \
	clangml/clang__printer.ml
	$(DUNE) build $(CLANGML_CMXA)

$(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A) : \
		clangml/clang_stubs.c clangml/libclang_extensions.cpp \
		clangml/libclang_extensions.h clangml/stubgen.h
	$(DUNE) build $(LIBCLANGML_STUBS_A)

$(DUNE_PREFIX)/$(STUBGEN_EXE) : \
		$(DUNE_PREFIX)/$(CLANGML_CMXA) stubgen/stubgen.ml
	$(DUNE) build $(STUBGEN_EXE)

current/clang__bindings.ml : $(DUNE_PREFIX)/$(STUBGEN_EXE) \
		clangml/libclang_extensions.h
	$(DUNE_PREFIX)/$(STUBGEN_EXE) --llvm-config $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml

clean-local : clean-local-dune

.PHONY : clean-local-dune
clean-local-dune :
	dune clean

$(DUNE_PREFIX)/clangml.install : \
		$(DUNE_PREFIX)/$(CLANGML_CMXA) \
		$(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A) \
		clangml.opam
	dune build @install

install : $(DUNE_PREFIX)/clangml.install
	- test -z $(INSTALL_NAME_TOOL) || \
		$(INSTALL_NAME_TOOL) -add_rpath $(LLVM_LIBDIR) \
			_build/install/default/lib/stublibs/dllclangml_stubs.so
	dune install

.mli.doc.ml :
	$(OCAMLCODOC) $<
