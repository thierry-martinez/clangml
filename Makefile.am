mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

DUNE_PREFIX = _build/default

CLANGML_CMXA = clangml/clangml.cmxa

LIBCLANGML_STUBS_A = clangml/libclangml_stubs.a

STUBGEN_EXE = stubgen/stubgen.exe

TESTS_EXE = tests/clangml__ast/tests.exe

LIFT_CMXA = lift/clangml_lift.cmxa

VISITORS_CMXA = visitors/clangml_visitors.cmxa

PPX_CMXA = ppx/clangml_ppx.cmxa

dune_files = \
	$(CLANGML_CMXA) \
	$(LIBCLANGML_STUBS_A) \
	$(STUBGEN_EXE) \
	$(TESTS_EXE) \
	$(LIFT_CMXA) \
	$(VISITORS_CMXA) \
	$(PPX_CMXA)

mypkg_SCRIPTS = \
	$(addprefix $(DUNE_PREFIX)/, $(filter-out $(TESTS_EXE), $(dune_files)))


# Entry targets: clangml, stubgen, lift, visitors, doc, tests, install
# and clean.

.PHONY : clangml
clangml : $(DUNE_PREFIX)/$(CLANGML_CMXA) $(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A)

.PHONY : stubgen
stubgen : $(DUNE_PREFIX)/$(STUBGEN_EXE)

.PHONY : lift
lift : $(DUNE_PREFIX)/$(LIFT_CMXA)

.PHONY : visitors
visitors : $(DUNE_PREFIX)/$(VISITORS_CMXA)

.PHONY : ppx
ppx : $(DUNE_PREFIX)/$(PPX_CMXA)

.PHONY : doc
doc :
	dune build @doc

.PHONY : tests
tests : $(DUNE_PREFIX)/$(TESTS_EXE)
	$(DUNE_PREFIX)/$(TESTS_EXE)
#	dune runtest

.PHONY : install
install :
	dune build @install
	- test -z $(INSTALL_NAME_TOOL) || \
		$(INSTALL_NAME_TOOL) -add_rpath $(LLVM_LIBDIR) \
			_build/install/default/lib/stublibs/dllclangml_stubs.so
	dune install

clean-local : clean-local-dune

.PHONY : clean-local-dune
clean-local-dune :
	dune clean

# Delegate to dune the building of $(DUNE_PREFIX)/* files
# They are all phony targets since we want to rely on dune for dependency
# management.

define foreach_dune_file
.PHONY : $(DUNE_PREFIX)/$(dune_file)
$(DUNE_PREFIX)/$(dune_file) :
	$(DUNE) build $(dune_file)
endef
$(foreach dune_file,$(dune_files),$(eval $(foreach_dune_file)))

# Targets for bootstrapping bindings

current/clang__bindings.ml : stubgen
	$(DUNE_PREFIX)/$(STUBGEN_EXE) --llvm-config $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml
