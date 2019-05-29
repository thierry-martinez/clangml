mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

DUNE_PREFIX = _build/default
DUNE_INSTALL_PREFIX = _build/install/default

CLANGML_CMXA = clangml/clang.cmxa

LIBCLANGML_STUBS_A = clangml/libclang_stubs.a

STUBGEN_EXE = stubgen/stubgen.exe

TESTS_DIR = tests

LIFT_CMXA = lift/clangml_lift.cmxa

VISITORS_CMXA = visitors/clangml_visitors.cmxa

PPX_CMXA = ppx/clangml_ppx.cmxa

tests ::= $(notdir $(wildcard $(TESTS_DIR)/*))

tests_exe ::= $(foreach test,$(tests),$(TESTS_DIR)/$(test)/$(test).exe)

dune_files ::= \
	$(CLANGML_CMXA) \
	$(LIBCLANGML_STUBS_A) \
	$(STUBGEN_EXE) \
	$(LIFT_CMXA) \
	$(VISITORS_CMXA) \
	$(PPX_CMXA)

mypkg_SCRIPTS ::= $(addprefix $(DUNE_PREFIX)/, $(dune_files))

# Entry targets: clangml, stubgen, lift, visitors, ppx, doc, tests, install
# and clean.

.PHONY : all
all : clangml lift visitors ppx

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
tests : $(foreach test,$(tests),$(TESTS_DIR)/$(test))

define foreach_test
.PHONY : $(TESTS_DIR)/$(test)
$(TESTS_DIR)/$(test) : $(DUNE_PREFIX)/$(TESTS_DIR)/$(test)/$(test).exe
	$(DUNE_PREFIX)/$(TESTS_DIR)/$(test)/$(test).exe
endef
$(foreach test, $(tests), $(eval $(foreach_test)))

.PHONY : install
install :
	dune build @install
# rpath required for dynamic link on Mac OS X
	- test -z $(INSTALL_NAME_TOOL) || \
		$(INSTALL_NAME_TOOL) -add_rpath $(LLVM_LIBDIR) \
			$(DUNE_INSTALL_PREFIX)/lib/stublibs/dllclangml_stubs.so
# obuild does not support line break in 'requires' field
	sed -i '/requires = "[^"]*$$/{:x N;s/\n */ /;/[^"]$$/bx}' \
		$(DUNE_INSTALL_PREFIX)/lib/clangml/META
	dune install
# obuild does not support sub-package directories
	clangml_lib=`ocamlfind query clangml` \
		sh -c 'cp $$clangml_lib/visitors/* \
			$$clangml_lib/ppx/* $$clangml_lib/lift/* $$clangml_lib'


.PHONY : uninstall
uninstall :
	dune uninstall

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
$(foreach dune_file, $(dune_files) $(tests_exe), $(eval $(foreach_dune_file)))

# Targets for bootstrapping bindings

current/clang__bindings.ml : stubgen
	$(DUNE_PREFIX)/$(STUBGEN_EXE) --llvm-config $(LLVM_CONFIG) current/

current/clang__bindings.mli : current/clang__bindings.ml

current/clang_stubs.c : current/clang__bindings.ml
