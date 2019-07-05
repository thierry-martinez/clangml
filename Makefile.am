mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

DUNE_PREFIX = _build/default
DUNE_INSTALL_PREFIX = _build/install/default

CLANGML_CMXA = clangml/clang.cmxa

LIBCLANGML_STUBS_A = clangml/libclang_stubs.a

STUBGEN_EXE = stubgen/stubgen.exe

TESTS_DIR = tests

libraries ::= lift ppx show printer compare

tests ::= $(notdir $(wildcard $(TESTS_DIR)/*))

tests_exe ::= $(foreach test,$(tests),$(TESTS_DIR)/$(test)/$(test).exe)

# This variable is expanded for each $(library)
library_cmxa = $(library)/clangml_$(library).cmxa

dune_files ::= \
	$(CLANGML_CMXA) \
	$(LIBCLANGML_STUBS_A) \
	$(STUBGEN_EXE) \
	$(foreach library,$(libraries),$(library_cmxa))

mypkg_SCRIPTS ::= $(addprefix $(DUNE_PREFIX)/, $(dune_files))

# Entry targets: all, clangml, stubgen, doc, tests, install, uninstall, clean,
# and one by library and one by test.

.PHONY : all
all : clangml clangml.opam $(libraries)

.PHONY : clangml
clangml : $(DUNE_PREFIX)/$(CLANGML_CMXA) $(DUNE_PREFIX)/$(LIBCLANGML_STUBS_A)

clangml.opam : dune-project
	dune build clangml.opam

.PHONY : stubgen
stubgen : $(DUNE_PREFIX)/$(STUBGEN_EXE)

.PHONY : doc
doc :
	dune build @doc

.PHONY : tests
tests : $(foreach test,$(tests),$(TESTS_DIR)/$(test))

.PHONY : install
install :
	dune build @install
# rpath required for dynamic link on Mac OS X
	- test -z $(INSTALL_NAME_TOOL) || \
		$(INSTALL_NAME_TOOL) -add_rpath $(LLVM_LIBDIR) \
			$(DUNE_INSTALL_PREFIX)/lib/stublibs/dllclangml_stubs.so
	dune install


.PHONY : uninstall
uninstall :
	dune uninstall

clean-local : clean-local-dune

.PHONY : clean-local-dune
clean-local-dune :
	dune clean

define foreach_library
.PHONY : ($library)
$(library) : $(DUNE_PREFIX)/$(library_cmxa)
endef
$(foreach library, $(libraries), $(eval $(foreach_library)))

define foreach_test
.PHONY : $(TESTS_DIR)/$(test)
$(TESTS_DIR)/$(test) : $(DUNE_PREFIX)/$(TESTS_DIR)/$(test)/$(test).exe
	$(DUNE_PREFIX)/$(TESTS_DIR)/$(test)/$(test).exe
endef
$(foreach test, $(tests), $(eval $(foreach_test)))

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
