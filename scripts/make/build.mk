# Enable secondary expansion.
.SECONDEXPANSION:

PACKAGE_YAML_FILES = $(wildcard server/lib/*/package.yaml)
GENERATED_CABAL_FILES = $(foreach package_file,$(PACKAGE_YAML_FILES),$(wildcard $(dir $(package_file))*.cabal))

.PHONY: build-all
## build-all: build all haskell packages, or "have i broken anything?"
build-all: build build-tests build-integration-tests build-pro build-pro-tests build-multitenant build-multitenant-integration-tests

.PHONY: build
## build: build non-pro graphql executable
build: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine

.PHONY: build-tests
## build-tests: build non-pro graphql executable tests
build-tests: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine-tests

.PHONY: build-integration-tests
## build-integration-tests: build hspec integration tests
build-integration-tests: $(GENERATED_CABAL_FILES)
	cabal build tests-hspec

.PHONY: build-pro
## build-pro: build pro graphql executable
build-pro: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine-pro

.PHONY: build-pro-tests
## build-pro-tests: build pro graphql executable tests
build-pro-tests: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine-pro-test

.PHONY: build-multitenant
## build-multitenant: build multitenant graphql executable
build-multitenant: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine-multitenant

.PHONY: build-multitenant-integration-tests
## build-multitenant-integration-tests: build multitenant integration tests
build-multitenant-integration-tests: $(GENERATED_CABAL_FILES)
	cabal build multitenant-integration-test

# This makes use of Make's static pattern rules. Effectively, it is generating
# multiple rules, of the form:
#
#     path/to/foo/foo.cabal: path/to/foo/package.yaml
#         hpack ...
#     path/to/bar/bar.cabal: path/to/bar/package.yaml
#         hpack ...
#
# In order to call `dir`, it uses secondary expansion.
#
# See the documentation for more information:
# https://www.gnu.org/software/make/manual/html_node/Static-Pattern.html
# https://www.gnu.org/software/make/manual/html_node/Secondary-Expansion.html
$(GENERATED_CABAL_FILES): %.cabal: $$(dir %)/package.yaml server/lib/common.yaml $$(shell find $$(dir %) -name '*.hs')
	./scripts/hpack.sh $@
	@ touch $@  # Required because `hpack` will not change the modified timestamp if the file is up-to-date.
