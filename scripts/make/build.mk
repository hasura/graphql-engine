# Enable secondary expansion.
.SECONDEXPANSION:

PACKAGE_YAML_FILES = $(wildcard server/lib/*/package.yaml)
GENERATED_CABAL_FILES = $(foreach package_file,$(PACKAGE_YAML_FILES),$(wildcard $(dir $(package_file))*.cabal))

.PHONY: build-all
## build-all: build all haskell packages, or "have i broken anything?"
build-all: $(GENERATED_CABAL_FILES)
	cabal build all --enable-tests --enable-benchmarks

.PHONY: build
## build: build non-pro graphql executable
build: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine

.PHONY: build-tests
## build-tests: build non-pro graphql executable tests
build-tests: $(GENERATED_CABAL_FILES)
	cabal build graphql-engine-tests graphql-engine-test-mssql graphql-engine-test-postgres

.PHONY: build-integration-tests
## build-integration-tests: build hspec integration tests
build-integration-tests: $(GENERATED_CABAL_FILES)
	cabal build api-tests

.PHONY: build-tests-dc-api
## build-dc-api-tests: build dc-api agent tests
build-tests-dc-api: $(GENERATED_CABAL_FILES)
	cabal build tests-dc-api

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

.PHONY: build-pro-api-tests
## build-pro-api-tests: build pro api-tests
build-pro-api-tests:
	cabal build api-tests-pro
