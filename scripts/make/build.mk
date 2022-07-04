.PHONY: build-all
## build-all: build all haskell packages, or "have i broken anything?"
build-all: build build-tests build-integration-tests build-pro build-pro-tests build-multitenant build-multitenant-integration-tests

.PHONY: build
## build: build non-pro graphql executable 
build:
	cabal build graphql-engine

.PHONY: build-tests 
## build-tests: build non-pro graphql executable tests 
build-tests:
	cabal build graphql-engine-tests

.PHONY: build-integration-tests 
## build-integration-tests: build hspec integration tests 
build-integration-tests:
	cabal build tests-hspec

.PHONY: build-pro 
## build-pro: build pro graphql executable 
build-pro:
	cabal build graphql-engine-pro

.PHONY: build-pro-tests 
## build-pro-tests: build pro graphql executable tests 
build-pro-tests:
	cabal build graphql-engine-pro-test

.PHONY: build-multitenant
## build-multitenant: build multitenant graphql executable 
build-multitenant:
	cabal build graphql-engine-multitenant

.PHONY: build-multitenant-integration-tests
## build-multitenant-integration-tests: build multitenant integration tests 
build-multitenant-integration-tests:
	cabal build multitenant-integration-test
