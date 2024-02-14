# ghcid gets its own cache
GRAPHQL_ENGINE_PATH=$(shell cabal list-bin exe:graphql-engine)

GHC_OPTIONS=-Wno-prepositive-qualified-module -Wno-missing-export-lists -O0
CABAL_REPL_FLAGS = --builddir ./dist-newstyle/repl --ghc-options=\"$(GHC_OPTIONS)\"
GHCID_FLAGS?=--no-height-limit

define run_ghcid_api_tests
	@if [[ $$(uname -p) == 'arm' ]]; then \
		HASURA_TEST_BACKEND_TYPE="$(2)" GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) ghcid -c "DYLD_LIBRARY_PATH=$${DYLD_LIBRARY_PATH:-} cabal repl $(1) $(CABAL_REPL_TESTS_FLAGS)" \
			--test "main" $(GHCID_FLAGS); \
	else \
  	HASURA_TEST_BACKEND_TYPE="$(2)" GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) ghcid -c "cabal repl $(1) $(CABAL_REPL_FLAGS)" \ 
  		--test "main" $(GHCID_FLAGS); \
	fi
endef

define run_ghcid_main_tests
	@if [[ $$(uname -p) == 'arm' ]]; then \
		ghcid -c "DYLD_LIBRARY_PATH=$${DYLD_LIBRARY_PATH:-} cabal repl $(1) $(CABAL_REPL_FLAGS)" \
			--test "main" $(GHCID_FLAGS); \
	else \
		ghcid -c "cabal repl $(1) $(CABAL_REPL_FLAGS)" \
			--test "main" $(GHCID_FLAGS); \
	fi
endef


define run_ghcid
	ghcid -c "cabal repl $(1) $(CABAL_REPL_FLAGS)" $(GHCID_FLAGS);
endef

define run_ghcid_run_main
	ghcid -c "cabal repl $(1) $(CABAL_REPL_FLAGS)" $(GHCID_FLAGS) -r;
endef

.PHONY: ghcid-library
## ghcid-library: build and watch library in ghcid
ghcid-library:
	$(call run_ghcid,graphql-engine:lib:graphql-engine)

.PHONY: ghcid-tests
## ghcid-tests: build and watch main tests in ghcid
ghcid-tests:
	$(call run_ghcid,graphql-engine:test:graphql-engine-tests)

.PHONY: ghcid-api-tests
## ghcid-api-tests: build and watch api-tests in ghcid
ghcid-api-tests:
	$(call run_ghcid,api-tests:lib:api-tests)

.PHONY: ghcid-api-tests-run
## ghcid-api-tests-run: build and watch api-tests in ghcid, and run them
ghcid-api-tests-run: start-api-tests-backends
	HASURA_TEST_LOGTYPE=STDOUT \
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
  	$(call run_ghcid_run_main,api-tests:lib:api-tests)

.PHONY: ghcid-test-harness
## ghcid-test-harness: build and watch test-harness in ghcid
ghcid-test-harness:
	$(call run_ghcid,test-harness)

.PHONY: ghcid-pg-client
## ghcid-pg-client: build and watch pg-client in ghcid
ghcid-pg-client:
	$(call run_ghcid,pg-client)

.PHONY: ghcid-api-tests-pro
## ghcid-api-tests-pro: build and watch api-tests in pro
ghcid-api-tests-pro:
	$(call run_ghcid,api-tests-pro:exe:api-tests-pro)

.PHONY: ghcid-test-backends
## ghcid-test-backends: run all api tests in ghcid
ghcid-test-backends: start-api-tests-backends remove-tix-file
	$(call run_ghcid_api_tests,api-tests:exe:api-tests)

.PHONY: ghcid-test-postgres
## ghcid-test-backends: run tests for Postgres backend in ghcid
ghcid-test-postgres: remove-tix-file
	docker compose up postgres -d --wait postgres
	$(call run_ghcid_api_tests,api-tests:exe:api-tests,Postgres)

.PHONY: ghcid-test-bigquery
## ghcid-test-bigquery: run tests for BigQuery backend in ghcid
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
ghcid-test-bigquery: remove-tix-file
	docker compose up -d --wait postgres
	$(call run_ghcid_api_tests,api-tests:exe:api-tests,BigQuery)

.PHONY: ghcid-test-sqlserver
## ghcid-test-sqlserver: run tests for SQL Server backend in ghcid
ghcid-test-sqlserver: remove-tix-file
	docker compose up -d --wait postgres sqlserver{,-healthcheck,-init}
	$(call run_ghcid_api_tests,api-tests:exe:api-tests,SQLServer)

.PHONY: ghcid-test-citus
## ghcid-test-citus: run tests for Citus backend in ghcid
ghcid-test-citus: remove-tix-file
	docker compose -d --wait postgres citus
	$(call run_ghcid_api_tests,api-tests:exe:api-tests,Citus)

.PHONY: ghcid-test-cockroach
## ghcid-test-cockroach: run tests for Cockroach backend in ghcid
ghcid-test-cockroach: remove-tix-file
	docker compose up -d --wait postgres cockroach
	$(call run_ghcid_api_tests,api-tests:exe:api-tests,Cockroach)

.PHONY: ghcid-test-data-connectors
## ghcid-test-data-connectors: run tests for DataConnectors in ghcid
ghcid-test-data-connectors: remove-tix-file
	docker compose build
	docker compose up -d --wait postgres dc-reference-agent dc-sqlite-agent
	$(call run_ghcid_api_tests,api-tests:exe:api-tests,DataConnector)

.PHONY: ghcid-library-pro
## ghcid-library-pro: build and watch pro library in ghcid
ghcid-library-pro:
	$(call run_ghcid,graphql-engine-pro:lib:graphql-engine-pro)

.PHONY: ghcid-test-unit
## ghcid-test-unit: build and run unit tests in ghcid
ghcid-test-unit: remove-tix-file
	$(call run_ghcid_main_tests,graphql-engine:graphql-engine-tests)
