GRAPHQL_ENGINE_PATH=$(shell cabal list-bin exe:graphql-engine)
GRAPHQL_ENGINE_PRO_PATH=$(shell cabal list-bin exe:graphql-engine-pro)

.PHONY: test-bigquery
## test-bigquery: run tests for BigQuery backend
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-bigquery: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=BigQuery \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-sqlserver
## test-sqlserver: run tests for MS SQL Server backend
test-sqlserver: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres sqlserver-healthcheck
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=SQLServer \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-citus
## test-citus: run tests for Citus backend
test-citus: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres citus
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=Citus \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-data-connectors
## test-data-connectors: run tests for Data Connectors
test-data-connectors: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) build
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres dc-reference-agent dc-sqlite-agent
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=DataConnector \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-cockroach
## test-cockroach: run tests for Cockroach backend
test-cockroach: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres cockroach
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=Cockroach \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-postgres
## test-postgres: run tests for Postgres backend
# we have a few tests labeled with 'Postgres' which test their variants, too,
# so this also starts containers for Postgres variants
test-postgres: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres cockroach citus dc-sqlite-agent
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=Postgres \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-no-backends
## test-no-backends
# the leftover tests with no particular backend, like Remote Schemas
test-no-backends: build start-backends remove-tix-file
	$(call stop_after, \
		HASURA_TEST_BACKEND_TYPE=None \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-backends
## test-backends: run tests for all backends
# BigQuery tests will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-backends: build start-backends remove-tix-file
	$(call stop_after, \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:api-tests)

.PHONY: test-matrix
## test-matrix: postgres test matrix generator
test-matrix: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up -d --wait postgres cockroach citus dc-sqlite-agent
	$(call stop_after, \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		cabal run api-tests:exe:produce-feature-matrix +RTS -N4 -RTS)

.PHONY: test-backends-pro
## test-backends-pro: run tests for HGE pro for all backends
test-backends-pro: build-pro start-backends remove-tix-file
	$(call stop_after, \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		cabal run api-tests-pro:exe:api-tests-pro)

.PHONY: test-unit
## test-unit: run unit tests from main suite
test-unit: remove-tix-file
	cabal run graphql-engine:test:graphql-engine-tests

.PHONY: test-integration-mssql
## test-integration-mssql: run MS SQL Server integration tests
test-integration-mssql: remove-tix-file
	docker compose up -d --wait sqlserver{,-healthcheck,-init}
	$(call stop_after, \
		HASURA_MSSQL_CONN_STR='$(TEST_MSSQL_CONNECTION_STRING)' \
			cabal run graphql-engine:test:graphql-engine-test-mssql)

.PHONY: test-integration-postgres
## test-integration-postgres: run PostgreSQL integration tests
test-integration-postgres: remove-tix-file
	docker compose up -d --wait postgres
	$(call stop_after, \
		HASURA_GRAPHQL_DATABASE_URL='$(TEST_POSTGRES_URL)' \
			cabal run graphql-engine:test:graphql-engine-test-postgres)

.PHONY: py-tests
## py-tests: run the python-based test suite
py-tests:
	$(call stop_after, \
		./server/tests-py/run-new.sh)
