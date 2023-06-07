GRAPHQL_ENGINE_PATH=$(shell cabal list-bin exe:graphql-engine)
POSTGRES_AGENT_PATH=$(shell cabal list-bin exe:postgres-agent)
GRAPHQL_ENGINE_PRO_PATH=$(shell cabal list-bin exe:graphql-engine-pro)

API_TESTS=api-tests:exe:api-tests
API_TESTS_PRO=api-tests-pro:exe:api-tests-pro

.PHONY: test-bigquery
## test-bigquery: run tests for BigQuery backend
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-bigquery: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait postgres
	HASURA_TEST_BACKEND_TYPE=BigQuery \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-sqlserver
## test-sqlserver: run tests for MS SQL Server backend
test-sqlserver: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait
	HASURA_TEST_BACKEND_TYPE=SQLServer \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-citus
## test-citus: run tests for Citus backend
test-citus: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait
	HASURA_TEST_BACKEND_TYPE=Citus \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-data-connectors
## test-data-connectors: run tests for Data Connectors
test-data-connectors: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait
	HASURA_TEST_BACKEND_TYPE=DataConnector \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-cockroach
## test-cockroach: run tests for Cockroach backend
test-cockroach: build remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait
	HASURA_TEST_BACKEND_TYPE=Cockroach \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-postgres
## test-postgres: run tests for Postgres backend
# we have a few tests labeled with 'Postgres' which test their variants, too,
# so this also starts containers for Postgres variants
test-postgres: build build-postgres-agent remove-tix-file
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait
	HASURA_TEST_BACKEND_TYPE=Postgres \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-no-backends
## test-no-backends
# the leftover tests with no particular backend, like Remote Schemas
test-no-backends: build start-api-tests-backends remove-tix-file
	HASURA_TEST_BACKEND_TYPE=None \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-backends
## test-backends: run tests for all backends
# BigQuery tests will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-backends: build build-postgres-agent start-api-tests-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS)

.PHONY: test-matrix
## test-matrix: postgres test matrix generator
test-matrix: build start-api-tests-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run api-tests:exe:produce-feature-matrix +RTS -N4 -RTS

.PHONY: test-data-connectors-pro
## test-data-connectors-pro: run tests for HGE pro for all backends
test-data-connectors-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-data-connectors-snowflake-pro
## test-data-connectors-snowflake-pro: run tests for HGE pro for all backends
test-data-connectors-snowflake-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO) -- --match "DataConnector \"snowflake\""

.PHONY: test-data-connectors-athena-pro
## test-data-connectors-athena-pro: run tests for HGE pro for all backends
test-data-connectors-athena-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	$(call stop_after, \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO) -- --match "DataConnector \"athena\"")

.PHONY: test-data-connectors-mysql-pro
## test-data-connectors-mysql-pro: run tests for HGE pro for all backends
test-data-connectors-mysql-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	$(call stop_after, \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO) -- --match "DataConnector \"mysql2\"")

.PHONY: test-backends-pro
## test-backends-pro: run tests for HGE pro for all backends
test-backends-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-postgres-pro
## test-postgres-pro: run tests for HGE pro for postgres
test-postgres-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		HASURA_TEST_BACKEND_TYPE=Postgres \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-citus-pro
## test-citus-pro: run tests for HGE pro for citus
test-citus-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		HASURA_TEST_BACKEND_TYPE=Citus \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-cockroach-pro
## test-cockroach-pro: run tests for HGE pro for Cockroach
test-cockroach-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		HASURA_TEST_BACKEND_TYPE=Cockroach \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-sqlserver-pro
## test-sqlserver-pro: run tests for HGE pro for SQLServer
test-sqlserver-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		HASURA_TEST_BACKEND_TYPE=SQLServer \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-bigquery-pro
## test-bigquery-pro: run tests for HGE pro for BigQuery
test-bigquery-pro: build-pro build-postgres-agent start-api-tests-pro-backends remove-tix-file
	GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		HASURA_TEST_BACKEND_TYPE=BigQuery \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-unit
## test-unit: run unit tests from main suite
test-unit: remove-tix-file
	cabal run graphql-engine:test:graphql-engine-tests

.PHONY: test-integration-mssql
## test-integration-mssql: run MS SQL Server integration tests
test-integration-mssql: remove-tix-file
	docker compose up --build --detach --wait sqlserver-healthcheck
	HASURA_MSSQL_CONN_STR='$(TEST_MSSQL_CONNECTION_STRING)' \
			cabal run graphql-engine:test:graphql-engine-test-mssql

.PHONY: test-integration-postgres
## test-integration-postgres: run PostgreSQL integration tests
test-integration-postgres: remove-tix-file
	docker compose up --build --detach --wait postgres
	HASURA_GRAPHQL_DATABASE_URL='$(TEST_POSTGRES_URL)' \
			cabal run graphql-engine:test:graphql-engine-test-postgres

.PHONY: test-native-queries
## test-native-queries: run all tests for the Native Query feature
test-native-queries: build-postgres-agent
	cabal build exe:graphql-engine-pro
	docker compose up --build --detach --wait postgres citus cockroach sqlserver-healthcheck
	HSPEC_MATCH=NativeQueries make test-unit
	HSPEC_MATCH=NativeQueries \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-native-queries-postgres
## test-native-queries-postgres: run all postgres tests for the Native Query feature
test-native-queries-postgres: build-postgres-agent
	cabal build exe:graphql-engine-pro
	docker compose up --build --detach --wait postgres
	HSPEC_MATCH=${HSPEC_MATCH:-"NativeQueries"}
	make test-unit
	HASURA_TEST_BACKEND_TYPE=Postgres \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-native-queries-sqlserver
## test-native-queries-sqlserver: run all sqlserver tests for the Native Query feature
test-native-queries-sqlserver: remove-tix-file build-postgres-agent
	cabal build exe:graphql-engine-pro
	docker compose up --build --detach --wait postgres sqlserver-healthcheck
	HSPEC_MATCH=${HSPEC_MATCH:-"NativeQueries"}
	HASURA_TEST_BACKEND_TYPE=SQLServer \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-native-queries-bigquery
## test-native-queries-bigquery: run all bigquery tests for the Native Query feature
test-native-queries-bigquery: remove-tix-file build-postgres-agent
	cabal build exe:graphql-engine-pro
	docker compose up --build --detach --wait postgres
	HSPEC_MATCH=${HSPEC_MATCH:-"NativeQueries"}
	HASURA_TEST_BACKEND_TYPE=BigQuery \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: test-stored-procedures-sqlserver
## test-stored-procedures-sqlserver: run all sqlserver tests for the Stored Procedure feature
test-stored-procedures-sqlserver: remove-tix-file build-postgres-agent
	cabal build exe:graphql-engine-pro
	docker compose up --build --detach --wait postgres sqlserver-healthcheck
	HASURA_TEST_BACKEND_TYPE=SQLServer \
		HSPEC_MATCH=StoredProcedures \
		GRAPHQL_ENGINE=$(GRAPHQL_ENGINE_PRO_PATH) \
		POSTGRES_AGENT=$(POSTGRES_AGENT_PATH) \
		cabal run $(API_TESTS_PRO)

.PHONY: py-tests
## py-tests: run the python-based test suite
py-tests:
	./server/tests-py/run.sh

.PHONY: upgrade-tests
## upgrade-tests: run the server upgrade tests
upgrade-tests:
	cabal run upgrade-tests:test:upgrade-tests

.PHONY: test-dc-postgres-agent
## test-dc-postgres-agent: run the dc-api test suite against the postgres agent
# The commands below are obtuse, so here they are, explained. (We can't comment inline.)
#   1. Build the agent.
#   2. Bring up the PostgreSQL container.
#   3. Run the agent on port 8889, and wait a second for it to start.
#   4. Ensure we clean up after ourselves.
#   5. Get the port of the PostgreSQL container.
#   6. Run the tests, pointing to the correct port for the DB.
test-dc-postgres-agent: build-postgres-agent
	$(DC_POSTGRES_DOCKER_COMPOSE) up --wait
	@ echo 'cabal run postgres-agent:exe:postgres-agent'; \
		cabal run postgres-agent:exe:postgres-agent -- --port 8889 & trap "kill $$!" EXIT; \
		sleep 1; \
		PG_PORT="$$($(DC_POSTGRES_DOCKER_COMPOSE) port postgres 5432 | sed 's/.*://')"; \
		echo 'cabal run test:tests-dc-api -- test'; \
		cabal run test:tests-dc-api -- \
			test \
			--agent-base-url 'http://localhost:8889' \
			--agent-config "{\"connection\": \"postgresql://postgres:password@localhost:$${PG_PORT}\"}" \
			sandwich

.PHONY: test-dc-postgres-agent-only
## test-dc-postgres-agent-only: run the dc-api test suite against an already-running postgres agent on port 8888
test-dc-postgres-agent-only:
	# See above for an explanation.
	$(DC_POSTGRES_DOCKER_COMPOSE) up --wait
	@ echo 'cabal run test:tests-dc-api -- test'
	@ PG_PORT="$$($(DC_POSTGRES_DOCKER_COMPOSE) port postgres 5432 | sed 's/.*://')"; \
		cabal run test:tests-dc-api -- \
			test \
			--agent-base-url "http://localhost:8888" \
			--agent-config "{\"connection\": \"postgresql://postgres:password@localhost:$${PG_PORT}\"}" \
			sandwich --tui
