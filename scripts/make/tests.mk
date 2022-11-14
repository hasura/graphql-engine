.PHONY: test-bigquery
## test-bigquery: run tests for BigQuery backend
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-bigquery: remove-tix-file
	docker compose up -d --wait postgres
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'BigQuery')

.PHONY: test-sqlserver
## test-sqlserver: run tests for SQL Server backend
test-sqlserver: start-sqlserver remove-tix-file
	docker compose up -d --wait postgres
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'SQLServer')

.PHONY: test-mysql
## test-mysql: run tests for MySQL backend
test-mysql: remove-tix-file
	docker compose up -d --wait postgres mariadb
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'MySQL')

.PHONY: test-citus
## test-citus: run tests for Citus backend
test-citus: remove-tix-file
	docker compose -d --wait postgres citus
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'Citus')

.PHONY: test-data-connectors
## test-data-connectors: run tests for Data Connectors
test-data-connectors: remove-tix-file
	docker compose up -d --wait postgres dc-reference-agent dc-sqlite-agent
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'DataConnector')

.PHONY: test-cockroach
## test-cockroach: run tests for Cockroach backend
test-cockroach: remove-tix-file
	docker compose up -d --wait postgres cockroach
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'Cockroach')

.PHONY: test-postgres
## test-postgres: run tests for Postgres backend
test-postgres: start-backends remove-tix-file
	$(call stop_after, \
		cabal run api-tests:exe:api-tests -- -m 'Postgres')

.PHONY: test-backends
## test-backends: run tests for all backends
# BigQuery tests will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-backends: start-backends remove-tix-file
	$(call stop_after, \
		cabal run api-tests:exe:api-tests)

.PHONY: test-unit
## test-unit: run unit tests from main suite
test-unit: remove-tix-file
	cabal run graphql-engine-tests -- unit
