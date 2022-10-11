.PHONY: test-bigquery
## test-bigquery: run tests for BigQuery backend
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-bigquery: start-postgres remove-tix-file
	$(call stop_after, \
		cabal run api-tests -- -m 'BigQuery')

.PHONY: test-sqlserver
## test-sqlserver: run tests for SQL Server backend
test-sqlserver: spawn-postgres spawn-sqlserver wait-for-postgres wait-for-sqlserver remove-tix-file
	$(call stop_after, \
		cabal run api-tests -- -m 'SQLServer')

.PHONY: test-mysql
## test-mysql: run tests for MySQL backend
test-mysql: spawn-postgres spawn-mysql wait-for-postgres wait-for-mysql remove-tix-file
	$(call stop_after, \
		cabal run api-tests -- -m 'MySQL')

.PHONY: test-citus
## test-citus: run tests for Citus backend
test-citus: spawn-postgres spawn-citus wait-for-postgres wait-for-citus remove-tix-file
	$(call stop_after, \
		cabal run api-tests -- -m 'Citus')

.PHONY: test-data-connectors
## test-data-connectors: run tests for Data Connectors
test-data-connectors: start-postgres start-dc-reference-agent remove-tix-file
	$(call stop_after, \
		cabal run api-tests -- -m 'DataConnector')

.PHONY: test-cockroach
## test-cockroach: run tests for Cockroach backend
test-cockroach: spawn-postgres spawn-cockroach wait-for-postgres wait-for-cockroach remove-tix-file
	$(call stop_after, \
		cabal run api-tests -- -m 'Cockroach')

.PHONY: test-backends
## test-backends: run tests for all backends
# BigQuery tests will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/lib/api-tests#required-setup-for-bigquery-tests
test-backends: start-backends remove-tix-file
	$(call stop_after, \
		cabal run api-tests)

.PHONY: test-unit
## test-unit: run unit tests from main suite
test-unit: remove-tix-file
	cabal run graphql-engine-tests -- unit
