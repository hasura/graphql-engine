.PHONY: test-bigquery
## test-bigquery: run tests for BigQuery backend
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/tests-hspec#required-setup-for-bigquery-tests
test-bigquery: start-postgres remove-tix-file
	$(call stop_after, \
		cabal run tests-hspec -- -m 'BigQuery')

.PHONY: test-sqlserver
## test-sqlserver: run tests for SQL Server backend
test-sqlserver: start-postgres start-sqlserver remove-tix-file
	$(call stop_after, \
		cabal run tests-hspec -- -m 'SQLServer')

.PHONY: test-mysql
## test-mysql: run tests for MySQL backend
test-mysql: start-postgres start-mysql remove-tix-file
	$(call stop_after, \
		cabal run tests-hspec -- -m 'MySQL')

.PHONY: test-backends
## test-backends: run tests for all backends
# BigQuery tests will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/tests-hspec#required-setup-for-bigquery-tests
test-backends: start-backends remove-tix-file
	$(call stop_after, \
		cabal run tests-hspec)
