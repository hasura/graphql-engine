# scripts used in CI to wait for DBs to be ready
DB_UTILS = ./.buildkite/scripts/util/util.sh

# the following variables are set up in Docker Compose
# and are also defined in Harness.Constants for use in hspec tests
PG_PORT = 65002
PG_DBNAME = "hasura"
PG_DBUSER = "hasura"
PG_DBPASSWORD = "hasura"
MYSQL_PORT = 65001
MYSQL_DBNAME = "hasura"
MYSQL_DBUSER = "hasura"
MYSQL_DBPASSWORD = "hasura"
MSSQL_PORT = 65003
MSSQL_DBNAME = "hasura"
MSSQL_DBUSER = "hasura"
MSSQL_DBPASSWORD = "Hasura1!"
CITUS_PORT = 65004

.PHONY: start-postgres
## start-postgres: start local postgres DB in Docker and wait for it to be ready
start-postgres:
	docker-compose up -d postgres
	$(DB_UTILS) wait_for_postgres $(PG_PORT)
	$(DB_UTILS) wait_for_postgres_db $(PG_PORT) "$(PG_DBNAME)" "$(PG_DBUSER)" "$(PG_DBPASSWORD)"

.PHONY: start-citus
## start-citus: start local citus DB in Docker and wait for it to be ready
start-citus:
	docker-compose up -d citus
	$(DB_UTILS) wait_for_postgres $(CITUS_PORT)
	$(DB_UTILS) wait_for_postgres_db $(CITUS_PORT) "$(PG_DBNAME)" "$(PG_DBUSER)" "$(PG_DBPASSWORD)"

.PHONY: start-sqlserver
## start-sqlserver: start local sqlserver DB in Docker and wait for it to be ready
start-sqlserver:
	docker-compose up -d sqlserver
	$(DB_UTILS) wait_for_mssql $(MSSQL_PORT)
	$(DB_UTILS) wait_for_mssql_db $(MSSQL_PORT) "$(MSSQL_DBNAME)" "$(MSSQL_DBUSER)" "$(MSSQL_DBPASSWORD)"

.PHONY: start-mysql
## start-mysql: start local mariaDB in Docker and wait for it to be ready
start-mysql:
	docker-compose up -d mariadb
	$(DB_UTILS) wait_for_mysql $(MYSQL_PORT) "$(MYSQL_DBNAME)" "$(MYSQL_DBUSER)" "$(MYSQL_DBPASSWORD)"
	# there isn't a wait_for_mysql_db that does an actual query yet, so just give
	# it a bit of time to wake up
	sleep 10

.PHONY: start-backends
## start-backends: start postgres/mysql/mssql and wait for them to be ready 
start-backends: start-postgres start-sqlserver start-mysql start-citus

.PHONY: test-bigquery
## test-bigquery: run tests for BigQuery backend
test-bigquery: start-postgres remove-tix-file
	# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/tests-hspec#required-setup-for-bigquery-tests
	@cabal run tests-hspec -- -m "BigQuery" || EXIT_STATUS=$$?; \
	if [ -z $$EXIT_STATUS ]; then \
		make test-cleanup; \
	else \
		make test-cleanup; \
		exit $$EXIT_STATUS; \
	fi

.PHONY: test-sqlserver
## test-sqlserver: run tests for SQL Server backend
test-sqlserver: start-postgres start-sqlserver remove-tix-file
	@cabal run tests-hspec -- -m "SQLServer" || EXIT_STATUS=$$?; \
	if [ -z $$EXIT_STATUS ]; then \
		make test-cleanup; \
	else \
		make test-cleanup; \
		exit $$EXIT_STATUS; \
	fi

.PHONY: test-mysql
## test-mysql: run tests for MySQL backend
test-mysql: start-postgres start-mysql remove-tix-file
	@cabal run tests-hspec -- -m "MySQL" || EXIT_STATUS=$$?; \
	if [ -z $$EXIT_STATUS ]; then \
		make test-cleanup; \
	else \
		make test-cleanup; \
		exit $$EXIT_STATUS; \
	fi

.PHONY: test-backends
## test-backends: run tests for all backends
test-backends: start-backends remove-tix-file
	# big query tests will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/tests-hspec#required-setup-for-bigquery-tests
	# run tests
	@cabal run tests-hspec || EXIT_STATUS=$$?; \
	if [ -z $$EXIT_STATUS ]; then \
		make test-cleanup; \
	else \
		make test-cleanup; \
		exit $$EXIT_STATUS; \
	fi

.PHONY: test-cleanup
## test-cleanup: teardown for test DBs
test-cleanup:
	# stop docker
	docker-compose down -v

.PHONY: remove-tix-file
remove-tix-file:
	@rm tests-hspec.tix || true
