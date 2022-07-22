# the following variables are set up in Docker Compose
# and are also defined in Harness.Constants for use in hspec tests
PG_PORT = 65002
PG_DBNAME = hasura
PG_DBUSER = hasura
PG_DBPASSWORD = hasura
MYSQL_PORT = 65001
MYSQL_DBNAME = hasura
MYSQL_DBUSER = hasura
MYSQL_DBPASSWORD = hasura
MSSQL_PORT = 65003
MSSQL_DBNAME = hasura
MSSQL_DBUSER = hasura
MSSQL_DBPASSWORD = Hasura1!
CITUS_PORT = 65004

# utils.sh contains functions used in CI to wait for DBs to be ready.
# this can be (ab)used like a script; e.g. `$(DB_UTILS) foo` expands to
# `source ./.buildkite/scripts/util/util.sh; foo`, which will run the `foo`
# function from util.sh (or anywhere else).
DB_UTILS = source ./.buildkite/scripts/util/util.sh;

ifneq ($(shell command -v docker-compose),)
DOCKER_COMPOSE = docker-compose
else
ifneq ($(shell command -v nix),)
DOCKER_COMPOSE = nix run nixpkgs\#docker-compose --
else
DOCKER_COMPOSE = $(error "Could not find docker-compose.")
endif
endif

define stop_after
@ echo $1 >&2
@ $1 || EXIT_STATUS=$$?; \
if [[ -z "$${EXIT_STATUS:-}" ]]; then \
	$(MAKE) stop-everything; \
else \
	$(MAKE) stop-everything; \
	exit $$EXIT_STATUS; \
fi
endef

.PHONY: start-postgres
## start-postgres: start local PostgreSQL DB in Docker and wait for it to be ready
start-postgres:
	$(DOCKER_COMPOSE) up -d postgres
	$(DB_UTILS) wait_for_postgres $(PG_PORT)
	$(DB_UTILS) wait_for_postgres_db $(PG_PORT) "$(PG_DBNAME)" "$(PG_DBUSER)" "$(PG_DBPASSWORD)"

.PHONY: start-citus
## start-citus: start local Citus DB in Docker and wait for it to be ready
start-citus:
	$(DOCKER_COMPOSE) up -d citus
	$(DB_UTILS) wait_for_postgres $(CITUS_PORT)
	$(DB_UTILS) wait_for_postgres_db $(CITUS_PORT) "$(PG_DBNAME)" "$(PG_DBUSER)" "$(PG_DBPASSWORD)"

.PHONY: start-sqlserver
## start-sqlserver: start local MS SQL Server DB in Docker and wait for it to be ready
start-sqlserver:
	$(DOCKER_COMPOSE) up -d sqlserver
	$(DB_UTILS) wait_for_mssql $(MSSQL_PORT)
	$(DB_UTILS) wait_for_mssql_db $(MSSQL_PORT) "$(MSSQL_DBNAME)" "$(MSSQL_DBUSER)" "$(MSSQL_DBPASSWORD)"

.PHONY: start-mysql
## start-mysql: start local MariaDB in Docker and wait for it to be ready
start-mysql:
	$(DOCKER_COMPOSE) up -d mariadb
	$(DB_UTILS) wait_for_mysql $(MYSQL_PORT) "$(MYSQL_DBNAME)" "$(MYSQL_DBUSER)" "$(MYSQL_DBPASSWORD)"

.PHONY: start-backends
## start-backends: start local PostgreSQL, MariaDB, and MS SQL Server in Docker and wait for them to be ready
start-backends: start-postgres start-sqlserver start-mysql start-citus

.PHONY: stop-everything
## stop-everything: tear down test databases
stop-everything:
	# stop docker
	$(DOCKER_COMPOSE) down -v

.PHONY: remove-tix-file
remove-tix-file:
	@ rm -f tests-hspec.tix
