# the following variables are set up in Docker Compose
# and are also defined in Harness.Constants for use in api tests
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
DC_REFERENCE_PORT = 65005
DC_REFERENCE_AGENT_URL = localhost:$(DC_REFERENCE_PORT)/health
DC_SQLITE_PORT = 65007
DC_SQLITE_AGENT_URL = localhost:$(DC_SQLITE_PORT)/health
COCKROACH_PORT = 65008
COCKROACH_DBNAME = hasura
COCKROACH_DBUSER = root

# Use the Azure SQL Edge image instead of the SQL Server image on arm64.
# The latter doesn't work yet.
ifeq ($(shell uname -m),arm64)
MSSQL_IMAGE=mcr.microsoft.com/azure-sql-edge
else
MSSQL_IMAGE=  # allow the Docker Compose file to set the image
endif

# Run `sqlcmd` in a separate image when waiting for SQL Server to start.
MSSQL_SQLCMD = docker run --rm --platform=linux/amd64 --net=host mcr.microsoft.com/mssql-tools /opt/mssql-tools/bin/sqlcmd

export MSSQL_IMAGE
export MSSQL_SQLCMD

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

.PHONY: start-cockroach
## start-cockroach: start local PostgreSQL DB in Docker and wait for it to be ready
start-cockroach:
	docker compose up -d --wait cockroach

.PHONY: start-postgres
## start-postgres: start local PostgreSQL DB in Docker and wait for it to be ready
start-postgres:
	docker compose up -d --wait postgres

.PHONY: start-citus
## start-citus: start local Citus DB in Docker and wait for it to be ready
start-citus:
	docker compose up -d --wait citus

.PHONY: start-sqlserver
## start-sqlserver: start local MS SQL Server DB in Docker and wait for it to be ready
start-sqlserver:
	docker compose up -d --wait sqlserver
	docker compose run sqlserver-init

.PHONY: start-mysql
## start-mysql: start local MariaDB in Docker and wait for it to be ready
start-mysql:
	docker compose up -d --wait mariadb

.PHONY: start-dc-reference-agent
## start-dc-reference-agent: start the Data Connectors reference agent in Docker and wait for it to be ready
start-dc-reference-agent:
	docker compose up -d --wait --build dc-reference-agent

.PHONY: start-dc-sqlite-agent
## start-dc-sqlite-agent: start the Data Connectors SQLite agent in Docker and wait for it to be ready
start-dc-sqlite-agent:
	docker compose up -d --wait --build dc-sqlite-agent

.PHONY: start-backends
## start-backends: start all known backends in Docker and wait for them to be ready
start-backends: \
	start-postgres start-sqlserver start-mysql start-citus start-dc-reference-agent start-dc-sqlite-agent start-cockroach

.PHONY: stop-everything
## stop-everything: tear down test databases
stop-everything:
	# stop docker
	docker compose down -v

.PHONY: remove-tix-file
remove-tix-file:
	@ find . -name '*.tix' -delete
