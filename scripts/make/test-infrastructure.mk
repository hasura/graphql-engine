# the following variables are set up in Docker Compose
# and are also defined in Harness.Constants for use in api tests
MSSQL_PORT = 65003
MSSQL_DBNAME = hasura
MSSQL_DBUSER = hasura
MSSQL_DBPASSWORD = Hasura1!

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

.PHONY: start-sqlserver
## start-sqlserver: start local MS SQL Server DB in Docker and wait for it to be ready
start-sqlserver:
	docker compose up -d --wait sqlserver
	docker compose run sqlserver-init

.PHONY: start-backends
## start-backends: start all known backends in Docker and wait for them to be ready
start-backends: start-sqlserver
	docker compose up -d --wait postgres mariadb citus dc-reference-agent dc-sqlite-agent cockroach

.PHONY: stop-everything
## stop-everything: tear down test databases
stop-everything:
	# stop docker
	docker compose down -v

.PHONY: remove-tix-file
remove-tix-file:
	@ find . -name '*.tix' -delete
