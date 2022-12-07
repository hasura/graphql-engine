# Use the Azure SQL Edge image instead of the SQL Server image on arm64.
# The latter doesn't work yet.
ifeq ($(shell uname -m),arm64)
MSSQL_IMAGE=mcr.microsoft.com/azure-sql-edge
else
MSSQL_IMAGE=  # allow the Docker Compose file to set the image
endif

export MSSQL_IMAGE

TEST_MSSQL_CONNECTION_STRING = Driver={ODBC Driver 18 for SQL Server};Server=localhost,65003;Uid=sa;Pwd=Password!;Encrypt=optional
TEST_POSTGRES_URL = postgres://hasura:hasura@localhost:65002/hasura

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

.PHONY: build-backends
## build-backends: build Docker images for any backends that need them
build-backends:
	docker compose build

.PHONY: start-backends
## start-backends: start all known backends in Docker and wait for them to be ready
start-backends: build-backends
	docker compose up --detach --wait

.PHONY: stop-everything
## stop-everything: tear down test databases
stop-everything:
	docker compose down --volumes
	cd server/tests-py && docker compose down --volumes

.PHONY: remove-tix-file
remove-tix-file:
	@ find . -name '*.tix' -delete
