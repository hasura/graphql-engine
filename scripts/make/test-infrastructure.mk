API_TESTS_DOCKER_COMPOSE = docker compose --project-directory=./server/lib/api-tests
DC_POSTGRES_DOCKER_COMPOSE = docker compose --project-directory=./pro/dc-agents/postgres/
API_PRO_TESTS_DOCKER_COMPOSE = docker compose --project-directory=./pro/server/lib/api-tests
PYTHON_TESTS_DOCKER_COMPOSE = docker compose --project-directory=./server/tests-py

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

.PHONY: build-backends
## build-backends: build Docker images for any backends that need them
build-backends:
	$(API_TESTS_DOCKER_COMPOSE) build

.PHONY: start-backends
## start-backends: start all known backends in Docker and wait for them to be ready
start-backends: build-backends
	$(API_TESTS_DOCKER_COMPOSE) up --detach --wait

.PHONY: stop-everything
## stop-everything: tear down test databases
stop-everything:
	$(API_TESTS_DOCKER_COMPOSE) down --volumes
	$(API_PRO_TESTS_DOCKER_COMPOSE) down --volumes
	$(PYTHON_TESTS_DOCKER_COMPOSE) down --volumes
	$(DC_POSTGRES_DOCKER_COMPOSE) down --volumes

.PHONY: remove-tix-file
remove-tix-file:
	@ find . -name '*.tix' -delete
