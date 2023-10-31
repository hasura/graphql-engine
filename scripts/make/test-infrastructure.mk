API_TESTS_DOCKER_COMPOSE = docker compose --project-directory=./server/lib/api-tests
API_TESTS_PRO_DOCKER_COMPOSE = docker compose --project-directory=./pro/server/lib/api-tests
PYTHON_TESTS_DOCKER_COMPOSE = docker compose --project-directory=./server/tests-py

TEST_MSSQL_CONNECTION_STRING = Driver={ODBC Driver 18 for SQL Server};Server=localhost,65003;Uid=sa;Pwd=Password!;Encrypt=optional
TEST_POSTGRES_URL = postgres://hasura:hasura@localhost:65002/hasura

.PHONY: start-api-tests-backends
## start-api-tests-backends: start all known backends in Docker and wait for them to be ready
start-api-tests-backends:
	$(API_TESTS_DOCKER_COMPOSE) up --build --detach --wait

.PHONY: start-api-tests-pro-backends
## start-api-tests-pro-backends: start all known backends in Docker and wait for them to be ready
start-api-tests-pro-backends:
	$(API_TESTS_PRO_DOCKER_COMPOSE) up --build --detach --wait

.PHONY: start-api-tests-pro-postgres
## start-api-tests-pro-backends: start the Postgres backend in Docker and wait for it to be ready
start-api-tests-pro-postgres:
	$(API_TESTS_PRO_DOCKER_COMPOSE) up --build --detach --wait postgres

.PHONY: stop-everything
## stop-everything: tear down test databases
stop-everything:
	docker compose down # don't discard volumes used for manual testing
	$(API_TESTS_DOCKER_COMPOSE) down --volumes
	$(API_TESTS_PRO_DOCKER_COMPOSE) down --volumes
	$(PYTHON_TESTS_DOCKER_COMPOSE) down --volumes

.PHONY: remove-tix-file
remove-tix-file:
	@ find . -name '*.tix' -delete
