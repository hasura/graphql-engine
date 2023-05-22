# This is the admin secret for the Pro server.
# Override it with `make run-pro-server ADMIN_SECRET=my-override`.
ADMIN_SECRET ?= top-secret

# this connection should match postgres settings in `/docker-compose/databases.yaml`
POSTGRES_URL = postgresql://hasura:hasura@localhost:65002/hasura
ENABLED_LOG_TYPES = startup,http-log,webhook-log,websocket-log,query-log,execution-log

.PHONY: run-oss-server
## run-oss-server: run an OSS server backend by a PostgreSQL database
run-oss-server: $(CONSOLE_CE_ASSETS_PATH)
	docker compose up --wait postgres
	cabal run graphql-engine:exe:graphql-engine -- \
		--database-url '$(POSTGRES_URL)' \
		serve \
		--enable-console --console-assets-dir '$(CONSOLE_CE_ASSETS_PATH)' \
		--enabled-log-types '$(ENABLED_LOG_TYPES)'

.PHONY: run-pro-server
## run-pro-server: run a pro server backend by a PostgreSQL database
run-pro-server: $(CONSOLE_EE_ASSETS_PATH)
	docker compose up --wait postgres
	cabal run graphql-engine-pro:exe:graphql-engine-pro -- \
		--database-url '$(POSTGRES_URL)' \
		serve \
		--admin-secret '$(ADMIN_SECRET)' \
		--enable-console --console-assets-dir '$(CONSOLE_EE_ASSETS_PATH)' \
		--enabled-log-types '$(ENABLED_LOG_TYPES)'

.PHONY: run-dc-postgres-agent
## run-dc-postgres-agent:  run the new exciting Postgres data connector
run-dc-postgres-agent: remove-tix-file
	$(DC_POSTGRES_DOCKER_COMPOSE) up --wait
	cabal run postgres-agent

.PHONY: run-dc-postgres-agent-watch
## run-dc-postgres-agent-watch
run-dc-postgres-agent-watch:
	$(DC_POSTGRES_DOCKER_COMPOSE) up --wait
	watchexec -r -e .hs cabal run postgres-agent
