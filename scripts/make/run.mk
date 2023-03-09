# this connection should match postgres settings in `/docker-compose/databases.yaml`
POSTGRES_URL=postgresql://hasura:hasura@localhost:65002/hasura
CONSOLE_EE_ASSETS_PATH=./frontend/dist/app/server-assets-console-ee

.PHONY: run-pro-server
## run-pro-server: run a pro server backend by a postgres database
run-pro-server:
	cd frontend && npm install && npm run server-build:ee
	docker compose up -d --wait postgres
	cabal run graphql-engine-pro:exe:graphql-engine-pro serve -- \
		--enable-console --console-assets-dir '$(CONSOLE_EE_ASSETS_PATH)' \
		--database-url '$(POSTGRES_URL)'

