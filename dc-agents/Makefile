SHELL := bash -e -u -o pipefail

# default target
.PHONY: help
## help: prints help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

.PHONY: typecheck
## typecheck: Typechecks all workspaces
typecheck: typecheck-dc-api-types typecheck-reference-agent typecheck-sqlite-agent

.PHONY: typecheck-dc-api-types
## typecheck-dc-api-types: Typechecks the dc-api-types
typecheck-dc-api-types:
	npm run -w dc-api-types typecheck

.PHONY: typecheck-reference-agent
## typecheck-reference-agent: Typechecks the Reference agent
typecheck-reference-agent:
	npm run -w reference typecheck

.PHONY: typecheck-sqlite-agent
## typecheck-sqlite-agent: Typechecks the SQLite agent
typecheck-sqlite-agent:
	npm run -w sqlite typecheck

.PHONY: start-reference-agent
## start-reference-agent: Starts the Reference agent
start-reference-agent:
	npm start -w reference

.PHONY: start-sqlite-agent
## start-sqlite-agent: Starts the SQLite agent
start-sqlite-agent:
	npm start -w sqlite

TESTS_DC_API := cabal run dc-api:test:tests-dc-api --

.PHONY: generate-types
## generate-types: Generates the TypeScript API types in dc-api-types
generate-types: export TESTS_DC_API := $(TESTS_DC_API)
generate-types:
	./scripts/generate-types.sh

.PHONY: regenerate-types
## regenerate-types: Regenerates the TypeScript API types in dc-api-types from the original Haskell types
regenerate-types: export TESTS_DC_API := $(TESTS_DC_API)
regenerate-types:
	rm -f ./dc-api-types/src/agent.openapi.json
	./scripts/generate-types.sh

.PHONY: update-api-types-deps
## update-api-types-deps: Updates packages that are dependant on dc-api-types with its current version from its package.json
update-api-types-deps:
	./scripts/update-api-types-deps.sh

.PHONY: derive-lockfiles
## derive-lockfiles: Derives individual lockfiles for the workspace packages from the lockfile in the root project
derive-lockfiles:
	npx ts-node ./scripts/derive-lockfile.ts -l package-lock.json -w reference -w sqlite
