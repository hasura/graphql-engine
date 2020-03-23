help:
	@echo "available commands:"
	@echo "  build:   builds console and server"
	@echo "  console: builds console"
	@echo "  server:  builds server"

build: console server

server:
	cd server && cabal new-install && cabal new-build

console:
	cd console && npm ci && npm run server-build


hs-tests:
	cd server && cabal new-run -- 'test:graphql-engine-tests' --database-url='postgres://localhost:4200/hasura-tests'

py-tests:
	./run-py-tests.sh

.PHONY: help build server console hs-tests py-tests
