.PHONY: all
all:

# when running locally, use the postgres instance from the `docker-compose.yml`
# in this repo
DOCKER_POSTGRES_DATABASE_URL=postgresql://hasura:hasura@127.0.0.1:64001/hasura

.PHONY: ormolu
ormolu:
	find src test bench -name '*.hs' | xargs ormolu -ie

.PHONY: format
format:
	cabal-fmt -i pg-client.cabal
	make ormolu

PROJECT ?= cabal.project
CABAL = cabal --project=$(PROJECT)

.PHONY: freeze
freeze:
	$(CABAL) freeze \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: configure
configure:
	$(CABAL) configure \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: update
update:
	$(CABAL) update

.PHONY: build-deps
build-deps:
	$(CABAL) build \
	  --only-dependencies \
	  --enable-tests \
	  --enable-benchmarks

.PHONY: build
build:
	$(CABAL) build \
	  --enable-tests \
	  --enable-benchmarks \
	  pg-client

.PHONY: build-all
build-all:
	$(CABAL) build \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: start-dbs
start-dbs:
	docker-compose up -d --wait

.PHONY: stop-dbs
stop-dbs:
	docker-compose down -v

.PHONY: test-ci
test-ci:
	$(CABAL) test \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: test-all
test-all: start-dbs
	export DATABASE_URL=$(DOCKER_POSTGRES_DATABASE_URL) && \
	make test-ci
	make stop-dbs

.PHONY: ghcid
ghcid:
	ghcid --command "\
	  $(CABAL) repl \
	    --repl-option='-fobject-code' \
	    --repl-option='-O0' \
	    pg-client \
	  "

.PHONY: ghcid-test
ghcid-test:
	ghcid \
	  --command "\
	    $(CABAL) repl \
	      --repl-option='-fobject-code' \
	      --repl-option='-O0' \
	      pg-client-test \
	    " \
