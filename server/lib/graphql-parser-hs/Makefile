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
	  --enable-benchmarks \
	  all

.PHONY: build
build:
	$(CABAL) build \
	  --enable-tests \
	  --enable-benchmarks \
	  graphql-parser

.PHONY: build-all
build-all:
	$(CABAL) build \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: test-all
test-all:
	$(CABAL) test \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: bench-all
bench-all:
	$(CABAL) bench \
	  --enable-tests \
	  --enable-benchmarks \
	  all

.PHONY: repl
repl:
	$(CABAL) repl \
	  --repl-option='-fobject-code' \
	  --repl-option='-O0' \
	  graphql-parser

.PHONY: ghcid
ghcid:
	ghcid --command "\
	  $(CABAL) repl \
	    --repl-option='-fobject-code' \
	    --repl-option='-O0' \
	    graphql-parser \
	  "

.PHONY: ghcid-test
ghcid-test:
	ghcid \
	  --command "\
	    $(CABAL) repl \
	      --repl-option '-fobject-code' \
	      --repl-option '-O0' \
	      graphql-parser-test \
	    " \
	--test ":main"

.PHONY: ghcid-bench
ghcid-bench:
	ghcid \
	  --command "\
	    $(CABAL) repl \
	      --repl-option '-fobject-code' \
	      --repl-option '-O0' \
	      graphql-parser-bench \
	    "
