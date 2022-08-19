# ghcid gets its own cache
GHCID_FLAGS = --builddir ./dist-newstyle/repl --repl-option -O0 --repl-option -fobject-code
GHCID_TESTS_FLAGS = --builddir ./dist-newstyle/repl-tests --repl-option -O0

PANE_WIDTH = $(shell tmux display -p "\#{pane_width}" || echo 80)
PANE_HEIGHT = $(shell tmux display -p "\#{pane_height}" || echo 30 )

# once ghcid's window errors are fixed we can remove this explicit width/height
# nonsense
# this needs to make it into ghcid: https://github.com/biegunka/terminal-size/pull/16
define run_ghcid_hspec_tests
	@if [[ $$(uname -p) == 'arm' ]]; then \
		HSPEC_MATCH="$(2)" ghcid -c "cabal repl $(1) $(GHCID_TESTS_FLAGS)" \
			--test "main" \
			--width=$(PANE_WIDTH) \
			--height=$(PANE_HEIGHT); \
	else \
  	HSPEC_MATCH="$(2)" ghcid -c "cabal repl $(1) $(GHCID_TESTS_FLAGS)" \
  		--test "main"; \
	fi
endef

define run_ghcid_main_tests
	@if [[ $$(uname -p) == 'arm' ]]; then \
		HSPEC_MATCH="$(3)" ghcid -c "cabal repl $(1) $(GHCID_TESTS_FLAGS)" \
			--test "main" \
			--setup ":set args $(2)" \
			--width=$(PANE_WIDTH) \
			--height=$(PANE_HEIGHT); \
	else \
  	HSPEC_MATCH="$(2)" ghcid -c "cabal repl $(1) $(GHCID_TESTS_FLAGS)" \
  		--test "main" \
			--setup ":set args $(2)"; \
	fi
endef


define run_ghcid
	@if [[ $$(uname -p) == 'arm' ]]; then \
		ghcid -c "cabal repl $(1) $(GHCID_FLAGS)" --width=$(PANE_WIDTH) --height=$(PANE_HEIGHT); \
	else \
  	ghcid -c "cabal repl $(1) $(GHCID_FLAGS)"; \
	fi
endef

.PHONY: ghcid-library
## ghcid-library: build and watch library in ghcid
ghcid-library:
	$(call run_ghcid,graphql-engine:lib:graphql-engine)

.PHONY: ghcid-tests
## ghcid-tests: build and watch main tests in ghcid
ghcid-tests:
	$(call run_ghcid,graphql-engine:test:graphql-engine-tests)

.PHONY: ghcid-hspec
## ghcid-hspec: build and watch tests-hspec in ghcid
ghcid-hspec:
	$(call run_ghcid,graphql-engine:tests-hspec)

.PHONY: ghcid-test-backends
## ghcid-test-backends: run all hspec tests in ghcid
ghcid-test-backends: remove-tix-file
	$(call run_ghcid_hspec_tests,graphql-engine:tests-hspec)

.PHONY: ghcid-test-bigquery
## ghcid-test-bigquery: run tests for BigQuery backend in ghcid
# will require some setup detailed here: https://github.com/hasura/graphql-engine-mono/tree/main/server/tests-hspec#required-setup-for-bigquery-tests
ghcid-test-bigquery: start-postgres remove-tix-file
	$(call run_ghcid_hspec_tests,graphql-engine:tests-hspec,BigQuery)

.PHONY: ghcid-test-sqlserver
## ghcid-test-sqlserver: run tests for SQL Server backend in ghcid
ghcid-test-sqlserver: start-postgres start-sqlserver remove-tix-file
	$(call run_ghcid_hspec_tests,graphql-engine:tests-hspec,SQLServer)

.PHONY: ghcid-test-mysql
## ghcid-test-mysql: run tests for MySQL backend in ghcid
ghcid-test-mysql: start-postgres start-mysql remove-tix-file
	$(call run_ghcid_hspec_tests,graphql-engine:tests-hspec,MySQL)

.PHONY: ghcid-test-citus
## ghcid-test-citus: run tests for Citus backend in ghcid
ghcid-test-citus: start-postgres start-citus remove-tix-file
	$(call run_ghcid_hspec_tests,graphql-engine:tests-hspec,Citus)

.PHONY: ghcid-library-pro
## ghcid-library-pro: build and watch pro library in ghcid
ghcid-library-pro:
	$(call run_ghcid,graphql-engine-pro:lib:graphql-engine-pro)

.PHONY: ghcid-test-unit
## ghcid-test-unit: build and run unit tests in ghcid
ghcid-test-unit: remove-tix-file
	$(call run_ghcid_main_tests,graphql-engine:graphql-engine-tests,unit)


