GHCID_FLAGS = --builddir ./dist-newstyle/repl --repl-option -O0 --repl-option -fobject-code
PANE_WIDTH = $(shell tmux display -p "\#{pane_width}" || echo 80)
PANE_HEIGHT = $(shell tmux display -p "\#{pane_height}" || echo 30 )


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

.PHONY: ghcid-hspec
## ghcid-hspec: build and watch tests-hspec in ghcid
ghcid-hspec:
	$(call run_ghcid,graphql-engine:tests-hspec)

