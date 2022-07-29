# skip contrib with its generated .hs file because it doesn't
# come with a cabal file, which can trigger a bug in ormolu
HS_FILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -v '^contrib/')
CHANGED_HS_FILES = $(shell git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep '.*\(\.hs\|hs-boot\)$$' | grep -v '^contrib/')

NIX_FILES = $(shell git ls-files '*.nix' 'nix/*.nix')

SHELL_FILES = $(shell git ls-files '*.sh')
CHANGED_SHELL_FILES = $(shell git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep '.*\.sh$$')

HLINT = hlint
HLINT_VERSION = $(shell $(HLINT) --numeric-version)
HLINT_CHECK_VERSION = $(shell jq '.hlint' ./server/VERSIONS.json)

NIX_FMT = nixpkgs-fmt

ORMOLU = ormolu
ORMOLU_ARGS = --cabal-default-extensions
ORMOLU_VERSION = $(shell $(ORMOLU) --version | awk 'NR==1 { print $$2 }')
ORMOLU_CHECK_VERSION = $(shell jq '.ormolu' ./server/VERSIONS.json)

SHELLCHECK = shellcheck

.PHONY: check-hlint-version
check-hlint-version:
	@if ! [ "$(HLINT_VERSION)" = "$(HLINT_CHECK_VERSION)" ]; then \
		echo "WARNING: hlint version mismatch, expected $(HLINT_CHECK_VERSION) but got $(HLINT_VERSION)"; \
	fi

.PHONY: check-ormolu-version
check-ormolu-version:
	@if ! [ "$(ORMOLU_VERSION)" = "$(ORMOLU_CHECK_VERSION)" ]; then \
		echo "WARNING: ormolu version mismatch, expected $(ORMOLU_CHECK_VERSION) but got $(ORMOLU_VERSION)"; \
	fi

.PHONY: format-hs
## format-hs: auto-format Haskell source code using ormolu
format-hs: check-ormolu-version
	@echo running ormolu --mode inplace
	@$(ORMOLU) $(ORMOLU_ARGS) --mode inplace $(HS_FILES)

.PHONY: format-hs-changed
## format-hs-changed: auto-format Haskell source code using ormolu (changed files only)
format-hs-changed: check-ormolu-version
	@echo running ormolu --mode inplace
	@if [ -n "$(CHANGED_HS_FILES)" ]; then \
		$(ORMOLU) $(ORMOLU_ARGS) --mode inplace $(CHANGED_HS_FILES); \
	fi

.PHONY: check-format-hs
## check-format-hs: check Haskell source code formatting using ormolu
check-format-hs: check-ormolu-version
	@echo running ormolu --mode check
	@$(ORMOLU) $(ORMOLU_ARGS) --mode check $(HS_FILES)

.PHONY: check-format-hs-changed
## check-format-hs-changed: check Haskell source code formatting using ormolu (changed-files-only)
check-format-hs-changed: check-ormolu-version
	@echo running ormolu --mode check
	@if [ -n "$(CHANGED_HS_FILES)" ]; then \
		$(ORMOLU) $(ORMOLU_ARGS) --mode check $(CHANGED_HS_FILES); \
	fi

# We don't bother checking only changed *.nix files, as there's so few.

.PHONY: format-nix
## format-nix: auto-format Nix source code using `nixpkgs-fmt`
format-nix:
	@if command -v $(NIX_FMT) > /dev/null; then \
		echo "running $(NIX_FMT)"; \
		$(NIX_FMT) $(NIX_FILES); \
	else \
		echo "$(NIX_FMT) is not installed; skipping"; \
	fi

.PHONY: check-format-nix
## check-format-nix: check Nix source code using `nixpkgs-fmt`
check-format-nix:
	@if command -v $(NIX_FMT) > /dev/null; then \
		echo "running $(NIX_FMT) --check"; \
		$(NIX_FMT) --check $(NIX_FILES); \
	else \
		echo "$(NIX_FMT) is not installed; skipping"; \
	fi

.PHONY: format
format: format-hs format-nix

.PHONY: format-changed
format-changed: format-hs-changed format-nix

.PHONY: check-format
check-format: check-format-hs check-format-nix

.PHONY: check-format-changed
check-format-changed: check-format-hs-changed check-format-nix

.PHONY: lint-hpack
## lint-hpack: ensure that Cabal files are up-to-date with hpack files
lint-hpack:
	@echo running hpack
	@ $(foreach cabal_file,$(GENERATED_CABAL_FILES),./scripts/hpack.sh --check $(cabal_file);)

.PHONY: lint-hs
## lint-hs: lint Haskell code using `hlint`
lint-hs: check-hlint-version
	@echo running hlint
	@$(HLINT) $(HS_FILES)

.PHONY: lint-hs-changed
## lint-hs-changed: lint Haskell code using `hlint` (changed files only)
lint-hs-changed: check-hlint-version
	@echo running hlint
	@if [ -n "$(CHANGED_HS_FILES)" ]; then \
		$(HLINT) $(CHANGED_HS_FILES); \
	fi

.PHONY: lint-shell
## lint-shell: lint shell scripts using `shellcheck`
lint-shell:
	@echo running shellcheck
	@$(SHELLCHECK) $(SHELL_FILES)

.PHONY: lint-shell-changed
## lint-shell-changed: lint shell scripts using `shellcheck` (changed files only)
lint-shell-changed:
	@echo running shellcheck
	@if [ -n "$(CHANGED_SHELL_FILES)" ]; then \
		$(SHELLCHECK) $(CHANGED_SHELL_FILES); \
	fi

.PHONY: lint
lint: lint-hpack lint-hs lint-shell check-format

.PHONY: lint-changed
lint-changed: lint-hpack lint-hs-changed lint-shell-changed check-format-changed
