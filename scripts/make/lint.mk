# skip server/forks because it contains forks of other projects, and
# we want to keep the diff between the fork and the original library as small
# as possible
HS_FILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -E -v '^server/forks/')
CHANGED_HS_FILES = $(shell git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep '.*\(\.hs\|hs-boot\)$$' | grep -E -v '^server/forks/')

NIX_FILES = $(shell git ls-files '*.nix' 'nix/*.nix')

SHELL_FILES = $(shell git ls-files '*.sh')
CHANGED_SHELL_FILES = $(shell git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep '.*\.sh$$')

HLINT = hlint
HLINT_VERSION = $(shell $(HLINT) --numeric-version)
HLINT_CHECK_VERSION = $(shell jq '.hlint' ./server/VERSIONS.json)

NIX_FMT = nixpkgs-fmt

ORMOLU = ormolu
ORMOLU_VERSION = $(shell $(ORMOLU) --version | awk 'NR==1 { print $$2 }')
ORMOLU_CHECK_VERSION = $(shell jq '.ormolu' ./server/VERSIONS.json)

# Run Shellcheck with access to any file that's sourced, relative to the script's own directory
SHELLCHECK = shellcheck --external-sources --source-path=SCRIPTDIR

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
	@echo running $(ORMOLU) --mode inplace
	@$(ORMOLU) --mode inplace $(HS_FILES)

.PHONY: format-hs-changed
## format-hs-changed: auto-format Haskell source code using ormolu (changed files only)
format-hs-changed: check-ormolu-version
	@echo running $(ORMOLU) --mode inplace
	@if [ -n "$(CHANGED_HS_FILES)" ]; then \
		$(ORMOLU) --mode inplace $(CHANGED_HS_FILES); \
	fi

.PHONY: check-format-hs
## check-format-hs: check Haskell source code formatting using ormolu
check-format-hs: check-ormolu-version
	@echo running $(ORMOLU) --mode check
	@$(ORMOLU) --mode check $(HS_FILES)

.PHONY: check-format-hs-changed
## check-format-hs-changed: check Haskell source code formatting using ormolu (changed-files-only)
check-format-hs-changed: check-ormolu-version
	@echo running $(ORMOLU) --mode check
	@if [ -n "$(CHANGED_HS_FILES)" ]; then \
		$(ORMOLU) --mode check $(CHANGED_HS_FILES); \
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

.PHONY: format-frontend
## format-frontend: auto-format all frontend code
format-frontend: frontend/node_modules
	@echo 'running nx format:write'
	cd frontend && yarn format:write:all

.PHONY: format-frontend-changed
## format-frontend-changed: auto-format all frontend code (changed files only)
format-frontend-changed: frontend/node_modules
	@echo 'running nx format:write'
	cd frontend && yarn format:write

.PHONY: check-format-frontend
## check-format-frontend: check frontend code
check-format-frontend: frontend/node_modules
	@echo 'running nx format:check'
	cd frontend && yarn nx format:check --base=origin/main

.PHONY: check-format-frontend-changed
## check-format-frontend-changed: check frontend code (changed files only)
check-format-frontend-changed: frontend/node_modules
	@echo 'running nx format:check'
	cd frontend && yarn nx format:check --base=origin/main

.PHONY: format
format: format-hs format-nix format-frontend

.PHONY: format-changed
format-changed: format-hs-changed format-nix format-frontend-changed

.PHONY: check-format
check-format: check-format-hs check-format-nix check-format-frontend

.PHONY: check-format-changed
check-format-changed: check-format-hs-changed check-format-nix check-format-frontend-changed

.PHONY: lint-hs
## lint-hs: lint Haskell code using `hlint`
lint-hs: check-hlint-version
	@echo running hlint
	@output=$$(mktemp); \
	trap 'rm $$output' EXIT; \
	$(HLINT) --no-exit-code $(HS_FILES) | tee "$$output"; \
	if grep -E '^[^ ]+: (Error|Warning):' "$$output" > /dev/null; then \
		echo 'Errors or warnings found.'; \
		exit 1; \
	fi

.PHONY: lint-hs-changed
## lint-hs-changed: lint Haskell code using `hlint` (changed files only)
lint-hs-changed: check-hlint-version
	@echo running hlint
	@if [[ -n "$(CHANGED_HS_FILES)" ]]; then \
		output=$$(mktemp); \
		trap 'rm $$output' EXIT; \
		$(HLINT) --no-exit-code $(CHANGED_HS_FILES) | tee "$$output"; \
		if grep -E '^[^ ]+: (Error|Warning):' "$$output" > /dev/null; then \
			echo 'Errors or warnings found.'; \
			exit 1; \
		fi; \
	fi

.PHONY: lint-hs-fix
## lint-hs-fix: lint Haskell code using `hlint` and attempt to fix warnings using `apply-refact`
lint-hs-fix: check-hlint-version
	@echo running hlint --refactor
	@echo $(HS_FILES) | xargs -n1 $(HLINT) --refactor --refactor-options='--inplace'

.PHONY: lint-hs-fix-changed
## lint-hs-fix-changed: lint Haskell code using `hlint` and attempt to fix warnings using `apply-refact` (changed files only)
lint-hs-fix-changed: check-hlint-version
	@echo running hlint --refactor
	@echo $(CHANGED_HS_FILES) | xargs --no-run-if-empty -n1 $(HLINT) --refactor --refactor-options='--inplace'

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

.PHONY: lint-frontend
## lint-frontend: lint all frontend code
lint-frontend: frontend/node_modules
	@echo 'running nx lint'
	cd frontend && yarn lint

.PHONY: lint-frontend-changed
## lint-frontend-changed: lint all frontend code
lint-frontend-changed: frontend/node_modules
	@echo 'running nx lint'
	cd frontend && yarn nx affected --target=lint --fix --parallel=3 --base=origin/main

.PHONY: lint
## lint: run all lint commands, and check formatting
lint: lint-hs lint-shell lint-frontend check-format

.PHONY: lint-changed
## lint: run all lint commands, and check formatting (changed files only)
lint-changed: lint-hs-changed lint-shell-changed lint-frontend-changed check-format-changed
