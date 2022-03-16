# skip contrib with its generated .hs file because it doesn't
# come with a cabal file, which can trigger a bug in ormolu
FORMAT_HS_FILES = $(shell git ls-files '*.hs' '*.hs-boot' | grep -v '^contrib/')
FORMAT_CHANGED_HS_FILES = $(shell git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` \
				| grep '.*\(hs\|hs-boot\)$$' | grep -v '^contrib/')

ORMOLU_CHECK_VERSION = 0.3.0.0
ORMOLU_ARGS = --cabal-default-extensions
ORMOLU = ormolu
ORMOLU_VERSION = $(shell $(ORMOLU) --version | awk 'NR==1 { print $$2 }')

# default target
.PHONY: help
## help: prints help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

.PHONY: check-ormolu-version
check-ormolu-version:
	@if ! [ "$(ORMOLU_VERSION)" = "$(ORMOLU_CHECK_VERSION)" ]; then \
		echo "WARNING: ormolu version mismatch, expected $(ORMOLU_CHECK_VERSION)"; \
	fi

.PHONY: format-hs
## format-hs: auto-format Haskell source code using ormolu
format-hs: check-ormolu-version
	@echo running ormolu --mode inplace
	@$(ORMOLU) $(ORMOLU_ARGS) --mode inplace $(FORMAT_HS_FILES)

.PHONY: format-hs-changed
## format-hs-changed: auto-format Haskell source code using ormolu (changed files only)
format-hs-changed: check-ormolu-version
	@echo running ormolu --mode inplace
	@if [ -n "$(FORMAT_CHANGED_HS_FILES)" ]; then \
		$(ORMOLU) $(ORMOLU_ARGS) --mode inplace $(FORMAT_CHANGED_HS_FILES); \
	fi

.PHONY: check-format-hs
## check-format-hs: check Haskell source code formatting using ormolu
check-format-hs: check-ormolu-version
	@echo running ormolu --mode check
	@$(ORMOLU) $(ORMOLU_ARGS) --mode check $(FORMAT_HS_FILES)

.PHONY: format
format: format-hs

.PHONY: format-changed
format-changed: format-hs-changed

.PHONY: check-format
check-format: check-format-hs
