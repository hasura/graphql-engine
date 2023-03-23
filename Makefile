SHELL := bash -e -u -o pipefail

# default target
.PHONY: help
## help: prints help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

include ./scripts/make/build.mk
include ./scripts/make/ci.mk
include ./scripts/make/frontend.mk
include ./scripts/make/ghcid.mk
include ./scripts/make/legacy-tests.mk
include ./scripts/make/lint.mk
include ./scripts/make/metadata-api.mk
include ./scripts/make/repl.mk
include ./scripts/make/run.mk
include ./scripts/make/tests.mk
include ./scripts/make/test-infrastructure.mk
