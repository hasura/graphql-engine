# default target
.PHONY: help
## help: prints help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

include ./scripts/make/tests.mk
include ./scripts/make/lint.mk
include ./scripts/make/build.mk
