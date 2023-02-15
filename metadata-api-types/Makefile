SHELL := bash -e -u -o pipefail

SCHEMA_FILE := $(abspath ../metadata.openapi.json)
TYPESCRIPT_ROOT := typescript
TYPESCRIPT_SRC := ${TYPESCRIPT_ROOT}/src
PATCHES := $(wildcard ${TYPESCRIPT_ROOT}/patches/*.patch)

# default target
.PHONY: help
## help: prints help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

.PHONY: generate-types
## generate-types: Generate the TypeScript types for working with the Metadata API
generate-types: ${TYPESCRIPT_SRC}

.PHONY: typecheck
## typecheck: Typechecks generated type definitions
typecheck: typecheck-metadata-api-types

${TYPESCRIPT_SRC}: ${SCHEMA_FILE} ${TYPESCRIPT_ROOT}/package.json ${TYPESCRIPT_ROOT}/package-lock.json ${PATCHES}
	./scripts/generate-types.sh "${SCHEMA_FILE}"

.PHONY: typecheck-metadata-api-types
## typecheck-metadata-api-types: Typechecks the metadata-api-types
typecheck-metadata-api-types:
	cd ${TYPESCRIPT_ROOT} && \
	npm run typecheck
