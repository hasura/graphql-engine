SHELL := bash -e -u -o pipefail

SCHEMA_FILE := $(abspath ../metadata.openapi.json)
TYPESCRIPT_ROOT := typescript
TYPESCRIPT_SRC := ${TYPESCRIPT_ROOT}/src
PATCHES := $(wildcard ${TYPESCRIPT_ROOT}/patches/*.patch)

FILE_NAME := metadata.openapi

GO_ROOT := go
GO_FILE := ${FILE_NAME}.go
GO_TARGET := ${GO_ROOT}/${GO_FILE}

RUST_ROOT := rust
RUST_FILE := ${FILE_NAME}.rs
RUST_TARGET := ${RUST_ROOT}/${RUST_FILE}

HASKELL_ROOT := haskell
HASKELL_FILE := ${FILE_NAME}.hs
HASKELL_TARGET := ${HASKELL_ROOT}/${HASKELL_FILE}

KOTLIN_ROOT := kotlin
KOTLIN_FILE := ${FILE_NAME}.kt
KOTLIN_TARGET := ${KOTLIN_ROOT}/${KOTLIN_FILE}

# default target
.PHONY: help
## help: prints help message
help:
	@echo "Usage:"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

.PHONY: generate-types
## generate-types: Generate types for working with the Metadata API
generate-types: generate-typescript-types generate-go-types generate-rust-types generate-haskell-types generate-kotlin-types

.PHONY: generate-typescript-types
## generate-typescript-types: Generate the TypeScript types for working with the Metadata API
generate-typescript-types: ${TYPESCRIPT_SRC}

.PHONY: generate-go-types
## generate-go-types: Generate the Go types for working with the Metadata API
generate-go-types: ${GO_TARGET}

.PHONY: generate-rust-types
## generate-rust-types: Generate the Rust types for working with the Metadata API
generate-rust-types: ${RUST_TARGET}

.PHONY: generate-haskell-types
## generate-haskell-types: Generate the Haskell types for working with the Metadata API
generate-haskell-types: ${HASKELL_TARGET}

.PHONY: generate-kotlin-types
## generate-kotlin-types: Generate the Kotlin types for working with the Metadata API
generate-kotlin-types: ${KOTLIN_TARGET}

.PHONY: typecheck
## typecheck: Typechecks generated type definitions
typecheck: typecheck-metadata-api-types

.PHONY: typecheck-metadata-api-types
## typecheck-metadata-api-types: Typechecks the metadata-api-types
typecheck-metadata-api-types:
	cd ${TYPESCRIPT_ROOT} && \
	npm run typecheck

${TYPESCRIPT_SRC}: ${SCHEMA_FILE} ${TYPESCRIPT_ROOT}/package.json ${TYPESCRIPT_ROOT}/package-lock.json ${PATCHES}
	./scripts/generate-typescript-types.sh "${SCHEMA_FILE}"

${GO_TARGET}: ${TYPESCRIPT_SRC}
	./scripts/generate-types-for-lang.sh ${GO_ROOT} ${GO_FILE}

${RUST_TARGET}: ${TYPESCRIPT_SRC}
	./scripts/generate-types-for-lang.sh ${RUST_ROOT} ${RUST_FILE}

${HASKELL_TARGET}: ${TYPESCRIPT_SRC}
	./scripts/generate-types-for-lang.sh ${HASKELL_ROOT} ${HASKELL_FILE}

${KOTLIN_TARGET}: ${TYPESCRIPT_SRC}
	./scripts/generate-types-for-lang.sh ${KOTLIN_ROOT} ${KOTLIN_FILE}
