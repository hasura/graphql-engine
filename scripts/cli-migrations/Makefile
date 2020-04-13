PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../get-version.sh)
BINARY ?= /build/_cli_output/binaries/cli-hasura-linux-amd64
REGISTRY ?= 
IMAGE_TAG ?= "hasura/graphql-engine:${VERSION}.cli-migrations"
BUILD_OUTPUT := /build/_cli_migrations_output
CLI_EXT_MANIFEST_FILE := /build/_cli_ext_output/manifest-dev.yaml

.PHONY: build-cli-migrations-v1
build-cli-migrations-v1:
	cd v1
	cp ${BINARY} .
	docker build -t ${IMAGE_TAG} .
	docker save -o '$(BUILD_OUTPUT)/v1.tar' '$(registry)/graphql-engine:$(VERSION)'

.PHONY: build-cli-migrations-v2
build-cli-migrations-v2:
	cd v2
	cp ${BINARY} .
	cp ${CLI_EXT_MANIFEST_FILE} manifest.yaml
	docker build -t ${IMAGE_TAG} .
	docker save -o '$(build_output)/v2.tar' '$(registry)/graphql-engine:$(VERSION)'