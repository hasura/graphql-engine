PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../get-version.sh)
BUILD_DIR ?= /build
BINARY ?= $(BUILD_DIR)/_cli_output/binaries/cli-hasura-linux-amd64
IMAGE_TAG ?= cli-migrations
BUILD_OUTPUT ?= $(BUILD_DIR)/_cli_migrations_output
CLI_EXT_MANIFEST_FILE ?= $(BUILD_DIR)/_cli_ext_output/manifest-dev.yaml

.PHONY: build-cli-migrations-v1
.ONESHELL:
build-cli-migrations-v1:
	cd v1
	cp ${BINARY} .
	docker build -t ${IMAGE_TAG} .
	docker save -o '$(BUILD_OUTPUT)/v1.tar' '$(IMAGE_TAG)'

.PHONY: test-cli-migrations-v1
.ONESHELL:
test-cli-migrations-v1:
	cd v1/test
	./test.sh

.PHONY: build-cli-migrations-v2
.ONESHELL:
build-cli-migrations-v2:
	cd v2
	cp ${BINARY} .
	cp ${CLI_EXT_MANIFEST_FILE} manifest.yaml
	docker build -t '${IMAGE_TAG}-v2' .
	docker save -o '$(BUILD_OUTPUT)/v2.tar' '$(IMAGE_TAG)-v2'

.PHONY: test-cli-migrations-v2
.ONESHELL:
test-cli-migrations-v2:
	cd v2/test
	./test.sh

.PHONY: all
all: build-cli-migrations-v1 build-cli-migrations-v2 test-cli-migrations-v1 test-cli-migrations-v2