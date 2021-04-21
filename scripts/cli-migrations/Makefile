PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../get-version.sh)
BUILD_DIR ?= /build
BUILD_OUTPUT ?= $(BUILD_DIR)/_cli_migrations_output
SERVER_BUILD_OUTPUT := $(BUILD_DIR)/_server_output
CLI_BUILD_OUTPUT := $(BUILD_DIR)/_cli_output
SHELL := /bin/bash
.SHELLFLAGS := -ce

.PHONY: build-cli-migrations-v2
.ONESHELL:
build-cli-migrations-v2:
	cd v2
	./build.sh $(SERVER_BUILD_OUTPUT) $(CLI_BUILD_OUTPUT) $(BUILD_OUTPUT)

.PHONY: test-cli-migrations-v2
.ONESHELL:
test-cli-migrations-v2:
	cd v2/test
	./test.sh

.PHONY: build-cli-migrations-v3
.ONESHELL:
build-cli-migrations-v3:
	cd v3
	./build.sh $(SERVER_BUILD_OUTPUT) $(CLI_BUILD_OUTPUT) $(BUILD_OUTPUT)

.PHONY: test-cli-migrations-v3
.ONESHELL:
test-cli-migrations-v3:
	cd v3/test
	./test.sh

.ONESHELL:
prepare-local-env:
	# works on *nix systems
	MAKEFILE_DIR=$(shell pwd)
	mkdir /tmp/build
	cd /tmp/build
	mkdir _cli_migrations_output
	# get server image and save it	
	LATEST_RELEASE=$(shell curl --silent "https://api.github.com/repos/hasura/graphql-engine/releases" | jq '.[0]' | grep '"tag_name":' |  sed -E 's/.*"([^"]+)".*/\1/')
	mkdir _server_output && cd _server_output && docker save -o image.tar hasura/graphql-engine:$$LATEST_RELEASE && cd -
	mkdir -p _cli_output/binaries && cd _cli_output/binaries && curl -LO https://github.com/hasura/graphql-engine/releases/download/$$LATEST_RELEASE/cli-hasura-linux-amd64 && cd -

	# edit makefile to use /tmp/build as BUILD_DIR
	cd $$MAKEFILE_DIR && sed -i 's/BUILD_DIR ?= \/build/BUILD_DIR ?= \/tmp\/build/' Makefile
reset-local-env:
	sed -i 's/BUILD_DIR ?= \/tmp\/build/BUILD_DIR ?= \/build/' Makefile

.PHONY: all
all: build-cli-migrations-v2 test-cli-migrations-v2 build-cli-migrations-v3 test-cli-migrations-v3
