PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../get-version.sh)
BUILD_DIR ?= /build
BUILD_OUTPUT ?= $(BUILD_DIR)/_cli_migrations_output
SERVER_BUILD_OUTPUT ?= $(BUILD_DIR)/_server_output
CLI_BUILD_OUTPUT := $(BUILD_DIR)/_cli_output
SHELL := /bin/bash
.SHELLFLAGS := -ce

.ONESHELL:
prepare-local-env:
	# works on *nix systems
	MAKEFILE_DIR=$(shell pwd)
	mkdir /tmp/build
	cd /tmp/build
	mkdir _cli_migrations_output
	# get server image and save it	
	LATEST_RELEASE=$(shell curl --silent "https://api.github.com/repos/hasura/graphql-engine/releases" | jq '.[0]' | grep '"tag_name":' |  sed -E 's/.*"([^"]+)".*/\1/')
	mkdir _server_output && cd _server_output && docker pull hasura/graphql-engine:$$LATEST_RELEASE && docker save -o image.tar hasura/graphql-engine:$$LATEST_RELEASE && cd -
	mkdir -p _cli_output/binaries && cd _cli_output/binaries && curl --silent -LO https://github.com/hasura/graphql-engine/releases/download/$$LATEST_RELEASE/cli-hasura-linux-amd64 && cd -

	# edit makefile to use /tmp/build as BUILD_DIR
	cd $$MAKEFILE_DIR && sed -i 's/BUILD_DIR ?= \/build/BUILD_DIR ?= \/tmp\/build/' Makefile
reset-local-env:
	sed -i 's/BUILD_DIR ?= \/tmp\/build/BUILD_DIR ?= \/build/' Makefile
