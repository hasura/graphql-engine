PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../get-version.sh)
BUILD_DIR ?= /build
BUILD_OUTPUT ?= $(BUILD_DIR)/_cli_migrations_output
SERVER_BUILD_OUTPUT := $(BUILD_DIR)/_server_output
SHELL := /bin/bash
.SHELLFLAGS := -ce

.PHONY: build-cli-migrations-v2
.ONESHELL:
build-cli-migrations-v2:
	cd v2
	./build.sh $(SERVER_BUILD_OUTPUT) $(BUILD_OUTPUT)

.PHONY: test-cli-migrations-v2
.ONESHELL:
test-cli-migrations-v2:
	cd v2/test
	./test.sh

.PHONY: all
all: build-cli-migrations-v2 test-cli-migrations-v2
