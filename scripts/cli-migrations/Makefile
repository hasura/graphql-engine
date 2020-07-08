PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../get-version.sh)
BUILD_DIR ?= /build
BINARY ?= $(BUILD_DIR)/_cli_output/binaries/cli-hasura-linux-amd64
IMAGE_TAG ?= cli-migrations
BUILD_OUTPUT ?= $(BUILD_DIR)/_cli_migrations_output
CLI_EXT_MANIFEST_FILE ?= $(BUILD_DIR)/_cli_ext_output/manifest-dev.yaml
CLI_EXT_BINARY_NAME ?= cli-ext-hasura-linux.tar.gz
CLI_EXT_LINUX_BINARY_PATH ?=$(BUILD_DIR)/_cli_ext_output/$(CLI_EXT_BINARY_NAME)
SERVER_BUILD_OUTPUT := $(BUILD_DIR)/_server_output
HAS_YQ := $(shell command -v yq;)

.PHONY: load-server-image
load-server-image:
	docker load -i '$(SERVER_BUILD_OUTPUT)/image.tar'

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
ifndef HAS_YQ
	curl -LO https://github.com/mikefarah/yq/releases/download/3.3.2/yq_linux_amd64 && chmod +x yq_linux_amd64 && mv yq_linux_amd64 /usr/local/bin/yq
endif
	cd v2
	cp ${BINARY} .
	# copy linux binary 
	cp ${CLI_EXT_LINUX_BINARY_PATH} .

	# edit manifest file cli-ext linux uri to file:///tmp/cli-ext-hasura-linux.tar.gz
	yq write -i ${CLI_EXT_MANIFEST_FILE} "platforms[0].uri" "file:///tmp/cli-ext/${CLI_EXT_BINARY_NAME}"
	cp ${CLI_EXT_MANIFEST_FILE} manifest.yaml

	# edit hasura home template directory
	TEMPLATE_CLI_EXT_INDEX_DIR="./hasura-home-dir-tmpl/index/plugins/cli-ext"
	CLI_EXT_VERSION=$(yq read manifest.yaml version)
	mkdir -p ${TEMPLATE_CLI_EXT_INDEX_DIR}/${CLI_EXT_VERSION}
	cp manifest.yaml ${TEMPLATE_CLI_EXT_INDEX_DIR}/${CLI_EXT_VERSION}/manifest.yaml

	docker build -t '${IMAGE_TAG}-v2' .
	docker save -o '$(BUILD_OUTPUT)/v2.tar' '$(IMAGE_TAG)-v2'

.PHONY: test-cli-migrations-v2
.ONESHELL:
test-cli-migrations-v2:
	cd v2/test
	./test.sh
	./test-upgrade-from-latest-release.sh

.PHONY: all
all: load-server-image build-cli-migrations-v1 build-cli-migrations-v2 test-cli-migrations-v1 test-cli-migrations-v2
