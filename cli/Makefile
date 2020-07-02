PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../scripts/get-version.sh)
PLUGINS_BRANCH ?= master
OS ?= linux darwin windows
OUTPUT_DIR := _output

HAS_GOX := $(shell command -v gox;)
HAS_PKGER := $(shell command -v pkger;)
HAS_GINKGO := $(shell command -v ginkgo;)

# compile assets
.PHONY: assets
assets:
ifndef HAS_PKGER
	go get github.com/markbates/pkger/cmd/pkger
endif
	pkger -o pkg/console/templates/packed -include /pkg/console/templates/gohtml/

.PHONY: deps
# get dependencies
deps:
	go mod download
ifndef HAS_GOX
	go get github.com/mitchellh/gox
endif
ifndef HAS_GINKGO
	go get github.com/onsi/ginkgo/ginkgo
	go get github.com/onsi/gomega/...
endif

# build cli locally, for all given platform/arch
.PHONY: build
build: export CGO_ENABLED=0
build:
	gox -ldflags '-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION) -X github.com/hasura/graphql-engine/cli/plugins.IndexBranchRef=$(PLUGINS_BRANCH) -s -w -extldflags "-static"' \
	-rebuild \
	-os="$(OS)" \
	-arch="amd64" \
	-output="$(OUTPUT_DIR)/$(VERSION)/cli-hasura-{{.OS}}-{{.Arch}}" \
	./cmd/hasura/

# compress
.PHONY: compress
compress:
	ls $(OUTPUT_DIR)/$(VERSION)/cli-hasura-* | xargs upx

# to be executed in circle-ci only
ci-copy-binary:
	mkdir -p /build/_cli_output/binaries
	cp $(OUTPUT_DIR)/$(VERSION)/cli-hasura-* /build/_cli_output/binaries
	echo "$(VERSION)" > /build/_cli_output/version.txt

# run tests
.PHONY: test
test:
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" -v -tags="${TEST_TAGS}" `go list ./... | grep -v integration | grep -v e2e`
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" -v -tags="${TEST_TAGS}" -run Commands/config=v1 ./tests/integration
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" -v -tags="${TEST_TAGS}" -run Commands/config=v2 ./tests/integration

test-e2e-ci:
	HASURA_CLI_E2E_TESTS_HGE_BINARY_PATH="/build/_server_output/graphql-engine" HASURA_CLI_E2E_TESTS_CLI_BINARY_PATH="/build/_cli_output/binaries/cli-linux-linux-amd64" ginkgo tests/e2e


# clean the output directory
.PHONY: clean
clean:
	rm -rf "$(OUTPUT_DIR)"

.PHONY: all
all: deps assets test build

# build cli inside a docker container
all-in-docker:
	docker build -t hasura-graphql-cli-builder -f build/builder.dockerfile build
	docker run --rm -it \
	-v $(PARENT_DIR):/go/src/github.com/hasura/graphql-engine \
	hasura-graphql-cli-builder \
	make all
