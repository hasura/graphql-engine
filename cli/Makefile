PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../scripts/get-version.sh)
PLUGINS_BRANCH ?= master
OS ?= linux darwin windows
OUTPUT_DIR := _output

# compile assets
.PHONY: assets
assets:
ifndef HAS_PKGER
	go get github.com/markbates/pkger/cmd/pkger
endif
	pkger -o pkg/console/templates/packed -include /pkg/console/templates/gohtml/

HAS_GOX := $(shell command -v gox;)
HAS_PKGER := $(shell command -v pkger;)

.PHONY: deps
# get dependencies
deps:
	go mod download
ifndef HAS_GOX
	go get github.com/mitchellh/gox
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
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" -v -tags="${TEST_TAGS}" `go list ./... | grep -v integration_test`
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" -v -tags="${TEST_TAGS}" -run Commands/config=v1 ./integration_test
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" -v -tags="${TEST_TAGS}" -run Commands/config=v2 ./integration_test

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
