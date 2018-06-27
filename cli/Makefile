PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION := $(shell build/get-version.sh)
OUTPUT_DIR := _output

.PHONY: build test clean

# compile assets
assets:
	go get github.com/hasura/go-bindata/go-bindata
	go-bindata --pkg assets -o manifests/assets.go manifests/console.html

# get dependencies
deps:
	go get -u github.com/golang/dep/cmd/dep
	dep ensure

# build cli locally, for all given platform/arch
build:
	go get github.com/mitchellh/gox
	gox -ldflags "-X github.com/hasura/graphql-engine/cli.version=$(VERSION)" \
	-os="linux darwin windows" \
	-arch="amd64" \
	-output="$(OUTPUT_DIR)/$(VERSION)/hasura-{{.OS}}-{{.Arch}}" \
	./cmd/hasura/

# run tests
test:
	go test ./...

# clean the output directory
clean:
	rm -rf "$(OUTPUT_DIR)"

all: assets deps test build

# build cli inside a docker container
all-in-docker:
	docker build -t hasura-graphql-cli-builder -f build/builder.dockerfile build
	docker run --rm -it \
	-v $(PARENT_DIR):/go/src/github.com/hasura/graphql-engine \
	hasura-graphql-cli-builder \
	make all
