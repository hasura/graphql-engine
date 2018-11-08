PWD := $(shell pwd)
PARENT_DIR := $(shell dirname $(PWD))
VERSION ?= $(shell ../scripts/get-version.sh)
OUTPUT_DIR := _output

# compile assets
.PHONY: assets
assets:
	go-bindata --pkg assets -o assets/assets.go assets/**/*.html

HAS_GOX := $(shell command -v gox;)
HAS_DEP := $(shell command -v dep;)
HAS_BINDATA := $(shell command -v go-bindata;)

.PHONY: deps
# get dependencies
deps:
ifndef HAS_DEP
	go get -u github.com/golang/dep/cmd/dep
endif
ifndef HAS_GOX
	go get github.com/mitchellh/gox
endif
ifndef HAS_BINDATA
	go get github.com/hasura/go-bindata/go-bindata
endif
	if [ ! -d "vendor" ]; then dep ensure -v -vendor-only; fi

# build cli locally, for all given platform/arch
.PHONY: build
build: export CGO_ENABLED=0
build:
	gox -ldflags '-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION) -s -w -extldflags "-static"' \
	-rebuild \
	-os="linux darwin windows" \
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
	go test -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=$(VERSION)" ./...

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
