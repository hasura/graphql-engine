WORKSPACE ?= $(shell dirname $(shell pwd))/

.PHONY: pipeline-build
## pipeline-build: check that the CI pipeline still builds
pipeline-build:
	cd .buildkite/pipeline-gen && go build ./...

.PHONY: pipeline-format
## pipeline-format: run go fmt on all the Go files in .buildkite
pipeline-format:
	cd .buildkite && gofmt -l -w -s .

.PHONY: pipeline-shellcheck
## pipeline-shellcheck: run shellcheck on all CI scripts in .buildkite/scripts
pipeline-shellcheck:
	shellcheck .buildkite/scripts/{.,**}/*.sh -P .buildkite/scripts
