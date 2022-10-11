SHELL := /bin/bash

VERSION ?= $(shell ../scripts/get-version.sh)
export VERSION

registry := hasura
# This packager version is built using the packeger.df in the packaging folder:
# docker build -t "hasura/graphql-engine-packager:20210218" -f packager.df .
packager_ver := 20210218
docs_output := /build/_server_output/docs.tar.gz

# assumes this is built in circleci
ci-docs:
	# build
	cabal haddock
	# copy
	cd ../dist-newstyle/build/*/*/graphql-engine-*/**/doc/html/graphql-engine/ && tar czf $(docs_output) *

packager: packaging/packager.df
	docker build -t '$(registry)/graphql-engine-packager:$(packager_ver)' -f packaging/packager.df ./packaging/

.PHONY: ci-build ci-docs packager
