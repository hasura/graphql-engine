SHELL := /bin/bash

VERSION ?= $(shell ../scripts/get-version.sh)
export VERSION

registry := hasura
# This packager version is built using the packeger.df in the packaging folder:
# docker build -t "hasura/graphql-engine-packager:20210218" -f packager.df .
packager_ver := 20210218
SERVER_BUILD_OUTPUT ?= /build/_server_output
docs_output := /build/_server_output/docs.tar.gz

# Getting access to the built products with the `cabal v2-*` commands is really awkward; see
# <https://www.haskell.org/cabal/users-guide/nix-local-build.html#where-are-my-build-products> for a
# little more context. Ideally, we could use `cabal v2-install` for this, but `v2-install` does
# strange and complicated things, and I could not get it to work.
#
# This glob is a pretty heavy hammer designed to work regardless of `cabal-install` version (see the
# aforementioned link for why that’s tricky). If `cabal v2-*` ever gets a better way to do this,
# please replace this with something less hacky.
executables_glob := ../dist-newstyle/build/*/*/graphql-engine-*/**/opt/build/{graphql-engine/graphql-engine,graphql-engine-tests/graphql-engine-tests,graphql-engine-integration-tests/graphql-engine-integration-tests}

# assumes this is built in circleci
ci-build:
	## configure
	cabal v2-update --project-file=cabal.project.ci
	## build
	cabal v2-build --project-file=cabal.project.ci
	## install
	mkdir -p '$(SERVER_BUILD_OUTPUT)'
	echo '$(VERSION)' > '$(SERVER_BUILD_OUTPUT)/version.txt'
	shopt -s failglob globstar && cp $(executables_glob) '$(SERVER_BUILD_OUTPUT)/'

# assumes this is built in circleci
ci-docs:
	# build
	cabal haddock
	# copy
	cd ../dist-newstyle/build/*/*/graphql-engine-*/**/doc/html/graphql-engine/ && tar czf $(docs_output) *

packager: packaging/packager.df
	docker build -t '$(registry)/graphql-engine-packager:$(packager_ver)' -f packaging/packager.df ./packaging/

.PHONY: ci-build ci-docs packager
