SHELL := /bin/bash

VERSION ?= $(shell ../scripts/get-version.sh)
export VERSION

registry := hasura
# This packager version is built using the packeger.df in the packaging folder:
# docker build -t "hasura/graphql-engine-packager:20210218" -f packager.df .
packager_ver := 20210218
pg_dump_ver := 13
build_output := /build/_server_output
docs_output := /build/_server_output/docs.tar.gz

# Getting access to the built products with the `cabal v2-*` commands is really awkward; see
# <https://www.haskell.org/cabal/users-guide/nix-local-build.html#where-are-my-build-products> for a
# little more context. Ideally, we could use `cabal v2-install` for this, but `v2-install` does
# strange and complicated things, and I could not get it to work.
#
# This glob is a pretty heavy hammer designed to work regardless of `cabal-install` version (see the
# aforementioned link for why that’s tricky). If `cabal v2-*` ever gets a better way to do this,
# please replace this with something less hacky.
executables_glob := ../dist-newstyle/build/*/*/graphql-engine-*/**/opt/build/{graphql-engine/graphql-engine,graphql-engine-tests/graphql-engine-tests}

# assumes this is built in circleci
ci-build:
	## configure
	cabal v2-update --project-file=cabal.project.ci
	## build
	cabal v2-build --project-file=cabal.project.ci
	## install
	mkdir -p '$(build_output)'
	echo '$(VERSION)' > '$(build_output)/version.txt'
	shopt -s failglob globstar && cp $(executables_glob) '$(build_output)/'

# assumes this is built in circleci
ci-docs:
	# build
	cabal haddock
	# copy
	cd ../dist-newstyle/build/*/*/graphql-engine-*/**/doc/html/graphql-engine/ && tar czf $(docs_output) *

# assumes this is built in circleci
ci-image:
	mkdir -p packaging/build/rootfs
	cp '$(build_output)/graphql-engine' packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/graphql-engine
	cp '/usr/lib/postgresql/$(pg_dump_ver)/bin/pg_dump' packaging/build/rootfs/pg_dump
	upx packaging/build/rootfs/graphql-engine
	docker build -t '$(registry)/graphql-engine:$(VERSION)' packaging/build/

ci-save-image:
	docker save -o '$(build_output)/image.tar' '$(registry)/graphql-engine:$(VERSION)'
ci-load-image:
	docker load -i '$(build_output)/image.tar'

push:
	docker push '$(registry)/graphql-engine:$(VERSION)'

push-latest:
	docker tag '$(registry)/graphql-engine:$(VERSION)' '$(registry)/graphql-engine:latest'
	docker push '$(registry)/graphql-engine:latest'

packager: packaging/packager.df
	docker build -t '$(registry)/graphql-engine-packager:$(packager_ver)' -f packaging/packager.df ./packaging/

.PHONY: ci-build ci-image ci-save-image ci-load-image push push-latest packager
