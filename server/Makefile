SHELL := /bin/bash

VERSION ?= $(shell ../scripts/get-version.sh)
export VERSION

registry := hasura
packager_ver := 20190731
pg_dump_ver := 12
build_output := /build/_server_output

# Getting access to the built products with the `cabal v2-*` commands is really awkward; see
# <https://www.haskell.org/cabal/users-guide/nix-local-build.html#where-are-my-build-products> for a
# little more context. Ideally, we could use `cabal v2-install` for this, but `v2-install` does
# strange and complicated things, and I could not get it to work.
#
# This glob is a pretty heavy hammer designed to work regardless of `cabal-install` version (see the
# aforementioned link for why that’s tricky). If `cabal v2-*` ever gets a better way to do this,
# please replace this with something less hacky.
executables_glob := dist-newstyle/build/*/*/graphql-engine-*/**/opt/build/{graphql-engine/graphql-engine,graphql-engine-tests/graphql-engine-tests}

# These are the directories where `cabal v2-build --enable-coverage` sticks .mix files. It would be
# much better if cabal-install supported something like stack’s `stack hpc report`, which passes the
# right `--hpcdir` options automatically.
mix_dirs_glob := dist-newstyle/**/hpc/vanilla/mix/**/{graphql-engine-1.0.0,graphql-engine}/

# assumes this is built in circleci
ci-build:
	## configure
	cp cabal.project.ci cabal.project.local
	# Passing `--enable-coverage` to `cabal v2-build` appears to enable coverage for all packages, not
	# just local packages (at least as of cabal-install-2.4.1.0), so add `coverage: true` to the
	# `cabal.project.local` file in the appropriate package stanza.
	if [[ -n '$(enable_coverage)' ]]; then \
		printf '\npackage graphql-engine\n  coverage: true\n' >> cabal.project.local; \
	fi
	cabal v2-update
	## build
	cabal v2-build
	## install
	mkdir -p '$(build_output)'
	echo '$(VERSION)' > '$(build_output)/version.txt'
	shopt -s failglob globstar && cp $(executables_glob) '$(build_output)/'
	# Copy the .mix files needed for `hpc` to generate code coverage reports into the build output
	# directory, only if coverage is enabled (the mix files aren't generated otherwise).
	if [[ -n '$(enable_coverage)' ]]; then \
	  mkdir -p '$(build_output)/mix/' && \
	  shopt -s failglob globstar && cp -R $(mix_dirs_glob) '$(build_output)/mix/'; \
	fi

# assumes this is built in circleci
ci-image:
	mkdir -p packaging/build/rootfs
	docker create -v /root/ --name dummy alpine:3.4 /bin/true
	docker cp '$(build_output)/graphql-engine' dummy:/root/
	docker run --rm --volumes-from dummy '$(registry)/graphql-engine-packager:$(packager_ver)' /build.sh graphql-engine | tar -x -C packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/bin/graphql-engine
	cp '/usr/lib/postgresql/$(pg_dump_ver)/bin/pg_dump' packaging/build/rootfs/bin/pg_dump
	upx packaging/build/rootfs/bin/graphql-engine
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
