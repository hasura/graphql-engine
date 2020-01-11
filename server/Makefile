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
package_directory_glob := dist-newstyle/build/*/*/graphql-engine-*
executables_glob := $(package_directory_glob)/**/opt/build/{graphql-engine/graphql-engine,graphql-engine-tests/graphql-engine-tests}

# assumes this is built in circleci
ci-build:
	## clean
	# Since we have to rely on `executables_glob` to get the executables (see above), delete them all
	# before building. This ensures that if `cabal-install` changes in the future, we don’t just copy
	# the stale, cached build products, since that would be a nightmare to debug.
	shopt -s nullglob globstar && for file in $(executables_glob); do rm "$$file"; done
	# Even though we did the above step to delete the executables, `cabal v2-build` won’t relink them
	# if none of the sources changed. We can get around this by deleting the `cache` directories.
	# (This has the convenient side-effect of ensuring files marked with `-fforce-recomp` get
	# recompiled, which is important so that the right git SHA gets used as the executable version.)
	shopt -s nullglob globstar && \
		for directory in $(package_directory_glob)/**/opt/cache/; do rm -rf "$$directory"; done
	## build
	cp cabal.project.ci cabal.project.local
	cabal v2-update
	cabal v2-build $(build_options)
	## install
	mkdir -p '$(build_output)'
	echo '$(VERSION)' > '$(build_output)/version.txt'
	shopt -s failglob globstar && cp $(executables_glob) '$(build_output)/'

# assumes this is built in circleci
ci-image:
	mkdir -p packaging/build/rootfs
	docker create -v /root/ --name dummy alpine:3.4 /bin/true
	docker cp '$(build_output)/graphql-engine' dummy:/root/
	docker run --rm --volumes-from dummy '$(registry)/graphql-engine-packager:$(packager_ver)' /build.sh graphql-engine | tar -x -C packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/bin/graphql-engine
	cp '/usr/lib/postgresql/$(pg_dump_ver)/bin/pg_dump packaging/build/rootfs/bin/pg_dump'
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
