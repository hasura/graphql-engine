SHELL := /bin/bash

VERSION ?= $(shell ../scripts/get-version.sh)
export VERSION

registry := hasura
packager_ver := 20190731
pg_dump_ver := 12
build_output := /build/_server_output

# assumes this is built in circleci
ci-build:
	# build
	cp cabal.project.ci cabal.project.local
	cabal v2-update
	cabal v2-build $(build_options)
	# install
	mkdir -p '$(build_output)'
	echo '$(VERSION)' > '$(build_output)/version.txt'
	cp "$$(cabal new-exec -- bash -c 'type -P graphql-engine')" '$(build_output)/graphql-engine'

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
