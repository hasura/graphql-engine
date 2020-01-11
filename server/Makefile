SHELL := /bin/bash

project  := graphql-engine
registry := hasura
VERSION  ?= $(shell ../scripts/get-version.sh)
export VERSION

packager_ver := 20190731
pg_dump_ver := 12
project_dir := $(shell pwd)
# build_dir := $(project_dir)/$(shell stack path --dist-dir)/build

build_output := /build/_server_output

# assumes this is built in circleci
ci-build:
	# build
	cp cabal.project.ci cabal.project.local
	cabal v2-update
	cabal v2-build
	# install
	mkdir -p '$(build_output)'
	echo '$(VERSION)' > '$(build_output)/version.txt'
	cp "$$(cabal new-exec -- bash -c 'type -P graphql-engine')" '$(build_output)/graphql-engine'

# assumes this is built in circleci
ci-image:
	mkdir -p packaging/build/rootfs
	docker create -v /root/ --name dummy alpine:3.4 /bin/true
	docker cp $(build_dir)/$(project)/$(project) dummy:/root/
	docker run --rm --volumes-from dummy $(registry)/graphql-engine-packager:$(packager_ver) /build.sh $(project) | tar -x -C packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/bin/$(project)
	cp /usr/lib/postgresql/$(pg_dump_ver)/bin/pg_dump packaging/build/rootfs/bin/pg_dump
	upx packaging/build/rootfs/bin/$(project)
	docker build -t $(registry)/$(project):$(VERSION) packaging/build/

ci-save-image:
	docker save -o $(build_output)/image.tar $(registry)/$(project):$(VERSION)

ci-load-image:
	docker load -i $(build_output)/image.tar

push:
	docker push $(registry)/$(project):$(VERSION)

push-latest:
	docker tag $(registry)/$(project):$(VERSION) $(registry)/$(project):latest
	docker push $(registry)/$(project):latest

packager: packaging/packager.df
	docker build -t "$(registry)/graphql-engine-packager:$(packager_ver)" -f packaging/packager.df ./packaging/

.PHONY: ci-build ci-image ci-save-image ci-load-image push push-latest packager
