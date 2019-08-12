project  := graphql-engine
registry := hasura
VERSION  ?= $(shell ../scripts/get-version.sh)
export VERSION
nproc    := $(shell nproc)

# TODO: needs to be replaced with something like yq
stack_resolver := $(shell awk '/^resolver:/ {print $$2;}' stack.yaml)
packager_ver := 20190731
pg_dump_ver := 11
project_dir := $(shell pwd)
build_dir := $(project_dir)/$(shell stack path --dist-dir)/build

build_output := /build/_server_output

build:
	stack build --fast --ghc-options '-j$(nproc)'

exec: build
	stack exec graphql-engine -- serve

exec-local-console: build-local-console
	stack exec graphql-engine -- serve --enable-console --console-assets-dir ../console/static/dist

image: $(project).cabal
	docker build -t "$(registry)/$(project):$(VERSION)" \
		-f packaging/Dockerfile \
		--build-arg build_flags=--fast \
		--build-arg project=$(project) \
		--build-arg stack_resolver=$(stack_resolver) \
		--build-arg packager_version=$(packager_ver) \
		.

local-image:
	$(eval build_dir_docker := $(project_dir)/$(shell stack --docker path --dist-dir)/build)
	stack docker pull
	stack --docker build --fast
	mkdir -p packaging/build/rootfs
	docker run --rm -v "$(build_dir_docker)/$(project)/$(project):/root/$(project)" \
		$(registry)/graphql-engine-packager:$(packager_ver) /build.sh \
		$(project) | tar -x -C packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/bin/$(project)
	upx packaging/build/rootfs/bin/$(project)
	docker build -t "$(registry)/$(project):$(VERSION)" packaging/build

release-image: $(project).cabal
	docker build -t "$(registry)/$(project):$(VERSION)" \
		-f packaging/Dockerfile \
		--build-arg project=$(project) \
		--build-arg stack_resolver=$(stack_resolver) \
		--build-arg packager_version=$(packager_ver) \
		.

# assumes this is built in circleci
ci-binary:
	mkdir -p packaging/build/rootfs
	stack $(STACK_FLAGS) build $(BUILD_FLAGS)
	mkdir -p $(build_output)
	cp $(build_dir)/$(project)/$(project) $(build_output)
# cp "$(build_dir)/$(project)-test/$(project)-test" $(build_output)
	echo "$(VERSION)" > $(build_output)/version.txt

# assumes this is built in circleci
ci-image:
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

.PHONY: image local-image release-image push packager ci-binary-and-test ci-image ci-save-image ci-load-image build exec build-local-console exec-local-console
