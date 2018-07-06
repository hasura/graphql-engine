project  := graphql-engine
registry := hasura
version  := $(shell ../scripts/get-version.sh)

stack_resolver := lts-11.15
packager_ver := 1.1
project_dir := $(shell pwd)
build_dir := $(project_dir)/$(shell stack --system-ghc path --dist-dir)/build

image: $(project).cabal
	docker build -t "$(registry)/$(project):$(version)" \
		-f packaging/Dockerfile \
		--build-arg build_flags=--fast \
		--build-arg project=$(project) \
		--build-arg stack_resolver=$(stack_resolver) \
		--build-arg packager_version=$(packager_ver) \
		.

release-image: $(project).cabal
	docker build -t "$(registry)/$(project):$(version)" \
		-f packaging/Dockerfile \
		--build-arg project=$(project) \
		--build-arg stack_resolver=$(stack_resolver) \
		--build-arg packager_version=$(packager_ver) \
		.

# assumes this is built in circleci
ci-binary: $(project).cabal
	mkdir -p packaging/build/rootfs
	stack --system-ghc build
	mkdir -p /build/_server_output
	cp $(build_dir)/$(project)/$(project) /build/_server_output/
	echo "$(VERSION)" > /build/_server_output/version.txt

# assumes this is built in circleci
ci-image:
	docker create -v /root/ --name dummy alpine:3.4 /bin/true
	docker cp $(build_dir)/$(project)/$(project) dummy:/root/
	docker run --rm --volumes-from dummy $(registry)/graphql-engine-packager:$(packager_ver) /build.sh $(project) | tar -x -C packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/bin/$(project)
	upx packaging/build/rootfs/bin/$(project)
	docker build -t $(registry)/$(project):$(version) packaging/build/


push: $(project).cabal
	docker push $(registry)/$(project):$(version)

packager: packaging/packager.df
	docker build -t "$(registry)/graphql-engine-packager:$(packager_ver)" -f packaging/packager.df ./packaging/

.PHONY: image release-image push packager
