project  := graphql-engine
registry := hasura
version  := $(shell ../scripts/get-version.sh)

# TODO: needs to be replaced with something like yq
stack_resolver := $(shell grep '^resolver:' stack.yaml | awk '{print $$2;}')
packager_ver := 1.2
project_dir := $(shell pwd)
build_dir := $(project_dir)/$(shell stack path --dist-dir)/build

build_output := /build/_server_output

dev: src-lib src-exec src-rsr
	stack build --fast
	stack exec graphql-engine -- --database-url postgres://postgres:@localhost:5432/hge serve --enable-console

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
ci-binary:
	mkdir -p packaging/build/rootfs
	stack $(STACK_FLAGS) build $(BUILD_FLAGS)
	mkdir -p $(build_output)
	cp $(build_dir)/$(project)/$(project) $(build_output)
# cp "$(build_dir)/$(project)-test/$(project)-test" $(build_output)
	echo "$(version)" > $(build_output)/version.txt

ci-test:
	$(build_output)/graphql-engine-test --database-url=$(DATABASE_URL)

# assumes this is built in circleci
ci-image:
	docker create -v /root/ --name dummy alpine:3.4 /bin/true
	docker cp $(build_dir)/$(project)/$(project) dummy:/root/
	docker run --rm --volumes-from dummy $(registry)/graphql-engine-packager:$(packager_ver) /build.sh $(project) | tar -x -C packaging/build/rootfs
	strip --strip-unneeded packaging/build/rootfs/bin/$(project)
	upx packaging/build/rootfs/bin/$(project)
	docker build -t $(registry)/$(project):$(version) packaging/build/

ci-save-image:
	docker save -o $(build_output)/image.tar $(registry)/$(project):$(version)

ci-load-image:
	docker load -i $(build_output)/image.tar

push:
	docker push $(registry)/$(project):$(version)

push-latest:
	docker tag $(registry)/$(project):$(version) $(registry)/$(project):latest
	docker push $(registry)/$(project):latest

packager: packaging/packager.df
	docker build -t "$(registry)/graphql-engine-packager:$(packager_ver)" -f packaging/packager.df ./packaging/

.PHONY: image release-image push packager ci-binary-and-test ci-image ci-save-image ci-load-image
