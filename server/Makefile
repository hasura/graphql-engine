# Check that given variables are set and all have non-empty values,
# die with an error otherwise.
#
# Params:
#   1. Variable name(s) to test.
#   2. (optional) Error message to print.
check_defined = \
		$(strip $(foreach 1,$1, \
				$(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
		$(if $(value $1),, \
			$(error $1$(if $2, ($2)) is not set))

project  := graphql-engine
registry := hasura
packager_ver := 1.0

project_dir := $(shell pwd)
build_dir   := $(project_dir)/$(shell stack --docker path --dist-dir)/build

build-dir: $(project).cabal
	@:$(call check_defined, IMAGE_VERSION IMAGE_REPO STACK_FLAGS)
	stack $(STACK_FLAGS) build $(BUILD_FLAGS)
	mkdir -p packaging/build/rootfs
	docker run --rm -v $(build_dir)/$(project)/$(project):/root/$(project) $(registry)/graphql-engine-packager:$(packager_ver) /build.sh $(project) | tar -x -C build/rootfs

image: $(project).cabal
	@:$(call check_defined, IMAGE_VERSION IMAGE_REPO STACK_FLAGS)
	make build-dir STACK_FLAGS=$(STACK_FLAGS) IMAGE_REPO=$(IMAGE_REPO) IMAGE_VERSION=$(IMAGE_VERSION)
	ln -f build/rootfs/bin/$(project) build/rootfs/bin/graphql-engine
	docker build -t $(IMAGE_REPO)/$(project):$(IMAGE_VERSION) packaging/build/

release-image: $(project).cabal
	@:$(call check_defined, IMAGE_VERSION IMAGE_REPO STACK_FLAGS)
	rm -rf .stack-work
	make build-dir STACK_FLAGS=$(STACK_FLAGS) IMAGE_REPO=$(IMAGE_REPO) IMAGE_VERSION=$(IMAGE_VERSION)
	strip --strip-unneeded build/rootfs/bin/$(project)
	upx packaging/build/rootfs/bin/$(project)
	ln -f packaging/build/rootfs/bin/$(project) build/rootfs/bin/graphql-engine
	docker packaging/build -t $(IMAGE_REPO)/$(project):$(IMAGE_VERSION) packaging/build/

push: $(project).cabal
	@:$(call check_defined, IMAGE_VERSION IMAGE_REPO)
	docker push $(IMAGE_REPO)/$(project):$(IMAGE_VERSION)

packager: packaging/packager.df
	docker build -t "$(registry)/graphql-engine-packager:$(packager_ver)" -f packaging/packager.df ./packaging/
