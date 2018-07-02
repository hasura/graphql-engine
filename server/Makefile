project  := graphql-engine
registry := hasura
version  := $(shell ../scripts/get-version.sh)

stack_resolver := lts-11.15
packager_ver   := 1.1

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

push: $(project).cabal
	docker push $(registry)/$(project):$(version)

packager: packaging/packager.df
	docker build -t "$(registry)/graphql-engine-packager:$(packager_ver)" -f packaging/packager.df ./packaging/

.PHONY: image release-image push packager
