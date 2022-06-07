BUILDDIR   := bin
BINARY_ASSETS := $(BUILDDIR)/cli-ext-linux-amd64 $(BUILDDIR)/cli-ext-darwin-amd64 $(BUILDDIR)/cli-ext-windows-amd64 $(BUILDDIR)/cli-ext-linux-arm64 $(BUILDDIR)/cli-ext-darwin-arm64
BINARY_ASSET_CHECKSUMS := $(patsubst %,%.sha256,$(BINARY_ASSETS))
GLOBALBUILDDIR ?= /build

COMPRESS := gzip --best -k -c

deps:
	npm install

.PHONY: ci-deps
## ci-deps: builds if not already build
ci-deps:
	if [ ! -d "node_modules" ]; then npm ci; fi

.PHONY: clean-deps
## clean-deps: deletes node_modules
clean-deps:
	if [ -d "node_modules" ]; then rm -r node_modules; fi

.PHONY: build
## build: generates build
build: ci-deps
	npm run build
	$(MAKE) $(BINARY_ASSET_CHECKSUMS)

.PHONY: clean-build
## clean-build: generates clean build
clean-build: clean clean-deps ci-deps
	npm run build

.PHONY: ci-copy-assets
ci-copy-assets:
## ci-copy-assets: copies assests to global build folder
	mkdir -p $(GLOBALBUILDDIR)/_cli_ext_output
	cp $(BUILDDIR)/* $(GLOBALBUILDDIR)/_cli_ext_output/

# Not supported: Refer https://github.com/zeit/pkg/issues/50
upx-compress: $(BUILDDIR)
	ls $(BUILDDIR)/cli-ext-hasura-* | xargs upx

.PRECIOUS: %.zip
%.zip: %.exe
	cd $(BUILDDIR) && \
	zip $(patsubst $(BUILDDIR)/%, %, $@) $(patsubst $(BUILDDIR)/%, %, $<)

.PRECIOUS: %.gz
%.gz: %
	$(COMPRESS) "$<" > "$@"

%.tar: %
	tar cf "$@" -C $(BUILDDIR) $(patsubst $(BUILDDIR)/%,%,$^)

%.sha256: %
	shasum -a 256 $< > $@

.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

.PHONY: help
## help: prints this help message
help:
	@echo "Usage: \n"
	@sed -n 's/^##//p' ${MAKEFILE_LIST} | column -t -s ':' |  sed -e 's/^/ /'

