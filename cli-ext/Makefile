BUILDDIR   := bin
ASSETS     := $(BUILDDIR)/cli-ext-hasura-macos.tar.gz $(BUILDDIR)/cli-ext-hasura-linux.tar.gz $(BUILDDIR)/cli-ext-hasura-win.zip
CHECKSUMS  := $(patsubst %,%.sha256,$(ASSETS))

COMPRESS := gzip --best -k -c

ci-deps:
	if [ ! -d "node_modules" ]; then npm ci; fi

ci-copy-assets:
	mkdir -p /build/_cli_ext_output
	cp $(BUILDDIR)/* /build/_cli_ext_output/

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

.PHONY: deploy
deploy: $(CHECKSUMS)
	./scripts/generate-manifest.sh && \
	$(RM) $(BUILDDIR)/tmp.yaml

.PHONY: clean
clean:
	$(RM) -r $(BUILDDIR)

