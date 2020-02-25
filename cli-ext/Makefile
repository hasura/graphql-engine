BUILDDIR   := bin
ASSETS     := $(BUILDDIR)/command-macos.tar.gz $(BUILDDIR)/command-linux.tar.gz $(BUILDDIR)/command-win.zip
CHECKSUMS  := $(patsubst %,%.sha256,$(ASSETS))

COMPRESS := gzip --best -k -c

ci-deps:
	if [ ! -d "node_modules" ]; then npm ci; fi

ci-copy-assets:
	mkdir -p /build/_cli_ext_output
	cp $(BUILDDIR)/* /build/_cli_ext_output/

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

