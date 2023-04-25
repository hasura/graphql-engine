CONSOLE_CE_ASSETS_PATH = frontend/dist/apps/server-assets-console-ce
CONSOLE_EE_ASSETS_PATH = frontend/dist/apps/server-assets-console-ee

.PHONY: build-console-ce-assets
## build-console-ce-assets: Build Console assets required for the CE server
build-console-ce-assets: $(CONSOLE_CE_ASSETS_PATH)

.PHONY: build-console-ee-assets
## build-console-ee-assets: Build Console assets required for the EE server
build-console-ee-assets: $(CONSOLE_EE_ASSETS_PATH)

# We split the named, phony targets from the path-based targets below so that
# `make` has a chance of avoiding rework, by recognizing that the output path
# is newer than the inputs.
#
# We use `touch` to ensure that the path is marked as newer, because modified
# timestamps on directories can be quite unreliable.
#
# `$@` refers to the target.

$(CONSOLE_CE_ASSETS_PATH): frontend/node_modules
	cd frontend && npm run server-build:ce
	touch $@

$(CONSOLE_EE_ASSETS_PATH): frontend/node_modules
	cd frontend && npm run server-build:ee
	touch $@

# Install node_modules if package-lock.json changes
frontend/node_modules: frontend/package-lock.json
	cd frontend && npm install
	touch $@

# Cleanly install node_modules if package.json changes
frontend/package-lock.json: frontend/package.json
	cd frontend && npm ci
	touch $@
