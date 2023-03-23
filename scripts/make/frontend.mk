.PHONY: build-console-assets
## build-console-assets
build-console-assets: frontend/node_modules
	cd frontend && npm run server-build:ce

# Install node_modules if package-lock.json changes
frontend/node_modules: frontend/package-lock.json
	cd frontend && npm install && touch node_modules

# Cleanly install node_modules if package.json changes
frontend/package-lock.json: frontend/package.json
	cd frontend && npm ci
