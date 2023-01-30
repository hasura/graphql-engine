export PATH := node_modules/.bin:$(PATH)
DIST_PATH ?= ./static/dist
BUCKET_NAME ?= graphql-engine-cdn.hasura.io
VERSION ?= $(shell ../scripts/get-version.sh)
NODE_OPTIONS=--max-old-space-size=4096

all: deps build

deps:
	NODE_OPTIONS=$(NODE_OPTIONS) npm ci

ci-deps:
	if [ ! -d "node_modules" ]; then NODE_OPTIONS=$(NODE_OPTIONS) npm ci; fi

build:
	NODE_OPTIONS=$(NODE_OPTIONS) npm run build

jest:
	NODE_OPTIONS=$(NODE_OPTIONS) npm run jest -- --runInBand

test:
	NODE_OPTIONS=$(NODE_OPTIONS) npm run dev & NODE_OPTIONS=$(NODE_OPTIONS) npm run test

# to be run inside circle-ci
ci-copy-assets:
	mkdir -p /build/_console_output
	cp $(DIST_PATH)/* /build/_console_output/
	echo "$(VERSION)" > /build/_console_output/version.txt

server-build: node_modules $(DIST_PATH)/common
	rm -rf "$(DIST_PATH)/versioned"
	npm run build
	mkdir -p "$(DIST_PATH)/versioned"
	cp "$(DIST_PATH)"/*.js "$(DIST_PATH)"/*.css "$(DIST_PATH)/versioned/"
	gzip -r -f "$(DIST_PATH)/versioned"

$(DIST_PATH)/common:
	mkdir -p $(DIST_PATH)
	gsutil -m cp -r gs://$(BUCKET_NAME)/console/assets/common "$(DIST_PATH)"
	touch $@

node_modules: package.json package-lock.json
	NODE_OPTIONS=$(NODE_OPTIONS) npm ci
