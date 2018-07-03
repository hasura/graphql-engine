export PATH := node_modules/.bin:$(PATH)
DIST_PATH = ./static/dist
BUCKET_NAME ?= hasura-graphql-engine
VERSION ?= $(shell ../scripts/get-version.sh)

all: deps build gcloud-cp-stable gcloud-set-metadata

deps:
	npm install

build:
	npm run build

test:
	npm run test

gcloud-cp-stable:
	gsutil cp -r $(DIST_PATH)/main.css gs://$(BUCKET_NAME)/console/$(VERSION)/main.css
	gsutil cp -r $(DIST_PATH)/main.js gs://$(BUCKET_NAME)/console/$(VERSION)/main.js
	gsutil cp -r $(DIST_PATH)/vendor.js gs://$(BUCKET_NAME)/console/$(VERSION)/vendor.js

gcloud-set-metadata:
	gsutil setmeta -h "Content-Type: application/javascript" gs://$(BUCKET_NAME)/console/$(VERSION)/*.js
	gsutil setmeta -h "Content-Type: text/css" gs://$(BUCKET_NAME)/console/$(VERSION)/*.css
	#gsutil setmeta -h "Content-Encoding: gzip" gs:// + $(BUCKET_NAME) + '/console/' + $(VERSION) + '/*'
