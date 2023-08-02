METADATA_OPENAPI_JSON := metadata.openapi.json

.PHONY: generate-metadata-openapi
## generate-schema: Generate an OpenAPI schema file in JSON format based on server code
generate-metadata-openapi:
	# --verbose=0 avoids an "Up to date" line at the top of the file
	cabal run --verbose=0 emit-metadata-openapi > "${METADATA_OPENAPI_JSON}"
