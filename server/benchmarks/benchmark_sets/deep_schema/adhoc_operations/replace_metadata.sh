#!/usr/bin/env bash
export iterations=200

function adhoc_operation() {
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  # Clears all metadata
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/../clear_metadata.json" \
    "$HASURA_URL/v1/query"
  # Re-create the schema
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/../replace_metadata.json" \
    "$HASURA_URL/v1/query"
}
