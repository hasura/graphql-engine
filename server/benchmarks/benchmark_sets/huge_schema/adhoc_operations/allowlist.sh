#!/usr/bin/env bash
export iterations=4

# There are some incremental Metadata API methods that have no good
# justification for taking so much time to complete. Allowlist management is one
# of them.
function adhoc_operation() {
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/add_collection.json" \
    "$HASURA_URL/v1/query"
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/remove_collection.json" \
    "$HASURA_URL/v1/query"
}
