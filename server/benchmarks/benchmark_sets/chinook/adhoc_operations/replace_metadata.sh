#!/usr/bin/env bash
export iterations=30

function adhoc_operation() {
  # This is the same replace_metadata command we run during setup (i.e. a noop)
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/../replace_metadata.json" \
    "$HASURA_URL/v1/query"
}
