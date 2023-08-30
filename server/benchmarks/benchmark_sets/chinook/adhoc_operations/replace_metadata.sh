#!/usr/bin/env bash
export iterations=30
export pause_after_seconds=10

function adhoc_operation() {
  # This is the same replace_metadata command we run during setup (i.e. a noop)
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --header 'x-hasura-admin-secret: my-secret' \
    --data "@$scriptDir/../replace_metadata.json" \
    "$HASURA_URL/v1/query"
}
