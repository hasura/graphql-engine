#!/usr/bin/env bash
export iterations=4
export pause_after_seconds=5

function adhoc_operation() {
  # This is the same replace_metadata command we run during setup (i.e. a noop)
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --header 'x-hasura-admin-secret: my-secret' \
    --data "@$scriptDir/untrack_table.json" \
    "$HASURA_URL/v1/query"
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --header 'x-hasura-admin-secret: my-secret' \
    --data "@$scriptDir/track_table.json" \
    "$HASURA_URL/v1/query"
}
