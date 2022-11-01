#!/usr/bin/env bash
export iterations=4

function adhoc_operation() {
  # This is the same replace_metadata command we run during setup (i.e. a noop)
  scriptDir=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/untrack_table.json" \
    "$HASURA_URL/v1/query"
  curl \
    --request POST \
    --header 'Content-Type: application/json' \
    --data "@$scriptDir/track_table.json" \
    "$HASURA_URL/v1/query"
}
