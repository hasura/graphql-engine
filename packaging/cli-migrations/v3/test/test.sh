#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"

if [[ -z "$TEST_IMAGE_NAME" ]]; then
  echo "please set TEST_IMAGE_NAME env to be a non-empty value and retry."
  exit 1
fi

if [[ -z "$TEST_PLATFORM" ]]; then
  echo "please set TEST_PLATFORM env to be a non-empty value and retry."
  exit 1
fi

wait_for_server() {
    echo "waiting for server"
    for _ in $(seq 1 60);
    do
      docker run --rm --network container:graphql-engine curlimages/curl http://127.0.0.1:8080/v1/version && return
      echo -n .
      sleep 1
    done
    echo "Failed waiting for server" && exit 1
}

# start postgres
docker compose up --no-start graphql-engine
# copy migrations directory to /hasura-migrations
docker cp migrations/. graphql-engine:/hasura-migrations
# copy metadata directory to /hasura-metadata
docker cp metadata/. graphql-engine:/hasura-metadata
# start graphql-engine
docker compose up -d --no-recreate graphql-engine
wait_for_server
# export metadata and run diff with validation/metadata.json
docker run --rm --network container:graphql-engine curlimages/curl -s -f -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/metadata | jq -j '.' | diff validation/metadata.json -
# get list of migrations applied from graphql-engine server
docker run --rm --network container:graphql-engine curlimages/curl -s -f -d'{"type" : "get_catalog_state", "args" : {} }' localhost:8080/v1/metadata | jq .cli_state | diff validation/catalog_cli_state.json -
# delete postgres and graphql-engine
docker compose down -v
