#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"

echo "TEST_IMAGE_NAME = $TEST_IMAGE_NAME"
echo "TEST_PLATFORM = $TEST_PLATFORM"

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
docker run --rm --network container:graphql-engine curlimages/curl -s -f   -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/query | jq -j '.' | diff validation/metadata.json -
# get list of migrations applied from graphql-engine server
docker run --rm --network container:graphql-engine curlimages/curl -s -f   -d'{"type" : "run_sql", "args" : {"sql": "select * from hdb_catalog.schema_migrations"} }' localhost:8080/v1/query | jq -j '.' | diff validation/schema_migrations.json -

# stop the graphql-engine container
docker compose stop graphql-engine

# overwrite existing metadata with intentionally inconsistent metadata 
docker cp bad_metadata/tables.yaml graphql-engine:/hasura-metadata/tables.yaml
# start the container with new, inconsistent metadata
docker compose up -d --no-recreate graphql-engine
# confirm that the service does not become available for at least 10 seconds
for _ in $(seq 1 10); do
    if docker run --rm --network container:graphql-engine curlimages/curl -s -f http://127.0.0.1:8080/v1/version > /dev/null 2>&1; then
        echo "Server became available when it should not have"
        exit 1
    fi
    sleep 1
done
# confirm that the service's log contain the expected error
if ! docker compose logs graphql-engine | grep -qi "error applying metadata"; then
    echo "Expected error message not found in logs"
    exit 1
fi

# delete postgres and graphql-engine
docker compose down -v
