#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"

wait_for_server() {
    echo "waiting for server"
    for _ in $(seq 1 60);
    do
      docker run --network container:graphql-engine appropriate/curl http://127.0.0.1:8080/v1/version && return
      echo -n .
      sleep 1
    done
    echo "Failed waiting for server" && exit 1
}


# get previous stable version docker-compose file
curl -L https://raw.githubusercontent.com/hasura/graphql-engine/stable/install-manifests/docker-compose/docker-compose.yaml -o docker-compose-latest.yaml
sed -i '/hasura\/graphql-engine:/ s/$/.cli-migrations-v2\n    container_name: graphql-engine/' docker-compose-latest.yaml
# start postgres
docker-compose -f docker-compose-latest.yaml up --no-start graphql-engine
# copy migrations directory to /hasura-migrations
docker cp migrations/. graphql-engine:/hasura-migrations
# copy metadata directory to /hasura-metadata
docker cp metadata/. graphql-engine:/hasura-metadata
# start graphql-engine
docker-compose -f docker-compose-latest.yaml up -d --no-recreate graphql-engine
wait_for_server
# export metadata and run diff with validation/metadata.json
docker run --network container:graphql-engine appropriate/curl -s -f   -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/query | jq -j '.' | diff validation/metadata.json -
# get list of migrations applied from graphql-engine server
docker run --network container:graphql-engine appropriate/curl -s -f   -d'{"type" : "run_sql", "args" : {"sql": "select * from hdb_catalog.schema_migrations"} }' localhost:8080/v1/query | jq -j '.' | diff validation/schema_migrations.json -

# use the current build to start container
docker-compose up -d
wait_for_server
# export metadata and run diff with validation/metadata.json
docker run --network container:graphql-engine appropriate/curl -s -f   -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/query | jq -j '.' | diff validation/metadata.json -
# get list of migrations applied from graphql-engine server
docker run --network container:graphql-engine appropriate/curl -s -f   -d'{"type" : "run_sql", "args" : {"sql": "select * from hdb_catalog.schema_migrations"} }' localhost:8080/v1/query | jq -j '.' | diff validation/schema_migrations.json -
# delete postgres and graphql-engine
docker-compose down -v
