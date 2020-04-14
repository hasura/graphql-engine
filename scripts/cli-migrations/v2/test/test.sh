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

docker-compose up --no-start graphql-engine
docker cp migrations/. graphql-engine:/hasura-migrations
docker-compose up -d --no-recreate graphql-engine
wait_for_server
docker-compose down -v