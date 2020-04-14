#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"

fail_if_port_busy() {
    local PORT=$1
    if nc -z localhost $PORT ; then
        echo "Port $PORT is busy. Exiting"
        exit 1
    fi
}

wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for _ in $(seq 1 60);
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 0.2
    done
    echo "Failed waiting for $PORT" && exit 1
}

fail_if_port_busy 8080
docker-compose up --no-start graphql-engine
docker cp migrations/. graphql-engine:/hasura-migrations
docker-compose up -d --no-recreate graphql-engine
docker run --network container:graphql-engine appropriate/curl --retry 10 --retry-delay 6 --retry-connrefused http://localhost:8080/v1/version
docker-compose down -v