#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CLI_ROOT="${BASH_SOURCE[0]%/*}/../cli"

wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for i in `seq 1 60`;
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 1
    done
    echo "Failed waiting for $PORT" && exit 1
}

cd "$CLI_ROOT"
mkdir -p /build/_cli_output
touch /build/_cli_output/server.log

# start graphql-engine without access key
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /build/_cli_output/server.log 2>&1 &
PID=$!

wait_for_port 8080

# test cli
HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" make test
kill $PID

# start graphql-engine with access key
psql -U gql_test -h localhost -c 'CREATE DATABASE "gql_test_with_access";'
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test_with_access serve --access-key "abcd" > /build/_cli_output/server.log 2>&1 &
PID=$!

wait_for_port 8080

# test cli
GOCACHE=off HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" HASURA_GRAPHQL_TEST_ACCESS_KEY="abcd" make test
kill $PID
