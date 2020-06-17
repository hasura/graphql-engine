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
touch /build/_cli_output/server-secret.log

# start graphql-engine without admin secret
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /build/_cli_output/server.log 2>&1 &
PID=$!

wait_for_port 8080

# test cli
HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" make test
kill -s INT $PID

# start graphql-engine with admin secret
psql -U gql_test -h localhost -c 'CREATE DATABASE "gql_test_with_admin_secret";'
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test_with_admin_secret serve --admin-secret "abcd" > /build/_cli_output/server-secret.log 2>&1 &
PID=$!

wait_for_port 8080

# test cli
HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" HASURA_GRAPHQL_TEST_CLI_EXT_MANIFEST_FILE_PATH="/build/_cli_ext_output/manifest-dev.yaml" HASURA_GRAPHQL_TEST_ADMIN_SECRET="abcd" make test
kill -s INT $PID
