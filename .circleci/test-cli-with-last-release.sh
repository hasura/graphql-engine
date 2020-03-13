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

# get latest cli
wget -O /bin/graphql-engine https://graphql-engine-cdn.hasura.io/server/latest/linux-amd64
chmod +x /bin/graphql-engine

cd "$CLI_ROOT"
mkdir -p /build/_cli_output
touch /build/_cli_output/server-last-release.log
touch /build/_cli_output/server-last-release-secret.log

# start graphql-engine without admin secret
/bin/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /build/_cli_output/server-last-release.log 2>&1 &
PID=$!

wait_for_port 8080

# test cli
HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" TEST_TAGS="latest_release" make test

# kill the running server
kill -s INT $PID

# start graphql-engine with admin secret
psql -U gql_test -h localhost -c 'CREATE DATABASE "gql_test_with_admin_secret";'
/bin/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test_with_admin_secret serve --access-key "abcd" > /build/_cli_output/server-last-release-secret.log 2>&1 &
PID=$!

wait_for_port 8080

# test cli
HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" HASURA_GRAPHQL_TEST_ADMIN_SECRET="abcd" TEST_TAGS="latest_release" make test
kill -s INT $PID
