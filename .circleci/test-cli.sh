#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CLI_ROOT="${BASH_SOURCE[0]%/*}/../cli"

cd "$CLI_ROOT"
mkdir -p /build/_cli_output
touch /build/_cli_output/server.log

# start graphql-engine
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /build/_cli_output/server.log 2>&1 &
#PID=$!

# test cli
HASURA_GRAPHQL_TEST_ENDPOINT="http://localhost:8080" make test
#kill $PID