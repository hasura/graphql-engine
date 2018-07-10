#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

cd "$CONSOLE_ROOT"

mkdir -p /build/_console_output
touch /build/_console_output/server.log
touch /build/_console_output/cli.log

# start graphql-engine
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /build/_console_output/server.log 2>&1 &

# start cli
/build/_cli_output/hasura-linux-amd64 init --directory gql-test && cd gql-test
/build/_cli_output/hasura-linux-amd64 console --no-browser > /build/_console_output/cli.log 2>&1 &

cd ..

export PORT=3000
export NODE_ENV=development
export DATA_API_URL=http://localhost:8080
export API_HOST=http://localhost
export API_PORT=9693
export CONSOLE_MODE=cli
export DEV_DATA_API_URL=http://localhost:8080
export URL_PREFIX=/

# test console
npm run dev &
npm run test