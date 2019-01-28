#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

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

cd "$CONSOLE_ROOT"

mkdir -p /build/_console_output
touch /build/_console_output/server.log
touch /build/_console_output/cli.log

# start graphql-engine
/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /build/_console_output/server.log 2>&1 &

wait_for_port 8080

# start cli
/build/_cli_output/binaries/cli-hasura-linux-amd64 init --directory gql-test && cd gql-test
/build/_cli_output/binaries/cli-hasura-linux-amd64 console --no-browser > /build/_console_output/cli.log 2>&1 &

cd ..

wait_for_port 9693

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
# wait for console to build
while [ ! -f ./webpack-assets.json ]
do
  sleep 2
done
# run console tests
npm run test
