#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

wait_for_port() {
    local ADDRESS=$1
    local PORT=$2
    echo "waiting for $PORT on $ADDRESS"
    for i in `seq 1 60`;
    do
      nc -z $ADDRESS $PORT && echo "port $PORT on $ADDRESS is ready" && return
      echo -n .
      sleep 1
    done
    echo "Failed waiting for $PORT" && exit 1
}

cd "$CONSOLE_ROOT"

mkdir -p /root/build/_console_output
touch /root/build/_console_output/server.log
touch /root/build/_console_output/cli.log

# start graphql-engine
/root/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@${POSTGRES_HOST}:5432/gql_test serve > /root/build/_console_output/server.log 2>&1 &

wait_for_port localhost 8080

# start cli
/root/build/_cli_output/binaries/cli-hasura-linux-amd64 init --directory gql-test && cd gql-test
/root/build/_cli_output/binaries/cli-hasura-linux-amd64 console --no-browser --address $(awk 'END{print $1}' /etc/hosts) > /root/build/_console_output/cli.log 2>&1 &

wait_for_port $(awk 'END{print $1}' /etc/hosts) 9693

cd ..

export PORT=3000
export NODE_ENV=development
export DATA_API_URL=http://$(awk 'END{print $1}' /etc/hosts):8080
export API_HOST=http://$(awk 'END{print $1}' /etc/hosts)
export API_PORT=9693
export CONSOLE_MODE=cli
export DEV_DATA_API_URL=http://$(awk 'END{print $1}' /etc/hosts):8080
export URL_PREFIX=/
export HOST=$(awk 'END{print $1}' /etc/hosts)

make ci-deps

node_modules/.bin/cypress install

# test console
npm run dev
