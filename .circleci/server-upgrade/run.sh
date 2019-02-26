#! /usr/bin/env bash

set -euo pipefail

# # keep track of the last executed command
# trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# # echo an error message before exiting
# trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT

ROOT="${BASH_SOURCE[0]%/*}"

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

log() { echo -e "--> $*"; }

# env HASURA_GRAPHQL_DATABASE_URL
: ${HASURA_GRAPHQL_SERVER_PORT:=8080}
: ${API_SERVER_PORT:=3000}
: ${HASURA_PROJECT_DIR:=$ROOT/hasura}
: ${API_SERVER_DIR:=$ROOT/api-server}
: ${SERVER_OUTPUT_DIR:=/build/_server_output}
: ${SERVER_BINARY:=/build/_server_output/graphql-engine}
: ${LATEST_SERVER_BINARY:=/bin/graphql-engine-latest}

LATEST_SERVER_LOG=$SERVER_OUTPUT_DIR/upgrade-test-latest-release-server.log
CURRENT_SERVER_LOG=$SERVER_OUTPUT_DIR/upgrade-test-current-server.log
API_SERVER_LOG=$SERVER_OUTPUT_DIR/api-server.log

API_SERVER_ENDPOINT=http://localhost:$API_SERVER_PORT
HGE_ENDPOINT=http://localhost:$HASURA_GRAPHQL_SERVER_PORT

log "setting up directories"
mkdir -p $SERVER_OUTPUT_DIR
touch $LATEST_SERVER_LOG
touch $CURRENT_SERVER_LOG

# installing deps
log "installing deps"
yarn --cwd $ROOT install

# updating hasura cli
log "updating hasura cli"
hasura update-cli

# start api server for event triggers and remote schemas
log "starting api server for triggers and remote schemas"
yarn --cwd $API_SERVER_DIR install
PORT=$API_SERVER_PORT yarn --cwd $API_SERVER_DIR start-prod > $API_SERVER_LOG 2>&1 &
API_SERVER_PID=$!

wait_for_port $API_SERVER_PORT

# download latest graphql engine release
log "downloading latest release of graphql engine"
curl -Lo $LATEST_SERVER_BINARY https://graphql-engine-cdn.hasura.io/server/latest/linux-amd64
chmod +x $LATEST_SERVER_BINARY

# start graphql engine
log "starting latest graphql engine"
$LATEST_SERVER_BINARY serve > $LATEST_SERVER_LOG 2>&1 &
LAST_REL_HGE_PID=$!

wait_for_port $HASURA_GRAPHQL_SERVER_PORT
log "graphql engine started"

# apply migrations
log "applying migrations"
hasura --project $HASURA_PROJECT_DIR migrate apply --endpoint $HGE_ENDPOINT

# make a test query
log "executing the test query"
node $ROOT/make_test_query.js $HGE_ENDPOINT/v1alpha1/graphql
log

# kill graphql engine
log "kill the server"
kill $LAST_REL_HGE_PID || true
wait $LAST_REL_HGE_PID || true

# start the current build
log "start the current build"
$SERVER_BINARY serve > $CURRENT_SERVER_LOG 2>&1 &
CURR_HGE_PID=$!

wait_for_port $HASURA_GRAPHQL_SERVER_PORT
log "server started"

# make a test query
log "executing test query"
node $ROOT/make_test_query.js $HGE_ENDPOINT/v1alpha1/graphql
log

log "kill the server"
kill $CURR_HGE_PID || true
wait $CURR_HGE_PID || true

log "kill the api server"
kill $API_SERVER_PID || true
wait $API_SERVER_PID || true

log "server upgrade succeeded"

exit 0
