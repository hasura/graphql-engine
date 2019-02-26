#! /usr/bin/env bash

# exit when any command fails
set -e

# keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# echo an error message before exiting
trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT

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
: ${HASURA_PROJECT_DIR:=/hasura}
: ${SERVER_OUTPUT_DIR:=/build/_server_output}
: ${SERVER_BINARY_LOC:=/build/_server_output/graphql-engine}

LATEST_SERVER_LOG=$SERVER_OUTPUT_DIR/upgrade-test-latest-release-server.log
CURRENT_SERVER_LOG=$SERVER_OUTPUT_DIR/upgrade-test-current-server.log
API_SERVER_LOG=$SERVER_OUTPUT_DIR/api-server.log

log "setting up directories"
mkdir -p $SERVER_OUTPUT_DIR
touch $LATEST_SERVER_LOG
touch $CURRENT_SERVER_LOG

# updating hasura cli
log "updating hasura cli"
hasura update-cli

# start api server for event triggers and remote schemas
log "starting api server for triggers and remote schemas"
PORT 3000 yarn --cwd api-server start-prod > $API_SERVER_LOG 2>&1 &
API_SERVER_PID=$!

wait_for_port 3000

export API_SERVER_ENDPOINT=http://localhost:3000

# download latest graphql engine release
log "downloading latest release of graphql engine"
curl -Lo /bin/graphql-engine-latest https://graphql-engine-cdn.hasura.io/server/latest/linux-amd64
chmod +x /bin/graphql-engine-latest

# start graphql engine
log "starting latest graphql engine"
graphql-engine-latest serve > $LATEST_SERVER_LOG 2>&1 &
LAST_REL_HGE_PID=$!

wait_for_port 8080
log "graphql engine started"

# apply migrations
log "applying migrations"
hasura --project $HASURA_PROJECT_DIR migrate apply

# make a test query
log "executing the test query"
curl -d@test_query.json http://localhost:8080/v1alpha1/graphql

# kill graphql enginej
log "kill the server"
kill $LAST_REL_HGE_PID || true
wait $LAST_REL_HGE_PID || true

# start the current build
log "start the current build"
$SERVER_BINARY_LOC serve > $CURRENT_SERVER_LOG 2>&1 &
CURR_HGE_PID=$!

wait_for_port 8080
log "server started"

# make a test query
log "executing test query"
curl -d@test_query.json http://localhost:8080/v1alpha1/graphql

log "server upgrade succeeded"
