#!/bin/sh

set -e

DEFAULT_MIGRATIONS_DIR="/hasura-migrations"
TEMP_MIGRATIONS_DIR="/tmp/hasura-migrations"

# wait for a port to be ready
wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for i in `seq 1 30`;
    do
        nc localhost $PORT > /dev/null 2>&1 && echo "port $PORT is ready" && return
        echo -n .
        sleep 1
    done
    echo "failed waiting for $PORT" && exit 1
}

echo "starting graphql engine on port 8080..."

# start graphql engine
graphql-engine serve &
# store the pid to kill it later
PID=$!

# wait for port 8080 to be ready
wait_for_port 8080

# check if migration directory is set, default otherwise
if [ -z ${HASURA_GRAPHQL_MIGRATIONS_DIR+x} ]; then
    echo "env var HASURA_GRAPHQL_MIGRATIONS_DIR is not set"
    echo "defaulting to $DEFAULT_MIGRATIONS_DIR"
    HASURA_GRAPHQL_MIGRATIONS_DIR="$DEFAULT_MIGRATIONS_DIR"
fi

# apply migrations if the directory exist
if [ -d "$HASURA_GRAPHQL_MIGRATIONS_DIR" ]; then
    echo "applying migrations in $HASURA_GRAPHQL_MIGRATIONS_DIR..."
    mkdir -p "$TEMP_MIGRATIONS_DIR"
    cp -a "$HASURA_GRAPHQL_MIGRATIONS_DIR/." "$TEMP_MIGRATIONS_DIR/migrations/"
    cd "$TEMP_MIGRATIONS_DIR"
    echo "endpoint: http://localhost:8080" > config.yaml
    hasura-cli migrate apply
else
    echo "directory $HASURA_GRAPHQL_MIGRATIONS_DIR does not exist"
    echo "skipping migration apply"
fi

# kill graphql engine that we started earlier
kill $PID

# pass control to CMD
exec "$@"
