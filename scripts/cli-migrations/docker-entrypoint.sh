#!/bin/sh

set -e

log() {
    TIMESTAMP=$(date -u "+%Y-%m-%dT%H:%M:%S.000+0000")
    MESSAGE=$1
    echo "{\"timestamp\":\"$TIMESTAMP\",\"level\":\"info\",\"type\":\"startup\",\"detail\":{\"kind\":\"migration-apply\",\"info\":\"$MESSAGE\"}}"
}

DEFAULT_MIGRATIONS_DIR="/hasura-migrations"
TEMP_MIGRATIONS_DIR="/tmp/hasura-migrations"

# check server port and ser default as 8080
if [ -z ${HASURA_GRAPHQL_SERVER_PORT+x} ]; then
    log "port env var is not set, defaulting to 8080"
    HASURA_GRAPHQL_SERVER_PORT=8080
fi
if [ -z ${HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT+x} ]; then
    log "server timeout is not set defaulting to 30 seconds"
    HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT=30
fi

# wait for a port to be ready
wait_for_port() {
    local PORT=$1
    log "waiting $HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT for $PORT to be ready"
    for i in `seq 1 $HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT`;
    do
        nc -z localhost $PORT > /dev/null 2>&1 && log "port $PORT is ready" && return
        sleep 1
    done
    log "failed waiting for $PORT" && exit 1
}

log "starting graphql engine temporarily on port $HASURA_GRAPHQL_SERVER_PORT"

# start graphql engine
graphql-engine serve &
# store the pid to kill it later
PID=$!

# wait for port to be ready
wait_for_port $HASURA_GRAPHQL_SERVER_PORT

# check if migration directory is set, default otherwise
log "checking for migrations directory"
if [ -z ${HASURA_GRAPHQL_MIGRATIONS_DIR+x} ]; then
    log "env var HASURA_GRAPHQL_MIGRATIONS_DIR is not set, defaulting to $DEFAULT_MIGRATIONS_DIR"
    HASURA_GRAPHQL_MIGRATIONS_DIR="$DEFAULT_MIGRATIONS_DIR"
fi

# apply migrations if the directory exist
if [ -d "$HASURA_GRAPHQL_MIGRATIONS_DIR" ]; then
    log "applying migrations from $HASURA_GRAPHQL_MIGRATIONS_DIR"
    mkdir -p "$TEMP_MIGRATIONS_DIR"
    cp -a "$HASURA_GRAPHQL_MIGRATIONS_DIR/." "$TEMP_MIGRATIONS_DIR/migrations/"
    cd "$TEMP_MIGRATIONS_DIR"
    echo "endpoint: http://localhost:$HASURA_GRAPHQL_SERVER_PORT" > config.yaml
    hasura-cli migrate apply
    # check if metadata.[yaml|json] exist and apply
    if [ -f migrations/metadata.yaml ]; then
        log "applying metadata from $HASURA_GRAPHQL_MIGRATIONS_DIR/metadata.yaml"
        hasura-cli metadata apply
    elif [ -f migrations/metadata.json ]; then
        log "applying metadata from $HASURA_GRAPHQL_MIGRATIONS_DIR/metadata.json"
        hasura-cli metadata apply
    fi
else
    log "directory $HASURA_GRAPHQL_MIGRATIONS_DIR does not exist, skipping migrations"
fi

# kill graphql engine that we started earlier
log "killing temporary server"
kill $PID

# pass control to CMD
log "graphql-engine will now start in normal mode"
exec "$@"
