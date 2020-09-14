#!/bin/sh

set -e

log() {
    TIMESTAMP=$(date -u "+%Y-%m-%dT%H:%M:%S.000+0000")
    LOGKIND=$1
    MESSAGE=$2
    echo "{\"timestamp\":\"$TIMESTAMP\",\"level\":\"info\",\"type\":\"startup\",\"detail\":{\"kind\":\"$LOGKIND\",\"info\":\"$MESSAGE\"}}"
}

DEFAULT_MIGRATIONS_DIR="/hasura-migrations"
TEMP_MIGRATIONS_DIR="/tmp/hasura-migrations"

# configure the target database for migrations
if [ ${HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR} ]; then
    log "migrations-startup" "database url for migrations is set by $HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR"
    HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL=$(printenv $HASURA_GRAPHQL_MIGRATIONS_DATABASE_ENV_VAR)
elif [ -z ${HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL+x} ]; then
    HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL=$HASURA_GRAPHQL_DATABASE_URL
fi
log "migrations-startup" "database url for migrations is set by HASURA_GRAPHQL_DATABASE_URL"

# Use 9691 port for running temporary instance. 
# In case 9691 is occupied (according to docker networking), then this will fail.
# override with another port in that case
# TODO: Find a proper random port
if [ -z ${HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT+x} ]; then
    log "migrations-startup" "migrations server port env var is not set, defaulting to 9691"
    HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT=9691
fi

if [ -z ${HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT+x} ]; then
    log "migrations-startup" "server timeout is not set, defaulting to 30 seconds"
    HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT=30
fi

# wait for a port to be ready
wait_for_port() {
    local PORT=$1
    log "migrations-startup" "waiting $HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT for $PORT to be ready"
    for i in `seq 1 $HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT`;
    do
        nc -z localhost $PORT > /dev/null 2>&1 && log "migrations-startup" "port $PORT is ready" && return
        sleep 1
    done
    log "migrations-startup" "failed waiting for $PORT, try increasing HASURA_GRAPHQL_MIGRATIONS_SERVER_TIMEOUT (default: 30)" && exit 1
}

log "migrations-startup" "starting graphql engine temporarily on port $HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT"

# start graphql engine with metadata api enabled
graphql-engine --database-url "$HASURA_GRAPHQL_MIGRATIONS_DATABASE_URL" \
               serve --enabled-apis="metadata" \
               --server-port=${HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT}  &
# store the pid to kill it later
PID=$!

# wait for port to be ready
wait_for_port $HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT

# check if migration directory is set, default otherwise
log "checking for migrations directory"
if [ -z ${HASURA_GRAPHQL_MIGRATIONS_DIR+x} ]; then
    log "migrations-startup" "env var HASURA_GRAPHQL_MIGRATIONS_DIR is not set, defaulting to $DEFAULT_MIGRATIONS_DIR"
    HASURA_GRAPHQL_MIGRATIONS_DIR="$DEFAULT_MIGRATIONS_DIR"
fi

# apply migrations if the directory exist
if [ -d "$HASURA_GRAPHQL_MIGRATIONS_DIR" ]; then
    log "migrations-apply" "applying migrations from $HASURA_GRAPHQL_MIGRATIONS_DIR"
    mkdir -p "$TEMP_MIGRATIONS_DIR"
    cp -a "$HASURA_GRAPHQL_MIGRATIONS_DIR/." "$TEMP_MIGRATIONS_DIR/migrations/"
    cd "$TEMP_MIGRATIONS_DIR"
    echo "endpoint: http://localhost:$HASURA_GRAPHQL_MIGRATIONS_SERVER_PORT" > config.yaml
    hasura-cli migrate apply
    # check if metadata.[yaml|json] exist and apply
    if [ -f migrations/metadata.yaml ]; then
        log "migrations-apply" "applying metadata from $HASURA_GRAPHQL_MIGRATIONS_DIR/metadata.yaml"
        hasura-cli metadata apply
    elif [ -f migrations/metadata.json ]; then
        log "migrations-apply" "applying metadata from $HASURA_GRAPHQL_MIGRATIONS_DIR/metadata.json"
        hasura-cli metadata apply
    fi
else
    log "migrations-apply" "directory $HASURA_GRAPHQL_MIGRATIONS_DIR does not exist, skipping migrations"
fi

# kill graphql engine that we started earlier
log "migrations-shutdown" "killing temporary server"
kill $PID

# pass control to CMD
log "migrations-shutdown" "graphql-engine will now start in normal mode"
exec "$@"
