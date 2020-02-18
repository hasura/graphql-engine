#! /usr/bin/env bash
# This is the non-circleci version of run.sh
#
# This script will start postgres in a docker container,
# sets a python virtual environment,
# and sets some of the required variables that run.sh needs,
# before executing run.sh
set -euo pipefail
ROOT="${BASH_SOURCE[0]%/*}"

SERVER_DIR="$ROOT/../../server"

cd $SERVER_DIR
set -x
cabal new-build --project-file=cabal.project.dev-sh exe:graphql-engine
export SERVER_BINARY=$(cabal new-exec which graphql-engine)
cd -
set +x

export SERVER_OUTPUT_DIR="server-output"
export LATEST_SERVER_BINARY="./graphql-engine-latest"

# Create Python virtualenv
if ! [ -f ".venv/bin/activate" ] ; then
	virtualenv .venv
fi

. .venv/bin/activate

PG_PORT=25432
PG_CONTAINER_NAME="hasura-dev-postgres-$PG_PORT"
export PGPASSWORD=postgres
CONF=$(cat <<-EOF
log_statement=all
log_connections=on
log_disconnections=on
log_hostname=off
log_duration=on
port=$PG_PORT
EOF
)
export HASURA_GRAPHQL_DATABASE_URL="postgres://postgres:$PGPASSWORD@127.0.0.1:$PG_PORT/postgres"

DOCKER_PSQL="docker exec -u postgres -it $PG_CONTAINER_NAME psql -p $PG_PORT"

function wait_docker_postgres {
  echo -n "Waiting for postgres to come up"
  until $DOCKER_PSQL postgres -c '\l' &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"
}

function launch_postgres_container(){
	CONF_FLAGS=$(echo "$CONF" | sed  -e 's/^/-c /'  | tr '\n' ' ')
	echo "Launching postgres container: $PG_CONTAINER_NAME"
	set -x
	docker run --name "$PG_CONTAINER_NAME" -p 127.0.0.1:"$PG_PORT":$PG_PORT --expose="$PG_PORT" \
		-e POSTGRES_PASSWORD="$PGPASSWORD"  -d circleci/postgres:11.5-alpine-postgis $CONF_FLAGS
	set +x
}

function stop_postgres_container(){
	docker stop "$PG_CONTAINER_NAME"
        docker rm -v "$PG_CONTAINER_NAME"
}

launch_postgres_container

wait_docker_postgres

trap stop_postgres_container ERR

set -x
"$ROOT"/run.sh "$@"
set +x

stop_postgres_container
