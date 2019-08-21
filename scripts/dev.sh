#!/bin/bash
set -euo pipefail

# A convenience script that launches a fresh postgres container instance that
# can be used by the graphql-engine server. After launch the verbose postgres
# logs will be printed. On shutdown we'll try to clean up the container
# completely.


echo_pretty() {
    echo ">>> $(tput setaf 2)$1$(tput sgr0)"
}

die_usage() {
cat <<EOL
A swiss-army script for local graphql-engine development

Usage:   $0 <COMMAND>

Available COMMANDs:

  graphql-engine [--no-rebuild]
    Launch graphql-engine, connecting to a database launched with '$0 postgres'
    You can pass --no-rebuild if you want to launch an instance from source you
    previously built if you have a dirty tree.

  postgres
    Launch a postgres container suitable for use with graphql-engine, watch its logs, 
    clean up nicely after

  test [pytest_args...]
    Run the integration tests, handling spinning up all dependencies. This will force
    a recompile. A code coverage report will be generated. All arguments after 'test'
    will be passed to the 'pytest' invocation.

EOL
exit 1
}

case "${1-}" in
  graphql-engine)
    case "${2-}" in
      --no-rebuild)
      REBUILD=false
      ;;
      "")
      REBUILD=true
      ;;
      *)
      die_usage
      ;;
    esac
  ;;
  postgres)
  ;;
  test)
    PYTEST_ARGS="${@:2}"
  ;;
  *)
    die_usage
  ;;
esac

# For now:
MODE="$1"


PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$PROJECT_ROOT"


####################################
###   Shared environment stuff   ###
####################################

# Hopefully these don't clash with anything. We could try to be smarter:
if [ "$MODE" = "test" ]; then
  # Choose a different port so PG is totally disposable:
  PG_PORT=35432
else
  PG_PORT=25432
fi

# export for psql, etc.
export PGPASSWORD=postgres

DB_URL="postgres://postgres:$PGPASSWORD@127.0.0.1:$PG_PORT/postgres"

PG_CONTAINER_NAME="hasura-dev-postgres-$PG_PORT"

# We can remove psql as a dependency:
DOCKER_PSQL="docker exec -u postgres -it $PG_CONTAINER_NAME psql -p $PG_PORT"

function wait_docker_postgres {
  echo -n "Waiting for postgres to come up"
  until $DOCKER_PSQL postgres -c '\l' &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"
}

#################################
###     Graphql-engine        ###
#################################
if [ "$MODE" = "graphql-engine" ]; then
  cd "$PROJECT_ROOT/server"

  export HASURA_GRAPHQL_SERVER_PORT=${HASURA_GRAPHQL_SERVER_PORT-8181}

  # Prettify JSON output if possible:
  if command -v jq ; then
    PIPE_JQ="| jq --unbuffered -R -r '. as \$line | try fromjson catch \$line'"
  fi

  echo_pretty "We will connect to postgres container '$PG_CONTAINER_NAME'"
  echo_pretty "If you haven't yet, please launch a postgres container in a separate terminal with:"
  echo_pretty "    $ $0 postgres"
  echo_pretty "or press CTRL-C and invoke graphql-engine manually"
  wait_docker_postgres

  # Starts EKG, fast build without optimizations
  BUILD_INVOCATION="stack build --fast --flag graphql-engine:developer --ghc-options=-j"
  RUN_INVOCATION="stack exec graphql-engine -- --database-url='$DB_URL' serve --enable-console --console-assets-dir \'$PROJECT_ROOT/console/static/dist\' +RTS -N -T -RTS ${PIPE_JQ-}"

  echo_pretty "About to do:"
  echo_pretty "    $ $BUILD_INVOCATION"
  echo_pretty "    $ $RUN_INVOCATION"
  echo_pretty ""

  # `stack exec` is a footgun, as it will happily execute a graphql-engine elsewhere in user's path:
  if [ "$REBUILD" = false ]; then
    if [[ ! -x "$(stack path --local-install-root)/bin/graphql-engine" ]]; then
      echo "You requested --no-rebuild but graphql-engine hasn't been built."
      echo "Please do e.g."
      echo "   $ $BUILD_INVOCATION"  # Naughty and dangerous!
      exit 3
    fi
  else
    $BUILD_INVOCATION
  fi

  # Print helpful info after startup logs so it's visible:
  {
    until curl -s "http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/v1/query" &>/dev/null; do
      sleep 0.2
    done
    sleep 1
    echo_pretty "▲▲▲ graphql-engine startup logs above ▲▲▲" 
    echo_pretty "" 
    echo_pretty "You can set additional environment vars to tailor 'graphql-engine' next time you"
    echo_pretty "invoke this script, e.g.:"
    echo_pretty "    # Keep polling statements out of logs"
    echo_pretty "    HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL=3000000"
    echo_pretty ""
    echo_pretty "The hasura console is available at:"
    echo_pretty "    http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/console"
    echo_pretty ""
    echo_pretty "  If the console was modified since your last build (re)build assets with:"
    echo_pretty "      $ cd \"$PROJECT_ROOT/console\""
    echo_pretty "      $ npm ci && npm run server-build "
    echo_pretty ""
    echo_pretty "Useful endpoints when compiling with 'graphql-engine:developer' and running with '+RTS -T'"
    echo_pretty "   http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/dev/subscriptions"
    echo_pretty "   http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/dev/plan_cache"
    echo_pretty ""
    echo_pretty "To view realtime GC stats and other info open in your browser:"
    echo_pretty "    file://$PROJECT_ROOT/scripts/ekg/ekg.html#$HASURA_GRAPHQL_SERVER_PORT"
    echo_pretty ""
    echo_pretty "▼▼▼ additional graphql-engine logs will appear below: ▼▼▼"
  } &

  # Logs printed until CTRL-C:
  eval "$RUN_INVOCATION"  # Naughty and dangerous!
  exit 0
  ### END SCRIPT ###
fi

#################################
###     Postgres Container    ###
#################################


# Useful development defaults for postgres (no spaces here, please):
#
# setting 'port' in container is a workaround for the pg_dump endpoint (see tests)
# log_hostname=off to avoid timeout failures when running offline due to:
#   https://forums.aws.amazon.com/thread.jspa?threadID=291285 
CONF=$(cat <<-EOF
log_statement=all
log_connections=on
log_disconnections=on
log_hostname=off
log_duration=on
port=$PG_PORT
EOF
)

# log lines above as -c flag arguments we pass to postgres
CONF_FLAGS=$(echo "$CONF" | sed  -e 's/^/-c /'  | tr '\n' ' ')


# The unofficial 'mdillon/postgis' comes with postgis installed, needed for tests:
docker run --name "$PG_CONTAINER_NAME" -p 127.0.0.1:"$PG_PORT":$PG_PORT --expose="$PG_PORT" \
  -e POSTGRES_PASSWORD="$PGPASSWORD"  -d mdillon/postgis:11 \
  $CONF_FLAGS


# graphql-engine calls the pg_dump executable. To avoid a version mismatch (and
# the dependency entirely) we create a shim that executes the pg_dump in the
# postgres container. Note output to file won't work.
DEV_SHIM_PATH="/tmp/hasura-dev-shims-$PG_PORT"
mkdir -p "$DEV_SHIM_PATH"
cat >"$DEV_SHIM_PATH/pg_dump" <<EOL
#!/bin/bash
# Generated from: $0
if [[ \$@ == *" -f"* ]]; then
  echo "It looks like we're trying to pg_dump to a file, but that won't work with this shim. See $0" >&2 
  exit 1
fi
docker exec -u postgres $PG_CONTAINER_NAME pg_dump "\$@"
EOL
chmod a+x "$DEV_SHIM_PATH/pg_dump"
export PATH="$DEV_SHIM_PATH":$PATH


# Since launching the postgres container worked we can set up cleanup routines. This will catch CTRL-C
function cleanup {
  echo

  if [ ! -z "${GRAPHQL_ENGINE_PID-}" ]; then
    # This may already have been killed:
    kill "$GRAPHQL_ENGINE_PID" &>/dev/null || true
  fi

  case "$MODE" in
    test|postgres)
      # Since scripts here are tailored to the env we've just launched:
      rm -r "$DEV_SHIM_PATH"

      echo_pretty "Removing $PG_CONTAINER_NAME and its volumes in 5 seconds!  PRESS CTRL-C TO ABORT."
      sleep 5
      docker stop "$PG_CONTAINER_NAME" 
      docker rm -v "$PG_CONTAINER_NAME"
    ;;
    graphql-engine)
    ;;
  esac

  echo_pretty "Done"
}
trap cleanup EXIT 

wait_docker_postgres


if [ "$MODE" = "postgres" ]; then
  echo_pretty "Postgres logs will start to show up in realtime here. Press CTRL-C to exit and "
  echo_pretty "shutdown this container."
  echo_pretty ""
  echo_pretty "You can use the following to connect to the running instance:"
  echo_pretty "    $ $DOCKER_PSQL"
  echo_pretty "        or..."
  echo_pretty "    $ PGPASSWORD="$PGPASSWORD" psql -h 127.0.0.1 -p "$PG_PORT" postgres -U postgres"
  echo_pretty ""
  echo_pretty "Here is the database URL:"
  echo_pretty "    $DB_URL"
  echo_pretty ""
  echo_pretty "If you want to launch a 'graphql-engine' that works with this database:"
  echo_pretty "    $ $0 graphql-engine"
  # Runs continuously until CTRL-C, jumping to cleanup() above:
  docker logs -f --tail=0 "$PG_CONTAINER_NAME"

elif [ "$MODE" = "test" ]; then 
  #################################
  ###     Integration tests     ###
  #################################
  cd "$PROJECT_ROOT/server"

  export EVENT_WEBHOOK_HEADER="MyEnvValue"
  export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"

  echo_pretty "Rebuilding for code coverage"
  stack build --fast --flag graphql-engine:developer --ghc-options=-j --coverage 

  echo_pretty "Starting graphql-engine"
  GRAPHQL_ENGINE_TEST_LOG=/tmp/hasura-dev-test-engine.log
  export HASURA_GRAPHQL_SERVER_PORT=8088 
  # stopped in cleanup()
  stack exec graphql-engine -- --database-url="$DB_URL" serve --enable-console --stringify-numeric-types \
    --console-assets-dir ../console/static/dist  &>  $GRAPHQL_ENGINE_TEST_LOG  & GRAPHQL_ENGINE_PID=$!
  echo -n "Waiting for graphql-engine"
  until curl -s "http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/v1/query" &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"

  ## Install misc test dependencies:
  cd "$PROJECT_ROOT/server/tests-py"


  if [ ! -d "node_modules" ]; then
    npm_config_loglevel=error npm install remote_schemas/nodejs/
  else
    echo_pretty "It looks like node dependencies have been installed already. Skipping."
    echo_pretty "If things fail please run this and try again"
    echo_pretty "  $ rm -r \"$PROJECT_ROOT/server/tests-py/node_modules\""
  fi

  PY_VENV=.hasura-dev-python-venv
  set +u  # for venv activate
  if [ ! -d "$PY_VENV" ]; then
    python3 -m venv "$PY_VENV"
    source "$PY_VENV/bin/activate"
    pip3 install -r requirements.txt
  else
    echo_pretty "It looks like python dependencies have been installed already. Skipping."
    echo_pretty "If things fail please run this and try again"
    echo_pretty "  $ rm -r \"$PROJECT_ROOT/server/tests-py/$PY_VENV\""

    source "$PY_VENV/bin/activate"
  fi

  # TODO MAYBE: fix deprecation warnings, make them an error
  if pytest -W ignore::DeprecationWarning --hge-urls http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT --pg-urls "$DB_URL" $PYTEST_ARGS; then
    PASSED=true
  else
    PASSED=false
    echo_pretty "^^^ graphql-engine logs from failed test run can be inspected at: $GRAPHQL_ENGINE_TEST_LOG" 
  fi
  deactivate  # python venv
  set -u
  
  cd "$PROJECT_ROOT/server"
  # INT so we get hpc report
  kill -INT "$GRAPHQL_ENGINE_PID"
  wait "$GRAPHQL_ENGINE_PID" || true
  echo
  stack hpc report graphql-engine.tix
  rm graphql-engine.tix

else
  echo "impossible; fix script."
fi
