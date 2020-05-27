#!/bin/bash
set -euo pipefail
shopt -s globstar

# A development swiss army knife script. The goals are to:
#
#  - encode some best practices and hard-won knowledge of quirks and corners of
#    our tooling
#  - simplify development; especially for new-comers; instead of writing a huge
#    document describing how to do various dev tasks (or worse yet, not writing
#    one), make it runnable
#
# This makes use of 'cabal.project.dev-sh*' files when building. See
# 'cabal.project.dev-sh.local'.


echo_pretty() {
    echo ">>> $(tput setaf 2)$1$(tput sgr0)"
}
echo_error() {
    echo ">>> $(tput setaf 1)$1$(tput sgr0)"
}
echo_warn() {
    echo ">>> $(tput setaf 3)$1$(tput sgr0)"
}

die_usage() {
cat <<EOL
A swiss-army script for local graphql-engine development

Usage:   $0 <COMMAND>

Available COMMANDs:

  graphql-engine
    Launch graphql-engine, connecting to a database launched with '$0 postgres'.

  postgres
    Launch a postgres container suitable for use with graphql-engine, watch its logs,
    clean up nicely after

  test [--integration [pytest_args...] | --unit]
    Run the unit and integration tests, handling spinning up all dependencies.
    This will force a recompile. A combined code coverage report will be
    generated for all test suites.
        Either integration or unit tests can be run individually with their
    respective flags. With '--integration' any arguments that follow will be
    passed to the pytest invocation

EOL
exit 1
}

# Prettify JSON output, if possible
try_jq() {
  if command -v jq >/dev/null; then
    command jq --unbuffered -R -r '. as $line | try fromjson catch $line'
  else
    cat
  fi
}

# Bump this to:
#  - force a reinstall of python dependencies, etc.
DEVSH_VERSION=1.3

case "${1-}" in
  graphql-engine)
    case "${2-}" in
      --no-rebuild)
      echo_error 'The --no-rebuild option is no longer supported.'
      die_usage
      ;;
      "")
      ;;
      *)
      die_usage
      ;;
    esac
  ;;
  postgres)
  ;;
  test)
    case "${2-}" in
      --unit)
      RUN_INTEGRATION_TESTS=false
      RUN_UNIT_TESTS=true
      ;;
      --integration)
      PYTEST_ARGS="${@:3}"
      RUN_INTEGRATION_TESTS=true
      RUN_UNIT_TESTS=false
      ;;
      "")
      RUN_INTEGRATION_TESTS=true
      RUN_UNIT_TESTS=true
      ;;
      *)
      die_usage
      ;;
    esac
  ;;
  *)
    die_usage
  ;;
esac

# For now:
MODE="$1"


PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$PROJECT_ROOT"

# Use pyenv if available to set an appropriate python version that will work with pytests etc.
if command -v pyenv >/dev/null; then
  # For now I guess use the greatest python3 >= 3.5
  v=$(pyenv versions --bare | (grep  '^ *3' || true) | awk '{if($1>=3.5)print$1}' | tail -n1)
  if [ -z "$v" ]; then
    echo_error 'Please `pyenv install` a version of python >= 3.5 so we can use it'
    exit 2
  fi
  echo_pretty "Pyenv found. Using python version: $v"
  export PYENV_VERSION=$v
  python3 --version
else
  echo_warn "Pyenv not installed. Proceeding with system python version: $(python3 --version)"
fi

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
  # Existing tix files for a different hge binary will cause issues:
  rm -f graphql-engine.tix

  # Attempt to run this after a CTRL-C:
  function cleanup {
    echo
    # Generate coverage, which can be useful for debugging or understanding
    if command -v hpc >/dev/null && command -v jq >/dev/null ; then
      # Get the appropriate mix dir (the newest one). This way this hopefully
      # works when cabal.project.dev-sh.local is edited to turn on optimizations.
      # See also: https://hackage.haskell.org/package/cabal-plan
      distdir=$(cat dist-newstyle/cache/plan.json | jq -r '."install-plan"[] | select(."id" == "graphql-engine-1.0.0-inplace")? | ."dist-dir"')
      hpcdir="$distdir/hpc/vanilla/mix/graphql-engine-1.0.0"
      echo_pretty "Generating code coverage report..."
      COVERAGE_DIR="dist-newstyle/dev.sh-coverage"
      hpc_invocation=(hpc markup
        --exclude=Main
        --hpcdir "$hpcdir"
        --reset-hpcdirs graphql-engine.tix
        --fun-entry-count
        --destdir="$COVERAGE_DIR")
      ${hpc_invocation[@]} >/dev/null

      echo_pretty "To view full coverage report open:"
      echo_pretty "  file://$(pwd)/$COVERAGE_DIR/hpc_index.html"

      tix_archive=dist-newstyle/graphql-engine.tix.$(date "+%Y.%m.%d-%H.%M.%S")
      mv graphql-engine.tix "$tix_archive"
      echo_pretty ""
      echo_pretty "The tix file we used has been archived to: $tix_archive"
      echo_pretty ""
      echo_pretty "You might want to use 'hpc combine' to create a diff of two different tix"
      echo_pretty "files, and then generate a new report with something like:"
      echo_pretty "  $ ${hpc_invocation[*]}"
    else
      echo_warn "Please install 'hpc' and 'jq' to get a code coverage report"
    fi
  }
  trap cleanup EXIT


  export HASURA_GRAPHQL_SERVER_PORT=${HASURA_GRAPHQL_SERVER_PORT-8181}

  echo_pretty "We will connect to postgres container '$PG_CONTAINER_NAME'"
  echo_pretty "If you haven't yet, please launch a postgres container in a separate terminal with:"
  echo_pretty "    $ $0 postgres"
  echo_pretty "or press CTRL-C and invoke graphql-engine manually"
  echo_pretty ""

  RUN_INVOCATION=(cabal new-run --project-file=cabal.project.dev-sh --RTS -- exe:graphql-engine +RTS -N -T -RTS
    --database-url="$DB_URL" serve
    --enable-console --console-assets-dir "$PROJECT_ROOT/console/static/dist")

  echo_pretty 'About to do:'
  echo_pretty '    $ cabal new-build --project-file=cabal.project.dev-sh exe:graphql-engine'
  echo_pretty "    $ ${RUN_INVOCATION[*]}"
  echo_pretty ''

  cabal new-build --project-file=cabal.project.dev-sh exe:graphql-engine
  wait_docker_postgres

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
  ${RUN_INVOCATION[@]} | try_jq
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

function launch_postgres_container(){
  echo_pretty "Launching postgres container: $PG_CONTAINER_NAME"
  docker run --name "$PG_CONTAINER_NAME" -p 127.0.0.1:"$PG_PORT":$PG_PORT --expose="$PG_PORT" \
    -e POSTGRES_PASSWORD="$PGPASSWORD"  -d circleci/postgres:11.5-alpine-postgis \
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
      # Kill the cabal new-run and its children. This may already have been killed:
      pkill -P "$GRAPHQL_ENGINE_PID" &>/dev/null || true
    fi

    case "$MODE" in
      test|postgres)
        # Since scripts here are tailored to the env we've just launched:
        rm -r "$DEV_SHIM_PATH"

        echo_pretty "Removing $PG_CONTAINER_NAME and its volumes in 5 seconds!"
        echo_pretty "  PRESS CTRL-C TO ABORT removal, or ENTER to clean up right away"
        read -t5 || true
        docker stop "$PG_CONTAINER_NAME"
        docker rm -v "$PG_CONTAINER_NAME"
      ;;
      graphql-engine)
      ;;
    esac

    echo_pretty "Done"
  }
  trap cleanup EXIT
}


if [ "$MODE" = "postgres" ]; then
  launch_postgres_container
  wait_docker_postgres
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
  ########################################
  ###     Integration / unit tests     ###
  ########################################
  cd "$PROJECT_ROOT/server"

  # Until we can use a real webserver for TestEventFlood, limit concurrency
  export HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE=8

  # We'll get an hpc error if these exist; they will be deleted below too:
  rm -f graphql-engine-tests.tix graphql-engine.tix graphql-engine-combined.tix

  export EVENT_WEBHOOK_HEADER="MyEnvValue"
  export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
  export SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN="http://127.0.0.1:5594"

  # It's better UX to build first (possibly failing) before trying to launch
  # PG, but make sure that new-run uses the exact same build plan, else we risk
  # rebuilding twice... ugh
  cabal new-build --project-file=cabal.project.dev-sh exe:graphql-engine test:graphql-engine-tests
  launch_postgres_container
  wait_docker_postgres

  # These also depend on a running DB:
  if [ "$RUN_UNIT_TESTS" = true ]; then
    echo_pretty "Running Haskell test suite"
    HASURA_GRAPHQL_DATABASE_URL="$DB_URL" cabal new-run --project-file=cabal.project.dev-sh -- test:graphql-engine-tests
  fi

  if [ "$RUN_INTEGRATION_TESTS" = true ]; then
    GRAPHQL_ENGINE_TEST_LOG=/tmp/hasura-dev-test-engine.log
    echo_pretty "Starting graphql-engine, logging to $GRAPHQL_ENGINE_TEST_LOG"
    export HASURA_GRAPHQL_SERVER_PORT=8088
    cabal new-run --project-file=cabal.project.dev-sh -- exe:graphql-engine --database-url="$DB_URL" serve --stringify-numeric-types \
      --enable-console --console-assets-dir ../console/static/dist \
      &> "$GRAPHQL_ENGINE_TEST_LOG" & GRAPHQL_ENGINE_PID=$!

    echo -n "Waiting for graphql-engine"
    until curl -s "http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/v1/query" &>/dev/null; do
      echo -n '.' && sleep 0.2
      # If the server stopped abort immediately
      if ! kill -0 $GRAPHQL_ENGINE_PID ; then
        echo_error "The server crashed or failed to start!!"
        exit 666
      fi
    done
    echo " Ok"

    ### Check for and install dependencies in venv
    cd "$PROJECT_ROOT/server/tests-py"

    if [ ! -d "node_modules" ]; then
        npm_config_loglevel=error npm install remote_schemas/nodejs/
    else
        echo_pretty "It looks like node dependencies have been installed already. Skipping."
        echo_pretty "If things fail please run this and try again"
        echo_pretty "  $ rm -r \"$PROJECT_ROOT/server/tests-py/node_modules\""
    fi

    PY_VENV=.hasura-dev-python-venv
    DEVSH_VERSION_FILE=.devsh_version
    # Do we need to force reinstall?
    if [ "$DEVSH_VERSION" = "$(cat $DEVSH_VERSION_FILE 2>/dev/null || true)" ]; then
      true # ok
    else
      echo_warn 'dev.sh version was bumped or fresh install. Forcing reinstallation of dependencies.'
      rm -rf "$PY_VENV"
      echo "$DEVSH_VERSION" > "$DEVSH_VERSION_FILE"
    fi
    set +u  # for venv activate
    if [ ! -d "$PY_VENV" ]; then
      python3 -m venv "$PY_VENV"
      source "$PY_VENV/bin/activate"
      pip3 install wheel
      # If the maintainer of this script or pytests needs to change dependencies:
      #  - alter requirements-top-level.txt as needed
      #  - delete requirements.txt
      #  - run this script, then check in the new frozen requirements.txt
      if [ -f requirements.txt ]; then
        pip3 install -r requirements.txt
      else
        pip3 install -r requirements-top-level.txt
        pip3 freeze > requirements.txt
      fi
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
      echo_error "^^^ graphql-engine logs from failed test run can be inspected at: $GRAPHQL_ENGINE_TEST_LOG"
    fi
    deactivate  # python venv
    set -u

    cd "$PROJECT_ROOT/server"
    # Kill the cabal new-run and its children. INT so we get hpc report:
    pkill -INT -P "$GRAPHQL_ENGINE_PID"
    wait "$GRAPHQL_ENGINE_PID" || true
    echo
  fi  # RUN_INTEGRATION_TESTS

  # If hpc available, combine any tix from haskell/unit tests:
  if command -v hpc >/dev/null; then
    if [ "$RUN_UNIT_TESTS" = true ] && [ "$RUN_INTEGRATION_TESTS" = true ]; then
      # As below, it seems we variously get errors related to having two Main
      # modules, so exclude:
      hpc combine --exclude=Main graphql-engine-tests.tix graphql-engine.tix --union > graphql-engine-combined.tix
    else
      # One of these should exist
      cp graphql-engine-tests.tix graphql-engine-combined.tix 2>/dev/null || true
      cp graphql-engine.tix       graphql-engine-combined.tix 2>/dev/null || true
    fi
    # Generate a report including the test code itself (see cabal.project.dev-sh.local):
    # NOTE: we have to omit the .mix directory for the executable, since it
    # seems hpc can't cope with two modules of the same name; '--exclude'
    # didn't help.
    echo_pretty "Generating code coverage report..."
    COVERAGE_DIR="dist-newstyle/dev.sh-coverage"
    hpc markup \
      --exclude=Main \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/noopt/hpc/vanilla/mix/graphql-engine-* \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/t/graphql-engine-tests/noopt/hpc/vanilla/mix/graphql-engine-tests \
      --reset-hpcdirs graphql-engine-combined.tix \
      --fun-entry-count \
      --destdir="$COVERAGE_DIR" >/dev/null
    hpc report \
      --exclude=Main \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/noopt/hpc/vanilla/mix/graphql-engine-* \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/t/graphql-engine-tests/noopt/hpc/vanilla/mix/graphql-engine-tests \
      --reset-hpcdirs graphql-engine-combined.tix
    echo_pretty "To view full coverage report open:"
    echo_pretty "  file://$(pwd)/$COVERAGE_DIR/hpc_index.html"

  else
    echo_warn "Please install hpc to get a combined code coverage report for tests"
  fi
  rm -f graphql-engine-tests.tix graphql-engine.tix graphql-engine-combined.tix

else
  echo "impossible; fix script."
fi
