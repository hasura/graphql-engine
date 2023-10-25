#!/usr/bin/env bash
# shellcheck disable=SC1091 # We do not want Shellcheck to validate that sourced scripts are present.

set -euo pipefail
shopt -s globstar
# for extended case patterns:
shopt -s extglob

# A development swiss army knife script. The goals are to:
#
#  - encode some best practices and hard-won knowledge of quirks and corners of
#    our tooling
#  - simplify development; especially for new-comers; instead of writing a huge
#    document describing how to do various dev tasks (or worse yet, not writing
#    one), make it runnable
#
# This makes use of 'cabal/dev-sh*.project' files when building.
# See 'cabal/dev-sh.project.local' for details, and $CABAL_PROJECT_FILE below.
#
# The configuration for the containers of each backend is stored in
# separate files, see files in 'scripts/containers'

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

  graphql-engine [--optimized | --prof-ticky | --prof-heap-infomap |--prof-ghc-debug] [-- <extra_args>]
      
    Launch graphql-engine, connecting to a database launched with
    '$0 postgres'. <extra_args> will be passed to graphql-engine directly.
    
        --optimized         : will launch a prod-like optimized build
        --prof-ticky        : "Ticky ticky" profiling for accounting of allocations (see: cabal/README.md)
        --prof-heap-infomap : Heap profiling (see: cabal/README.md)
        --prof-ghc-debug    : Enable ghc-debug (see: cabal/README.md)
        --prof-time         : Time profiling (see: cabal/README.md)

  postgres
    Launch a postgres container suitable for use with graphql-engine, watch its
    logs, clean up nicely after

  mssql
    Launch a MSSQL container suitable for use with graphql-engine, watch its
    logs, clean up nicely after

  citus
    Launch a Citus single-node container suitable for use with graphql-engine,
    watch its logs, clean up nicely after

  mysql
    Launch a MySQL container suitable for use with graphql-engine, watch its
    logs, clean up nicely after

  test [--integration [pytest_args...] | --unit | --hlint]
    Run the unit and integration tests, handling spinning up all dependencies.
    This will force a recompile. A combined code coverage report will be
    generated for all test suites.
    Either integration or unit tests can be run individually with their
    respective flags. With '--integration' any arguments that follow will be
    passed to the pytest invocation. Run the hlint code linter individually
    using '--hlint'.

    For unit tests, you can limit the number of tests by using
    'test --unit --match "runTx" mssql'

EOL
exit 1
}

# See: TODO
cabal --version | grep -q ' 3\.10' || { 
    echo_error "Please use cabal 3.10, as cabal broke 'import' and we can't make it compatible"
    exit 1
}

# The default configuration this script expects. May be overridden depending on
# flags passed to subcommands, or this can be edited for one-off tests:
CABAL_PROJECT_FILE=cabal/dev-sh.project

# Prettify JSON output, if possible
try_jq() {
  if command -v jq >/dev/null; then
    command jq --unbuffered -R -r '. as $line | try fromjson catch $line'
  else
    cat
  fi
}

case "${1-}" in
  graphql-engine?(-pro) )
    ## The differences between OSS and Enterprise/pro defined here:
    EDITION_NAME="${1-}"
    if [ "$EDITION_NAME" = "graphql-engine-pro" ];then
      EDITION_ABBREV=ee
      if [ -z "${HASURA_GRAPHQL_EE_LICENSE_KEY-}" ]; then
          echo_error "You need to have the HASURA_GRAPHQL_EE_LICENSE_KEY environment variable defined." 
          echo_error "Ask a pro developer for the dev key."
          exit 1
      fi
      # This is required for pro with EE license available:
      if [ -z "${HASURA_GRAPHQL_ADMIN_SECRET-}" ]; then
        # This should match benchmarks and other dev utilities:
        export HASURA_GRAPHQL_ADMIN_SECRET=my-secret
      fi
    else
      EDITION_ABBREV=ce
    fi

    # pass arguments after '--' directly to engine:
    GRAPHQL_ENGINE_EXTRA_ARGS=()
    case "${2-}" in

      --no-rebuild)
      echo_error 'The --no-rebuild option is no longer supported.'
      die_usage
      ;;

      --prof-ticky)
      if [ -f "$EDITION_NAME.ticky" ]; then
          echo_error "The file '$EDITION_NAME.ticky' exists and we would clobber it. Please delete or rename it and try again."
          exit 1
      fi
      echo_warn "This will perform significant recompilation. Ok?"
      echo_warn "    Press enter to continue [will proceed in 10s]"
      read -r -t10 || true
      CABAL_PROJECT_FILE=cabal/dev-sh-prof-ticky.project
      HASURA_PROF_MODE=ticky
      GRAPHQL_ENGINE_EXTRA_ARGS+=( +RTS -r -RTS )
      case "${3-}" in
          --)
          GRAPHQL_ENGINE_EXTRA_ARGS+=( "${@:4}" )
          ;;
      esac
      ;;

      --prof-heap-infomap)
      echo_warn "This will delete any '$EDITION_NAME.eventlog' and '$EDITION_NAME.eventlog.html' and perform significant recompilation. Ok?"
      echo_warn  "Press enter to continue [will proceed in 10s]"
      read -r -t10 || true
      # Avoid confusion:
      rm -f "$EDITION_NAME.eventlog"
      rm -f "$EDITION_NAME.eventlog.html"
      CABAL_PROJECT_FILE=cabal/dev-sh-prof-heap-infomap.project
      HASURA_PROF_MODE=heap-infomap
      GRAPHQL_ENGINE_EXTRA_ARGS+=( +RTS -hi -l-agu -RTS )
      case "${3-}" in
          --)
          GRAPHQL_ENGINE_EXTRA_ARGS+=( "${@:4}" )
          ;;
      esac
      ;;

      --prof-ghc-debug)
      # Used by ghc-debug-stub:
      export GHC_DEBUG_SOCKET=/tmp/ghc-debug
      echo_warn "This will require significant recompilation unless you just ran with --prof-heap-infomap "
      echo_warn "A GHC debug socket will be opened at $GHC_DEBUG_SOCKET"
      echo_warn "See examples of client code here: https://github.com/hasura/hasura-debug/"
      echo_warn  "Press enter to continue [will proceed in 10s]"
      read -r -t10 || true
      # NOTE: we just need IPE info so can re-use this:
      CABAL_PROJECT_FILE=cabal/dev-sh-prof-heap-infomap.project
      # This will open the debug socket:
      export HASURA_GHC_DEBUG=true
      HASURA_PROF_MODE=ghc-debug
      case "${3-}" in
          --)
          GRAPHQL_ENGINE_EXTRA_ARGS+=( "${@:4}" )
          ;;
      esac
      ;;

      --prof-time)
      echo_warn "This will delete any $EDITION_NAME.prof and perform significant recompilation."
      echo_warn  "Press enter to continue [will proceed in 10s]"
      read -r -t10 || true
      rm -f "$EDITION_NAME.prof"
      rm -f "$EDITION_NAME.profiterole.html"
      CABAL_PROJECT_FILE=cabal/dev-sh-prof-time.project
      HASURA_PROF_MODE="time"
      GRAPHQL_ENGINE_EXTRA_ARGS+=( +RTS -P -RTS )
      # TODO alternatively we can do `-pj` and use speedscope (unfortunately we
      # can't get both formats of output), but I think profiterole is more
      # useful
      # GRAPHQL_ENGINE_EXTRA_ARGS+=( +RTS -pj -RTS )
      case "${3-}" in
          --)
          GRAPHQL_ENGINE_EXTRA_ARGS+=( "${@:4}" )
          ;;
      esac
      ;;

      --optimized)
      CABAL_PROJECT_FILE=cabal/dev-sh-optimized.project
      case "${3-}" in
          --)
          GRAPHQL_ENGINE_EXTRA_ARGS+=( "${@:4}" )
          ;;
      esac
      ;;

      --)
      GRAPHQL_ENGINE_EXTRA_ARGS+=( "${@:3}" )
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
  mssql)
  ;;
  citus)
  ;;
  mysql)
  ;;
  test)
    case "${2-}" in
      --unit)
      UNIT_TEST_ARGS=( "${@:3}" )
      RUN_INTEGRATION_TESTS=false
      RUN_UNIT_TESTS=true
      RUN_HLINT=false
      ;;
      --integration)
      PYTEST_ARGS=( "${@:3}" )
      RUN_INTEGRATION_TESTS=true
      RUN_UNIT_TESTS=false
      RUN_HLINT=false
      source scripts/parse-pytest-backend
      ;;
      --hlint)
      RUN_INTEGRATION_TESTS=false
      RUN_UNIT_TESTS=false
      RUN_HLINT=true
      ;;
      "")
      RUN_INTEGRATION_TESTS=true
      RUN_UNIT_TESTS=true
      RUN_HLINT=true
      BACKEND="postgres"
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

# In CI we use the get version script to actually populate the version number
# that will be compiled into the server. For local development we use this
# magic number, which means we won't recompile unnecessarily. This number also
# gets explicitly ignored in the version test in integration tests.
echo '12345' > "$PROJECT_ROOT/server/CURRENT_VERSION"

# Use pyenv if available to set an appropriate python version that will work with pytests etc.
# Note: this does not help at all on 'nix' environments since 'pyenv' is not
# something you normally use under nix.
if command -v pyenv >/dev/null; then
  # Use the latest version of Python installed with `pyenv`.
  # Ensure that it is at least v3.9, so that generic types are fully supported.
  v="$(pyenv versions --bare | (grep  '^ *3' || true) | awk '{ print $1 }' | tail -n1)"

  # Awk fails when you compare e.g. 3.9 and 3.10, because 3.9 is a higher
  # number than 3.1 (having coerced both to floats). So, we convert a version
  # like 1.20.3 to a number like 001020003000 (every section becomes a
  # three-digit number) and we compare them instead.
  formatted="$(printf "%03d%03d%03d%03d" $(echo $v | tr '.' ' '))"
  if [[ "$formatted" -lt "003009000000" ]]; then
    # shellcheck disable=SC2016
    echo_error 'Please `pyenv install` a version of python >= 3.9 (found '$v')'
    exit 2
  fi

  echo_pretty "Pyenv found. Using Python version: $v"
  export PYENV_VERSION=$v
  python3 --version
else
  echo_warn "Pyenv not installed. Proceeding with Python from the path, version: $(python3 --version)"
fi


####################################
###       Containers setup       ###
####################################

source scripts/containers/postgres
source scripts/containers/mssql.sh
source scripts/containers/citus
source scripts/containers/mysql.sh
source scripts/data-sources-util.sh

PG_RUNNING=0
MSSQL_RUNNING=0
CITUS_RUNNING=0
MYSQL_RUNNING=0

function cleanup {
  echo

  if [ -n "${GRAPHQL_ENGINE_PID-}" ]; then
    # Kill the cabal new-run and its children. This may already have been killed:
    pkill -P "$GRAPHQL_ENGINE_PID" &>/dev/null || true
  fi

  if [    $PG_RUNNING -eq 1 ]; then    pg_cleanup; fi
  if [ $MSSQL_RUNNING -eq 1 ]; then mssql_cleanup; fi
  if [ $CITUS_RUNNING -eq 1 ]; then citus_cleanup; fi
  if [ $MYSQL_RUNNING -eq 1 ]; then mysql_cleanup; fi

  echo_pretty "Done"
}

trap cleanup EXIT

function pg_start() {
  if [ $PG_RUNNING -eq 0 ]; then
    pg_launch_container
    PG_RUNNING=1
    pg_wait
  fi
}

function mssql_start() {
  if [ $MSSQL_RUNNING -eq 0 ]; then
    mssql_launch_container
    MSSQL_RUNNING=1
    mssql_wait
  fi
}

function citus_start() {
  if [ $CITUS_RUNNING -eq 0 ]; then
    citus_launch_container
    CITUS_RUNNING=1
    citus_wait
  fi
}

function mysql_start() {
  if [ $MYSQL_RUNNING -eq 0 ]; then
    mysql_launch_container
    MYSQL_RUNNING=1
    mysql_wait
  fi
}

function start_dbs() {
  # always launch the postgres container
  pg_start

  case "$BACKEND" in
    citus)
      citus_start
    ;;
    mssql)
      mssql_start
    ;;
    mysql)
      mysql_start
    ;;
    # bigquery deliberately omitted as its test setup is atypical. See:
    # https://github.com/hasura/graphql-engine/blob/master/server/py-tests/README.md#running-bigquery-tests
  esac
}


#################################
###     Graphql-engine        ###
#################################

if [ "$MODE" = "graphql-engine" ] || [ "$MODE" = "graphql-engine-pro" ]; then
  # Set the file descriptor limit up to the hard limit.  The common default of
  # 1024 is too low to really properly test subscriptions for instance.
  # It might be best just to do this in the engines:
  # https://hackage.haskell.org/package/unix-2.8.1.1/docs/System-Posix-Resource.html
  ulimit -Sn unlimited

  cd "$PROJECT_ROOT"
  # Existing tix files for a different hge binary will cause issues:
  rm -f "$EDITION_NAME.tix"

  # Attempt to run this after a CTRL-C:
  function cleanup {
    echo
    ### Run analysis or visualization tools, if we ran in one of the profiling modes
    case "${HASURA_PROF_MODE-}" in
        ticky)
          TICKY_FILENAME=$("$PROJECT_ROOT"/scripts/get-version.sh)-$(date +%s).ticky
          if [ -f "$EDITION_NAME.ticky" ]; then
              # Sort the main part of the profile by allocations and reassemble:
              TICKY_TEMPD=$(mktemp -d)
              awk -v TICKY_TEMPD="$TICKY_TEMPD" \
                  '/-------------[-]+$|\*\*\*\*\*[*]+$/{n++}{print >TICKY_TEMPD "/" "x" n }' \
                  "$EDITION_NAME.ticky"
              ticky_tmp=$(mktemp hasura_devsh_ticky.tmp.XXXXXXX)
              {
              cat "$TICKY_TEMPD/x" "$TICKY_TEMPD/x1"        ;
              head -n1   "$TICKY_TEMPD/x2"                  ;
              # This is the main section we care about, with allocation counts by name:
              tail -n +2 "$TICKY_TEMPD/x2" | sort -k2 -r -n | tee "$ticky_tmp";
              cat        "$TICKY_TEMPD/x3"
              } >> "$TICKY_FILENAME"

              # Make sure we didn't screw anything up, e.g. if ticky format changes:
              TICKY_FILENAME_sz=$(wc -c <"$TICKY_FILENAME")
              wc_c=$(wc -c <"$EDITION_NAME.ticky")
              if [ "$TICKY_FILENAME_sz" -ne "$wc_c" ]; then
                  echo_error "Erm... seems our processing of ticky file has a bug. Please fix me"
              fi
              rm -r "$TICKY_TEMPD"

              echo_warn "Done. View the ticky report at:  $TICKY_FILENAME"
              echo_warn "See: https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#using-ticky-ticky-profiling-for-implementors"
              echo_warn "Lookup referenced STG names dumped to their respective module files:  dist-newstyle/**/*.dump-stg-final"
              
              ### Do some additional analysis:
              # Extract module names, along with allocation counts
              ticky_tmp2=$(mktemp hasura_devsh_ticky2.tmp.XXXXXXX)
              if command -v rg >/dev/null ; then
                rg -o ' +[0-9]+ +([0-9]+).*(   | \()([A-Z][a-zA-Z]*(\.[A-Z][A-Za-z]*)*)' -r '$1  $3' \
                    "$ticky_tmp" > "$ticky_tmp2"
                awk '{sum[$2]+=$1} END {for (val in sum) printf "%'"'"'20d\t%s\n", sum[val], val }' "$ticky_tmp2" \
                    | sort -nr -k1 \
                    > "$TICKY_FILENAME.modules"
                echo
                echo_warn "Here are the top modules by allocation (see $TICKY_FILENAME.modules for all):"
                head -n5 "$TICKY_FILENAME.modules"
                echo
              else
                echo_error "Please install ripgrep (rg) to get per-module allocation summary"
              fi
              # NOTE: this should equal the sum of allocations in all entries
              # in the list and we find it does within ~1% for e.g. a benchmark
              # workload, but it's not clear why it doesn't exactly match:
              instrumented_bytes_allocated=$(grep ALLOC_HEAP_tot "$EDITION_NAME.ticky" | awk '{print $1}')
              echo_warn "There were..." 
              printf "%'20d\n" "$instrumented_bytes_allocated" 
              echo_warn "...bytes allocated from instrumented code in the profile."
              echo_warn "Compare this to the \"bytes allocated in the heap\" reported from the"
              echo_warn "'+RTS -s' above to see how many allocations aren't visible due to dependencies"
              echo_warn "not being instrumented (TODO --prof-ticky-all mode, maybe)"

              rm "$ticky_tmp" "$ticky_tmp2" "$EDITION_NAME.ticky"
          else
              echo_error "Hmmm. $EDITION_NAME.ticky wasn't generated for some reason..."
          fi
        ;;
        heap-infomap)
          if command -v eventlog2html >/dev/null ; then
            echo_warn "Running eventlog2html against the event log we just generated:  $EDITION_NAME.eventlog"
            eventlog2html --bands 100 "$EDITION_NAME.eventlog"
            echo_warn "Done. View the report at: $EDITION_NAME.eventlog.html"
            echo_warn "Lookup referenced STG names dumped to their respective module files:  dist-newstyle/**/*.dump-stg-final"
          else
            echo_warn "Please install eventlog2html"
          fi
        ;;
        ghc-debug)
            # TODO maybe integrate snapshotting + common analysis here
        ;;
        time)
          if command -v profiterole >/dev/null ; then
            if [ -f "$EDITION_NAME.prof" ]; then
                echo_warn "Running profiterole..."
                profiterole "$EDITION_NAME.prof"
                echo_warn "Done. Check out..."
                echo_warn "  - $EDITION_NAME.prof              ...for the top-down report"
                echo_warn "  - $EDITION_NAME.profiterole.html  ...for the folded report"
                echo_warn "Lookup referenced STG names dumped to their respective module files:  dist-newstyle/**/*.dump-stg-final"
            else
                echo_error "No $EDITION_NAME.prof was created... :("
            fi
          else
            echo_warn "You may wish to install profiterole"
          fi
        ;;
        "")
        ;;
        *)
        echo_error "Bug!: HASURA_PROF_MODE = $HASURA_PROF_MODE"
        exit 1
        ;;
    esac

    ### Generate coverage, which can be useful for debugging or understanding
    if command -v hpc >/dev/null && command -v jq >/dev/null ; then
      # FIXME: this was broken some time ago
      # Get the appropriate mix dir (the newest one); this way this hopefully
      # works when 'cabal/dev-sh.project.local' is edited to turn on
      # optimizations.
      #
      # See also: https://hackage.haskell.org/package/cabal-plan
      distdir=$(jq -r '."install-plan"[] | select(."id" == "graphql-engine-1.0.0-inplace")? | ."dist-dir"' dist-newstyle/cache/plan.json)
      hpcdir="$distdir/hpc/dyn/mix/graphql-engine-1.0.0"
      echo_pretty "Generating code coverage report..."
      COVERAGE_DIR="dist-newstyle/dev.sh-coverage"
      hpc_invocation=(hpc markup
        --exclude=Main
        --hpcdir "$hpcdir"
        --reset-hpcdirs graphql-engine.tix
        --fun-entry-count
        --destdir="$COVERAGE_DIR")
      "${hpc_invocation[@]}" >/dev/null

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

  export HASURA_GRAPHQL_DATABASE_URL=${HASURA_GRAPHQL_DATABASE_URL-$PG_DB_URL}
  export HASURA_GRAPHQL_SERVER_PORT=${HASURA_GRAPHQL_SERVER_PORT-8181}
  # Add 'developer' to the default list, for more visiblility:
  export HASURA_GRAPHQL_ENABLED_APIS=metadata,graphql,pgdump,config,developer,metrics

  echo_pretty "We will connect to postgres at '$HASURA_GRAPHQL_DATABASE_URL'"
  echo_pretty "If you haven't overridden HASURA_GRAPHQL_DATABASE_URL, you can"
  echo_pretty "launch a fresh postgres container for us to connect to, in a"
  echo_pretty "separate terminal with:"
  echo_pretty "    $ $0 postgres"
  echo_pretty ""

  RUN_INVOCATION=(cabal new-run --project-file="$CABAL_PROJECT_FILE" --RTS --
    "exe:$EDITION_NAME" +RTS -N -T -s -RTS serve
    --enable-console --console-assets-dir "$PROJECT_ROOT/frontend/dist/apps/server-assets-console-$EDITION_ABBREV"
    "${GRAPHQL_ENGINE_EXTRA_ARGS[@]}"
    )

  echo_pretty 'About to do:'
  echo_pretty "    $ cabal new-build --project-file=$CABAL_PROJECT_FILE exe:$EDITION_NAME"
  echo_pretty "    $ ${RUN_INVOCATION[*]}"
  echo_pretty ''

  cabal new-build --project-file="$CABAL_PROJECT_FILE" "exe:$EDITION_NAME"

  # We assume a PG is *already running*, and therefore bypass the
  # cleanup mechanism previously set.
  pg_wait

  # Print helpful info after startup logs so it's visible:
  {
    until curl -s "http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/v1/query" &>/dev/null; do
      sleep 0.2
    done
    sleep 1
    echo_pretty "▲▲▲ $EDITION_NAME startup logs above ▲▲▲"
    echo_pretty ""
    echo_pretty "You can set additional environment vars to tailor '$EDITION_NAME' next time you"
    echo_pretty "invoke this script, e.g.:"
    echo_pretty "    # Keep polling statements out of logs"
    echo_pretty "    HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL=3000000"
    echo_pretty ""
    echo_pretty "The hasura console is available at:"
    echo_pretty "    http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/console"
    echo_pretty ""
    echo_pretty "  If the console was modified since your last build (re)build assets with:"
    echo_pretty "      $ cd \"$PROJECT_ROOT/frontend\""
    echo_pretty "      $ yarn install && yarn server-build:$EDITION_ABBREV"
    echo_pretty ""
    echo_pretty "Useful endpoints when compiling with '$EDITION_NAME:developer' and running with '+RTS -T'"
    echo_pretty "   http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/dev/subscriptions/extended"
    echo_pretty "   http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/dev/plan_cache"
    echo_pretty ""
    echo_pretty "To view realtime GC stats and other info open in your browser:"
    echo_pretty "    file://$PROJECT_ROOT/scripts/ekg/ekg.html#$HASURA_GRAPHQL_SERVER_PORT"
    echo_pretty ""
    if [ "$EDITION_NAME" = "graphql-engine-pro" ]; then
    echo_pretty "If you want to observe traces, you can run jaeger all-in-oner:"
    echo_pretty "    $ docker run -d --name jaeger -e COLLECTOR_ZIPKIN_HOST_PORT=:9411 -e COLLECTOR_OTLP_ENABLED=true -p 6831:6831/udp -p 6832:6832/udp -p 5778:5778 -p 16686:16686 -p 4317:4317 -p 4318:4318 -p 14250:14250 -p 14268:14268 -p 14269:14269 -p 9411:9411 jaegertracing/all-in-one:1.44"
    echo_pretty "...then configure http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/console/settings/opentelemetry"
    echo_pretty "...setting 'Endpoint' to: http://localhost:4318/v1/traces"
    fi
    echo_pretty "▼▼▼ additional $EDITION_NAME logs will appear below: ▼▼▼"
  } &

  # Logs printed until CTRL-C:
  "${RUN_INVOCATION[@]}" | try_jq
  exit 0
  ### END SCRIPT ###



#################################
###    Postgres container     ###
#################################

elif [ "$MODE" = "postgres" ]; then
  pg_start
  echo_pretty "Postgres logs will start to show up in realtime here. Press CTRL-C to exit and "
  echo_pretty "shutdown this container."
  echo_pretty ""
  echo_pretty "You can use the following to connect to the running instance:"
  echo_pretty "    $ $PG_DOCKER"
  echo_pretty "        or..."
  echo_pretty "    $ PGPASSWORD=$PG_PASSWORD psql -h 127.0.0.1 -p $PG_PORT postgres -U postgres"
  echo_pretty ""
  echo_pretty "Here is the database URL:"
  echo_pretty "    $PG_DB_URL"
  echo_pretty ""
  echo_pretty "If you want to launch a 'graphql-engine' that works with this database:"
  echo_pretty "    $ $0 graphql-engine  # or graphql-engine-pro"
  docker logs -f --tail=0 "$PG_CONTAINER_NAME"


#################################
###      MSSQL Container      ###
#################################

elif [ "$MODE" = "mssql" ]; then
  mssql_start
  echo_pretty "MSSQL logs will start to show up in realtime here. Press CTRL-C to exit and "
  echo_pretty "shutdown this container."
  echo_pretty ""
  echo_pretty "Here is the database URL:"
  echo_pretty "    $MSSQL_CONN_STR"
  echo_pretty ""
  docker logs -f --tail=0 "$MSSQL_CONTAINER_NAME"


#################################
###      Citus Container      ###
#################################

elif [ "$MODE" = "citus" ]; then
  citus_start
  echo_pretty "CITUS logs will start to show up in realtime here. Press CTRL-C to exit and "
  echo_pretty "shutdown this container."
  echo_pretty ""
  echo_pretty "You can use the following to connect to the running instance:"
  echo_pretty "    $ $CITUS_DOCKER"
  echo_pretty ""
  echo_pretty "Here is the database URL:"
  echo_pretty "    $CITUS_DB_URL"
  echo_pretty ""
  docker logs -f --tail=0 "$CITUS_CONTAINER_NAME"

#################################
###      MySQL Container      ###
#################################

elif [ "$MODE" = "mysql" ]; then
  mysql_start
  echo_pretty "MYSQL logs will start to show up in realtime here. Press CTRL-C to exit and "
  echo_pretty "shutdown this container."
  echo_pretty ""
  echo_pretty "You can use the following to connect to the running instance:"
  echo_pretty "    $ $MYSQL_DOCKER"
  echo_pretty ""
  echo_pretty "If you want to import a SQL file into MYSQL:"
  echo_pretty "    $ $MYSQL_DOCKER -i <import_file>"
  echo_pretty ""
  docker logs -f --tail=0 "$MYSQL_CONTAINER_NAME"


elif [ "$MODE" = "test" ]; then
  ########################################
  ###     Integration / unit tests     ###
  ########################################
  cd "$PROJECT_ROOT"

  # Until we can use a real webserver for TestEventFlood, limit concurrency
  export HASURA_GRAPHQL_EVENTS_HTTP_POOL_SIZE=8

  # We'll get an hpc error if these exist; they will be deleted below too:
  rm -f graphql-engine-tests.tix graphql-engine.tix graphql-engine-combined.tix

  # Various tests take some configuration from the environment; set these up here:
  export EVENT_WEBHOOK_HEADER="MyEnvValue"
  export EVENT_WEBHOOK_HANDLER="http://localhost:5592"
  export ACTION_WEBHOOK_HANDLER="http://localhost:5593"
  export SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN="http://localhost:5594"
  export REMOTE_SCHEMAS_WEBHOOK_DOMAIN="http://localhost:5000"
  export GRAPHQL_SERVICE_HANDLER="http://localhost:4001"
  export GRAPHQL_SERVICE_1="http://localhost:4020"
  export GRAPHQL_SERVICE_2="http://localhost:4021"
  export GRAPHQL_SERVICE_3="http://localhost:4022"

  if [ "$RUN_INTEGRATION_TESTS" = true ]; then
    # It's better UX to build first (possibly failing) before trying to launch
    # PG, but make sure that new-run uses the exact same build plan, else we risk
    # rebuilding twice... ugh
    # Formerly this was a `cabal build` but mixing cabal build and cabal run
    # seems to conflict now, causing re-linking, haddock runs, etc. Instead do a
    # `graphql-engine version` to trigger build
    cabal run \
      --project-file="$CABAL_PROJECT_FILE" \
      -- exe:graphql-engine \
        --metadata-database-url="$PG_DB_URL" \
        version
    start_dbs
  fi

  if [ "$RUN_UNIT_TESTS" = true ]; then
    echo_pretty "Running Haskell test suite"

    # unit tests need access to postgres and mssql instances:
    mssql_start
    pg_start

    echo "${UNIT_TEST_ARGS[@]}"
    HASURA_GRAPHQL_DATABASE_URL="$PG_DB_URL" \
      HASURA_MSSQL_CONN_STR="$MSSQL_CONN_STR" \
      cabal run \
        --project-file="$CABAL_PROJECT_FILE" \
        test:graphql-engine-tests \
        -- "${UNIT_TEST_ARGS[@]}"
  fi

  if [ "$RUN_HLINT" = true ]; then
    if command -v hlint >/dev/null; then
      hlint "${PROJECT_ROOT}/server/src-"*
    else
      echo_warn "hlint is not installed: skipping"
    fi
  fi

  if [ "$RUN_INTEGRATION_TESTS" = true ]; then
    GRAPHQL_ENGINE_TEST_LOG=/tmp/hasura-dev-test-engine.log
    echo_pretty "Starting graphql-engine, logging to $GRAPHQL_ENGINE_TEST_LOG"
    export HASURA_GRAPHQL_SERVER_PORT=8088

    # Extra sources for multi-source tests. Uses the default postgres DB if no extra sources
    # are defined.
    export HASURA_GRAPHQL_PG_SOURCE_URL_1=${HASURA_GRAPHQL_PG_SOURCE_URL_1-$PG_DB_URL}
    export HASURA_GRAPHQL_PG_SOURCE_URL_2=${HASURA_GRAPHQL_PG_SOURCE_URL_2-$PG_DB_URL}
    export HASURA_GRAPHQL_MSSQL_SOURCE_URL=$MSSQL_CONN_STR
    export HGE_URL="http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT"

    # Using --metadata-database-url flag to test multiple backends
    #       HASURA_GRAPHQL_PG_SOURCE_URL_* For a couple multi-source pytests:
    cabal new-run \
      --project-file="$CABAL_PROJECT_FILE" \
      -- exe:graphql-engine \
        --metadata-database-url="$PG_DB_URL" serve \
        --stringify-numeric-types \
        --enable-console \
        --console-assets-dir ../frontend/dist/apps/server-assets-console-ce \
      &> "$GRAPHQL_ENGINE_TEST_LOG" & GRAPHQL_ENGINE_PID=$!

    echo -n "Waiting for graphql-engine"
    until curl -s "http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT/v1/query" &>/dev/null; do
      echo -n '.' && sleep 0.2
      # If the server stopped abort immediately
      if ! kill -0 $GRAPHQL_ENGINE_PID ; then
        echo_error "The server crashed or failed to start!!"
        exit 42
      fi
    done

    echo " Ok"

    add_sources $HASURA_GRAPHQL_SERVER_PORT

    TEST_DIR="server/tests-py"

    # Install and load Python test dependencies
    PY_VENV="${TEST_DIR}/.hasura-dev-python-venv"
    make "$PY_VENV"
    source "${PY_VENV}/bin/activate"

    # Install node.js test dependencies
    make "${TEST_DIR}/node_modules"

    cd "$TEST_DIR"

    # TODO MAYBE: fix deprecation warnings, make them an error
    if ! pytest \
          --hge-urls http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT \
          --pg-urls "$PG_DB_URL" \
          --assert=plain \
          "${PYTEST_ARGS[@]}"
    then
      echo_error "^^^ graphql-engine logs from failed test run can be inspected at: $GRAPHQL_ENGINE_TEST_LOG"
    fi
    deactivate  # python venv

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
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/noopt/hpc/dyn/mix/graphql-engine-* \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/t/graphql-engine-tests/noopt/hpc/dyn/mix/graphql-engine-tests \
      --reset-hpcdirs graphql-engine-combined.tix \
      --fun-entry-count \
      --destdir="$COVERAGE_DIR" >/dev/null
    hpc report \
      --exclude=Main \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/noopt/hpc/dyn/mix/graphql-engine-* \
      --hpcdir dist-newstyle/build/*/ghc-*/graphql-engine-*/t/graphql-engine-tests/noopt/hpc/dyn/mix/graphql-engine-tests \
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
