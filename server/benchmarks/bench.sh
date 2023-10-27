#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

# NOTE: we want to use --network=host everywhere we use docker here, for
# performance (supposedly).  We include X:X port mappings which will be used on
# Mac and Windows(?) where --network=host is ignored.

echo_pretty() {
    echo ">>> $(tput setaf 2)$1$(tput sgr0)"
}

die_usage() {
cat <<EOL
-------------------------=========########========-------------------------

Run hasura benchmarks

Usage:
    $ $0 <benchmark_dir> [<hasura_docker_image>] [<sleep_time_sec_before_bench>]

The first argument chooses the particular benchmark set to run e.g. "chinook"
or "big_schema" (these correspond to directories under 'benchmark_sets/').

The second optional argument is the docker image name to test. e.g.
"hasura/graphql-engine:latest" If omitted we'll look for a hasura instance
launched with 'dev.sh graphql-engine'.

-------------------------=========########========-------------------------
EOL
exit 1
}

# Dependencies:
if ! command -v jq &> /dev/null
then
    echo "Please install 'jq'" >&2; exit 1
fi

{ [ -z "${1-}" ] || [ ! -d "benchmark_sets/${1-}" ]; } && die_usage
BENCH_DIR="$(pwd)/benchmark_sets/$1"
REQUESTED_HASURA_DOCKER_IMAGE="${2-}"
# We may wish to sleep after setting up the schema, etc. to e.g. allow memory
# to settle to a baseline before we measure it:
if [ -z "${3-}" ]; then
  POST_SETUP_SLEEP_TIME=0
else
  POST_SETUP_SLEEP_TIME="$3"
fi

# Make sure we clean up, even if something goes wrong:
function cleanup {
  if [ -n "${HASURA_CONTAINER_NAME-}" ]; then
    echo_pretty "Stopping and removing hasura container"
    docker stop "$HASURA_CONTAINER_NAME" && docker rm "$HASURA_CONTAINER_NAME" \
      || echo "Stopping hasura failed, maybe it never started?"
  fi
  pg_cleanup || echo "Stopping postgres failed, maybe it never started?"

  custom_cleanup || echo "Custom cleanup failed"
}
trap cleanup EXIT

# How can we communicate with localhost from a container?
if [ "$(uname -s)" = Darwin ]; then
  # Docker for mac:
  LOCALHOST_FROM_CONTAINER=host.docker.internal
  DOCKER_NETWORK_HOST_MODE=""
else
  LOCALHOST_FROM_CONTAINER=127.0.0.1
  DOCKER_NETWORK_HOST_MODE="--network=host"
fi

# The beefy c4.8xlarge EC2 instance has two sockets, so we'll try our best to
# pin hasura on one and postgres on the other
if taskset -c 17 sleep 0 ; then
  echo_pretty "CPUs? Running on a beefy CI machine"
  TASKSET_HASURA="taskset -c 0-6" # SOCKET/NUMA_NODE 0
  TASKSET_K6="taskset -c 7,8"     # SOCKET/NUMA_NODE 0
  TASKSET_PG="taskset -c 9-17"    # SOCKET/NUMA_NODE 1
  HASURA_RTS="-qa -N7"
  # This is a sort of hack to, on CI (where circleci doesn't handle control
  # characters properly), force K6 to not print progress bar updates. But note
  # that the fabric script that calls this script will also fail if this is not
  # run with a pTTY...
  # This is still not great because K6 spits out progress every second or so.
  # TODO maybe combine this stuff into a single are-we-on-ci check / env var
  K6_DOCKER_tty_OR_init="--init"
else
  echo_pretty "CPUs? Running on a puny local machine"
  TASKSET_HASURA=""
  TASKSET_PG=""
  TASKSET_K6=""
  HASURA_RTS=""

  K6_DOCKER_tty_OR_init="--tty"
fi

##################
#   Postgres     #
##################
# FYI this is adapted from scripts/containers/postgres, and uses settings
# (ports, passwords, etc) identical to `dev.sh postgres` for compatibility
# with `dev.sh graphql-engine`

PG_PORT=25432
PG_PASSWORD=postgres
PG_CONTAINER_NAME="hasura-dev-postgres-$PG_PORT"
PG_DB_URL="postgres://postgres:$PG_PASSWORD@$LOCALHOST_FROM_CONTAINER:$PG_PORT/postgres"
PSQL_DOCKER="docker exec -u postgres -i $PG_CONTAINER_NAME psql $PG_DB_URL"

if [ "$(awk '/^MemTotal:/{print $2}' /proc/meminfo)" -ge "30000000" ]; then
echo_pretty "RAM? Running on a beefy CI machine"
# These are the suggested values from https://pgtune.leopard.in.ua/#/
# using the parameters of c4.8xlarge, divided by two (since hasura is running
# on the same instance): 9 cores and 30GB RAM, as "web application".
#
# NOTE: no spaces here or this will break
CONF=$(cat <<-EOF
shared_buffers=7680MB
effective_cache_size=23040MB
maintenance_work_mem=1920MB
checkpoint_completion_target=0.9
wal_buffers=16MB
default_statistics_target=100
random_page_cost=1.1
effective_io_concurrency=200
work_mem=19660kB
min_wal_size=1GB
max_wal_size=4GB
max_worker_processes=9
max_parallel_workers_per_gather=4
max_parallel_workers=9
max_parallel_maintenance_workers=4
port=$PG_PORT
EOF
)

# otherwise just use a configuration assuming 8GB RAM local dev machine:
else
echo_pretty "RAM? Running on a puny local machine"
CONF=$(cat <<-EOF
max_connections=50
shared_buffers=1GB
effective_cache_size=3GB
maintenance_work_mem=256MB
checkpoint_completion_target=0.9
wal_buffers=16MB
default_statistics_target=100
random_page_cost=1.1
effective_io_concurrency=200
work_mem=20971kB
min_wal_size=1GB
max_wal_size=4GB
max_worker_processes=2
max_parallel_workers_per_gather=1
max_parallel_workers=2
max_parallel_maintenance_workers=1
port=$PG_PORT
EOF
)
fi

# log lines above as -c flag arguments we pass to postgres
CONF_FLAGS=$(echo "$CONF" | sed  -e 's/^/-c /'  | tr '\n' ' ')

# NOTE: after some consideration we decided to serve postgres from ramdisk
# here. A few reasons:
#
# - EBS is incredibly finicky and difficult to provision correctly[1]; we
#   could easily add a new benchmark which exhausts our IOPS and causes
#   confusing regression-like results
# - SQL-gen regressions should still show up as regressions if we're backed by
#   tmpfs; only perhaps the magnitidue would change. We also expected PG to be
#   doing significant in-memory caching on the small datasets here.
# - There is some evidence[2] that ramdisk is actually a decent approximation of
#   the performance of a perfectly-tuned durable PG instance (i.e. the latency
#   numbers we get here are useful in absolute terms as well, representing ideal
#   performance)
#
# [1]: https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSPerformance.html
# [2]: https://performance.sunlight.io/postgres/
function pg_launch_container(){
  echo_pretty "Launching postgres container: $PG_CONTAINER_NAME"
  # `TASKSET_PG`, `DOCKER_HOST_MODE`, and `CONF_FLAGS` depend on
  # word-splitting.
  #
  # cf. https://github.com/koalaman/shellcheck/wiki/Sc2086
  #
  # shellcheck disable=SC2086
  $TASKSET_PG docker run \
    --detach \
    --name "$PG_CONTAINER_NAME" \
    --mount type=tmpfs,destination=/var/lib/postgresql/data \
    --publish 127.0.0.1:"$PG_PORT":"$PG_PORT" \
    --expose="$PG_PORT" \
    --env POSTGRES_PASSWORD="$PG_PASSWORD" \
    $DOCKER_NETWORK_HOST_MODE \
     circleci/postgres:11.5-alpine-postgis \
    $CONF_FLAGS
}

function pg_wait() {
  echo -n "Waiting for postgres to come up"
  until ( $PSQL_DOCKER -c '\l' ) &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo " Ok"
}

function pg_cleanup(){
  echo_pretty "Removing $PG_CONTAINER_NAME and its volumes"
  docker stop "$PG_CONTAINER_NAME"
  docker rm --volumes "$PG_CONTAINER_NAME"
}

######################
#   graphql-engine   #
######################

# This matches the default we use in `dev.sh graphql-engine`
HASURA_GRAPHQL_SERVER_PORT=8181
# For Mac compatibility, we need to use this URL for hasura when communicating
# FROM a container (in this case graphql-bench):
HASURA_URL_FROM_CONTAINER="http://$LOCALHOST_FROM_CONTAINER:$HASURA_GRAPHQL_SERVER_PORT"
# ...and for anything outside a container, just:
export HASURA_URL="http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT"

# Maybe launch the hasura instance we'll benchmark
function maybe_launch_hasura_container() {
  if [ -n "$REQUESTED_HASURA_DOCKER_IMAGE" ]; then
    # NOTE!: presence of HASURA_GRAPHQL_EE_LICENSE_KEY will determine if we run
    # in EE mode or not, and this will happen silently! version doesn't indicate
    # the mode we're in.
    if [ -z "${HASURA_GRAPHQL_EE_LICENSE_KEY}" ]; then
        echo_pretty "Running in OSS mode"
    else
        echo_pretty "Running in EE mode"
        # consistent across scripts/dev.sh and server/benchmarks:
        export HASURA_GRAPHQL_ADMIN_SECRET=my-secret
    fi
    HASURA_CONTAINER_NAME="graphql-engine-to-benchmark"
    # `TASKSET_HASURA`, `$DOCKER_NETWORK_HOST_MODE`, and `HASURA_RTS` depend on
    # word-splitting.
    #
    # cf. https://github.com/koalaman/shellcheck/wiki/Sc2086
    #
    # shellcheck disable=SC2086
    $TASKSET_HASURA docker run \
      --detach \
      --name "$HASURA_CONTAINER_NAME" \
      --publish 127.0.0.1:"$HASURA_GRAPHQL_SERVER_PORT":"$HASURA_GRAPHQL_SERVER_PORT" \
      --env HASURA_GRAPHQL_DATABASE_URL="$PG_DB_URL" \
      --env HASURA_GRAPHQL_ENABLE_CONSOLE=true \
      --env HASURA_GRAPHQL_SERVER_PORT="$HASURA_GRAPHQL_SERVER_PORT" \
      --env HASURA_GRAPHQL_ADMIN_SECRET \
      --env HASURA_GRAPHQL_EE_LICENSE_KEY \
      $DOCKER_NETWORK_HOST_MODE \
      "$REQUESTED_HASURA_DOCKER_IMAGE" \
      graphql-engine serve \
        +RTS -T $HASURA_RTS -RTS
      # ^^^ We run with `+RTS -T` to expose the /dev/rts_stats endpoint for
      #     inspecting memory usage stats
  else
    echo_pretty "We'll benchmark the hasura instance at port $HASURA_GRAPHQL_SERVER_PORT"
  fi
}

function hasura_wait() {
  # Wait for the graphql-engine under bench to be ready
  wait_time=120
  echo -n "Waiting for graphql-engine at $HASURA_URL for $wait_time seconds"
  if [ -z "$REQUESTED_HASURA_DOCKER_IMAGE" ]; then
    echo -n " (e.g. from 'dev.sh graphql-engine')"
  fi
  start_time="$(date +%s)"
  until curl -s "$HASURA_URL/v1/query" &>/dev/null; do
    if [[ $(( "$(date +%s)" - start_time )) -gt "$wait_time" ]]; then
      echo
      echo 'Timed out.'
      if [ -n "$HASURA_CONTAINER_NAME" ]; then
        echo 'Container logs:'
        docker logs -f "$HASURA_CONTAINER_NAME" 2>&1 &
        docker stop "$HASURA_CONTAINER_NAME" || : >/dev/null
      fi
      return 1
    fi
    echo -n '.' && sleep 0.2
  done
  echo
  echo ' Ok'
  echo -n "Sleeping for an additional $POST_SETUP_SLEEP_TIME seconds as requested... "
  sleep "$POST_SETUP_SLEEP_TIME"
  echo ' Ok'
}

#####################
#   graphql-bench   #
#####################

# We want to always use the latest graphql-bench. Installing is idempotent and
# fairly speedy the second time, if no changes.
function install_latest_graphql_bench() {
  echo_pretty "Installing/updating graphql-bench"
  graphql_bench_git=$(mktemp -d -t graphql-bench-XXXXXXXXXX)
  git clone --depth=1 https://github.com/hasura/graphql-bench.git "$graphql_bench_git"

  cd "$graphql_bench_git"
  # We name this 'graphql-bench-ci' so it doesn't interfere with other versions
  # (e.g. local dev of `graphql-bench`, installed with `make
  # build_local_docker_image`:
  docker build -t graphql-bench-ci:latest ./app
  cd -
  echo_pretty "Done"
}

# graphql-bench -powered benchmarks
function run_graphql_benchmarks() {
  echo_pretty "Starting graphql benchmarks"

  cd "$BENCH_DIR"
  # This reads config.query.yaml from the current directory, outputting
  # report.json to the same directory
  $TASKSET_K6 docker run \
    --interactive \
    --volume "$PWD":/app/tmp \
    $DOCKER_NETWORK_HOST_MODE \
    $K6_DOCKER_tty_OR_init \
    graphql-bench-ci query \
      --config="./tmp/config.query.yaml" \
      --outfile="./tmp/report.json" \
      --url "$HASURA_URL_FROM_CONTAINER/v1/graphql"

  echo_pretty "Done. Report at $PWD/report.json"
  cd -
}

# adhoc script-based benchmarks (more flexible, but less reporting).
function run_adhoc_operation_benchmarks() (
  cd "$BENCH_DIR"
  # Filenames should look like <operation_name>.sh
  reg=".*\.sh"
  # Collect any adhoc operations, else skip silently
  scripts=$(find "adhoc_operations" -maxdepth 1 -executable -type f -regextype sed -regex "$reg" 2>/dev/null ) || return 0
  [ -z "$scripts" ] && return 0

  echo_pretty "Running adhoc operations for $BENCH_DIR..."

  # sanity check:
  if [ ! -f report.json ]; then
      echo "Bug: Somehow a report from graphql-engine isn't present! Exiting"
      exit 43
  fi

  # NOTE: This loops over each word in `$scripts` with globbing.
  for script in $scripts; do
      # The script must define a function named "adhoc_operation" which we
      # execute multiple times below. This gives more flexibility and is maybe
      # faster. It also must define 'iterations' indicating the number of
      # iterations to execute here.
      unset -f adhoc_operation
      unset iterations

      # shellcheck disable=SC1090
      . "$script"
      if [[ $(type -t adhoc_operation) != function ]]; then
          echo "Error: $script must define a function named 'adhoc_operation'! Exiting." >&2; exit 1
      fi

      # shellcheck disable=SC2154
      if ! [[ "$iterations" =~ ^[0-9]+$ ]] ; then
          echo "Error: $script must define 'iterations'" >&2; exit 1
      fi
      # shellcheck disable=SC2154
      if ! [[ "$pause_after_seconds" =~ ^[0-9]+$ ]] ; then
          echo "Error: $script must define 'pause_after_seconds'" >&2; exit 1
      fi

      # TODO I was relying on being able to also get 'mutator_cpu_ns' to get a
      # stable metric of CPU usage (like counting instructions with 'perf').
      # Unfortunately that metric is fubar
      # (https://gitlab.haskell.org/ghc/ghc/-/issues/21082). Trying to use perf
      # will be a pain, might require root, means this script won't work over a
      # network, etc...
      live_bytes_before=$(curl "$HASURA_URL/dev/rts_stats" 2>/dev/null | jq '.gc.gcdetails_live_bytes')
      mem_in_use_bytes_before=$(curl "$HASURA_URL/dev/rts_stats" 2>/dev/null | jq '.gc.gcdetails_mem_in_use_bytes')
      allocated_bytes_before=$(curl "$HASURA_URL/dev/rts_stats" 2>/dev/null | jq '.allocated_bytes')
 
      IFS="./" read -r -a script_name_parts <<< "$script"
      name=${script_name_parts[1]}

      # Space separated list of nanosecond timings
      latencies_ns=""
      # Select an item from latencies_ns at the percentile requested, this
      # never takes the mean of two samples.
      rough_percentile() {
          echo "$latencies_ns" |\
              tr ' ' '\n' | sort -n |\
              awk '{all[NR] = $0} END{print all[int(NR*'"$1"'+0.5)]}' 
              # ^^^ select a value based on line number, rounding up
      }

      echo -n "Running $name $iterations time(s)..."
      for _ in $(seq 1 "$iterations"); do
         echo -n "."
         # NOTE: Executing a command like `date` takes about 1 ms in Bash, so
         # we can't really accurately time operations that are on the order of
         # single-digit ms
         time_ns_before=$(date +%s%N)
         adhoc_operation &> /tmp/hasura_bench_adhoc_last_iteration.out
         time_ns_after=$(date +%s%N)
         latencies_ns+=" $((time_ns_after-time_ns_before))"
      done
      echo

      # Allowing any asynchronous activity to settle:
      sleep "$pause_after_seconds"

      allocated_bytes_after=$(curl "$HASURA_URL/dev/rts_stats" 2>/dev/null | jq '.allocated_bytes')
      mem_in_use_bytes_after=$(curl "$HASURA_URL/dev/rts_stats" 2>/dev/null | jq '.gc.gcdetails_mem_in_use_bytes')
      live_bytes_after=$(curl "$HASURA_URL/dev/rts_stats" 2>/dev/null | jq '.gc.gcdetails_live_bytes')

      mean_ms=$(echo "$latencies_ns" | jq -s '(add/length)/1000000')
      min_ms=$(echo "$latencies_ns"  | jq -s 'min/1000000')
      max_ms=$(echo "$latencies_ns"  | jq -s 'max/1000000')
      p50_ms=$(rough_percentile 0.50 | jq -s 'min/1000000')
      p90_ms=$(rough_percentile 0.90 | jq -s 'min/1000000')
      bytes_allocated_per_request=$(jq -n \("$allocated_bytes_after"-"$allocated_bytes_before"\)/"$iterations")

      echo " Done. For $name, measured $mean_ms ms/op and $bytes_allocated_per_request bytes_allocated/op"
      echo "Appending to $PWD/report.json..."
      # NOTE: certain metrics that are difficult to collect or that we don't
      # care about are set to 0 here, to prevent the graphql-bench web app from
      # breaking 
      #
      # Name these "ADHOC-foo" so we can choose to display them differently,
      # e.g. in PR regression reports
      temp_result_file="$(mktemp)"  # ('sponge' unavailable)
      jq --arg name "$name" \
         --argjson iterations "$iterations" \
         --argjson p50_ms "$p50_ms" \
         --argjson p90_ms "$p90_ms" \
         --argjson max_ms "$max_ms" \
         --argjson mean_ms "$mean_ms" \
         --argjson min_ms "$min_ms" \
         --argjson bytes_allocated_per_request "$bytes_allocated_per_request" \
         --argjson live_bytes_before "$live_bytes_before" \
         --argjson live_bytes_after "$live_bytes_after" \
         --argjson mem_in_use_bytes_before "$mem_in_use_bytes_before" \
         --argjson mem_in_use_bytes_after "$mem_in_use_bytes_after" \
         '. +
          [{
            "name": "ADHOC-\($name)",
            "time": {
            },
            "requests": {
              "count": $iterations,
              "average": 0
            },
            "response": {
              "totalBytes": 0,
              "bytesPerSecond": 0
            },
            "histogram": {
              "json": {
                "p50": $p50_ms,
                "p90": $p90_ms,
                "max": $max_ms,
                "totalCount": $iterations,
                "mean": $mean_ms,
                "min": $min_ms,
                "stdDeviation": 0
              },
              "parsedStats": [
              ]
            },
            "extended_hasura_checks": {
              "bytes_allocated_per_request": $bytes_allocated_per_request,
              "live_bytes_before": $live_bytes_before,
              "live_bytes_after": $live_bytes_after,
              "mem_in_use_bytes_before": $mem_in_use_bytes_before,
              "mem_in_use_bytes_after": $mem_in_use_bytes_after
            }
          }]' < report.json > "$temp_result_file"
      mv -f "$temp_result_file" report.json

      # TODO once GHC issue 21082 fixed:
      #  - collect mutator cpu ns
      #  - add an untrack/track table benchmark to chinook and huge_schema
  done
)

function custom_setup() {
  cd "$BENCH_DIR"
  if [ -x setup.sh ]; then
    echo_pretty "Running custom setup script"
    ./setup.sh
  fi

  cd -
}

function custom_cleanup() {
  cd "$BENCH_DIR"
  if [ -x cleanup.sh ]; then
    echo_pretty "Running custom cleanup script"
    ./cleanup.sh
  fi

  cd -
}

function load_data_and_schema() {
  echo_pretty "Loading data and adding schema"
  cd "$BENCH_DIR"
  if [ -f dump.sql.gz ]; then
    gunzip -c dump.sql.gz | $PSQL_DOCKER &> /dev/null
  else
    echo_pretty "No data to load"
  fi

  if [ -f replace_metadata.json ]; then
    # --fail-with-body is what we want, but is not available on older curl:
    # TODO LATER: use /v1/metadata once stable
    curl \
      --fail \
      --request POST \
      --header "Content-Type: application/json" \
      --header "X-Hasura-Admin-Secret: my-secret" \
      --data @replace_metadata.json \
      "$HASURA_URL/v1/query"
  else
    echo_pretty "No metadata to replace"
  fi

  cd -
}

##################################
#   bringing it all together...  #
##################################

# Start this ahead of time...
pg_launch_container
# meanwhile...
install_latest_graphql_bench

# Wait for pg, then bring up hasura if needed
pg_wait
maybe_launch_hasura_container
hasura_wait

custom_setup

load_data_and_schema
run_graphql_benchmarks
run_adhoc_operation_benchmarks

echo_pretty "All done. You can visualize the report using the web app here:"
echo_pretty "    https://hasura.github.io/graphql-bench/app/web-app/"
echo_pretty "...and passing it the file we just generated:"
echo_pretty "    $BENCH_DIR/report.json"
