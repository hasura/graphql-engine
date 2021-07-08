#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar


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

[ ! -d "benchmark_sets/${1-}" ] && die_usage
BENCH_DIR="benchmark_sets/$1"
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
  if [ ! -z "${HASURA_CONTAINER_NAME-}" ]; then
    echo_pretty "Stopping and removing hasura container"
    docker stop "$HASURA_CONTAINER_NAME" && docker rm "$HASURA_CONTAINER_NAME" \
      || echo "Stopping hasura failed, maybe it never started?"
  fi
  pg_cleanup || echo "Stopping postgres failed, maybe it never started?"
}
trap cleanup EXIT

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
  K6_DOCKER_t_OR_init="--init"
else
  echo_pretty "CPUs? Running on a puny local machine"
  TASKSET_HASURA=""
  TASKSET_PG=""
  TASKSET_K6=""
  HASURA_RTS=""

  K6_DOCKER_t_OR_init="-t"
fi

##################
#   Postgres     #
##################
# FYI this is adapted from scripts/containers/postgres, and uses settings
# (ports, passwords, etc) identical to `dev.sh postgres`


PG_PORT=25430
PG_PASSWORD=postgres
PG_CONTAINER_NAME="hasura-benchmarks-postgres-$PG_PORT"
PG_DB_URL="postgres://postgres:$PG_PASSWORD@127.0.0.1:$PG_PORT/postgres"
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
  $TASKSET_PG docker run \
    --mount type=tmpfs,destination=/var/lib/postgresql/data \
    --name "$PG_CONTAINER_NAME" \
    -p 127.0.0.1:"$PG_PORT":$PG_PORT \
    --expose="$PG_PORT" \
    -e POSTGRES_PASSWORD="$PG_PASSWORD" \
    -d circleci/postgres:11.5-alpine-postgis \
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
  docker rm -v "$PG_CONTAINER_NAME"
}

######################
#   graphql-engine   #
######################
  
# This matches the default we use in `dev.sh graphql-engine`
HASURA_GRAPHQL_SERVER_PORT=8181
HASURA_URL="http://127.0.0.1:$HASURA_GRAPHQL_SERVER_PORT"

# Maybe launch the hasura instance we'll benchmark
function maybe_launch_hasura_container() {
  if [ ! -z "$REQUESTED_HASURA_DOCKER_IMAGE" ]; then
    HASURA_CONTAINER_NAME="graphql-engine-to-benchmark"
    $TASKSET_HASURA docker run -d -p $HASURA_GRAPHQL_SERVER_PORT:$HASURA_GRAPHQL_SERVER_PORT \
      --name "$HASURA_CONTAINER_NAME" \
      -e HASURA_GRAPHQL_DATABASE_URL=$PG_DB_URL \
      -e HASURA_GRAPHQL_ENABLE_CONSOLE=true \
      -e HASURA_GRAPHQL_SERVER_PORT="$HASURA_GRAPHQL_SERVER_PORT" \
      --network=host \
      "$REQUESTED_HASURA_DOCKER_IMAGE" \
      graphql-engine serve +RTS -T $HASURA_RTS -RTS
      # ^^^ We run with `+RTS -T` to expose the /dev/rts_stats endpoint for
      #     inspecting memory usage stats
  else
    echo_pretty "We'll benchmark the hasura instance at port $HASURA_GRAPHQL_SERVER_PORT"
  fi
}

function hasura_wait() {
  # Wait for the graphql-engine under bench to be ready
  echo -n "Waiting for graphql-engine"
  until curl -s "$HASURA_URL/v1/query" &>/dev/null; do
    echo -n '.' && sleep 0.2
  done
  echo ""
  echo " Ok"
  echo -n "Sleeping for an additional $POST_SETUP_SLEEP_TIME seconds as requested... "
  sleep "$POST_SETUP_SLEEP_TIME"
  echo " Ok"
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

function run_benchmarks() {
  echo_pretty "Starting benchmarks"

  cd "$BENCH_DIR"
  # This reads config.query.yaml from the current directory, outputting
  # report.json to the same directory
  $TASKSET_K6 docker run --net=host -v "$PWD":/app/tmp -i $K6_DOCKER_t_OR_init \
    graphql-bench-ci query \
    --config="./tmp/config.query.yaml" \
    --outfile="./tmp/report.json" --url "$HASURA_URL/v1/graphql"

  echo_pretty "Done. Report at $PWD/report.json"
  cd -
}

function load_data_and_schema() {
  echo_pretty "Loading data and adding schema"
  cd "$BENCH_DIR"
  gunzip -c dump.sql.gz | $PSQL_DOCKER &> /dev/null
  # --fail-with-body is what we want, but is not available on older curl:
  # TODO LATER: use /v1/metadata once stable
  curl --fail -X POST -H "Content-Type: application/json" -d @replace_metadata.json "$HASURA_URL/v1/query"
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

load_data_and_schema
run_benchmarks
