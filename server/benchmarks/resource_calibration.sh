#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

# Allow killing background process by pgid without killing self
set -m

## This is a rough script that helps us quantify the resources required for
## hasura depending on the schema and the expected load. We expect only to need
## to run this quarterly or so.
##
## see: https://hasurahq.atlassian.net/browse/PR-56

echo_pretty() {
    echo ">>> $(tput setaf 2)$1$(tput sgr0)"
}
echo_error() {
    echo ">>> $(tput setaf 1)$1$(tput sgr0)"
}
echo_warn() {
    echo ">>> $(tput setaf 3)$1$(tput sgr0)"
}

REPO_TOPLEVEL=$(git rev-parse --show-toplevel)

MEM_CPU_OUTFILE=$(mktemp)
ENGINE_OUT_FILE=$(mktemp)
POSTGRES_OUT_FILE=$(mktemp)

# Test between 1 and 3:
NUM_SERVER_CORES=2

echo_warn "Please make sure your computer has at least 8 cores and that you've disabled lower processor sleep states! "
echo_warn "    $ sudo cpupower frequency-set -g performance  && sudo cpupower idle-set -D10 # PERFORMANCE "
sleep 5

if ! command -v gblreg &> /dev/null
then
    echo_error "Install gbutils for regression tool 'gblreg'"
    exit 1
fi

function start_engine {
    # use gnu time to get the memory high watermark
    # Run with -Fd for faster memory reclamation back to baseline
    command time -f "%M %P" -o "$MEM_CPU_OUTFILE" \
        "$REPO_TOPLEVEL/scripts/dev.sh" graphql-engine --optimized -- +RTS -N"$NUM_SERVER_CORES" -Fd0.01 -RTS \
        &> "$ENGINE_OUT_FILE" & GRAPHQL_ENGINE_PID=$!

    if [ "${1-}" != "no_wait" ]; then
        echo -n "Waiting for graphql-engine (for logs see: $ENGINE_OUT_FILE)"

        until curl -s "http://127.0.0.1:8181/v1/query" &>/dev/null; do
          echo -n '.' && sleep 0.2
          # If the server stopped abort immediately
          if ! kill -0 $GRAPHQL_ENGINE_PID ; then
            echo_error "The server crashed or failed to start!!"
            exit 42
          fi
        done
        echo " Ok"
    fi
}

function stop_engine {
    PGID=$(ps -o '%r' "$GRAPHQL_ENGINE_PID" | tail -n1 | xargs)
    # echo "PID/PGID: $$ $GRAPHQL_ENGINE_PID $PGID"
    # Send INT to get output from GNU time!:
    kill -INT "-$PGID"
    # kill -- "-$PGID"  # ...not this
    wait "$GRAPHQL_ENGINE_PID" || true
    if [ -f "$ENGINE_OUT_FILE" ]; then
        echo_pretty "Productivity of engine just stopped, FYI:"
        until grep '^  Productivity' "$ENGINE_OUT_FILE" ; do 
            sleep 1 
        done
    fi
    rm -f "$ENGINE_OUT_FILE"

}
function start_postgres {
    echo "Launching postgres (see logs at $POSTGRES_OUT_FILE)"
    "$REPO_TOPLEVEL/scripts/dev.sh" postgres \
        &> "$POSTGRES_OUT_FILE" & POSTGRES_PID=$!
}
function stop_postgres {
    PGID=$(ps -o '%r' "$POSTGRES_PID" | tail -n1 | xargs)
    kill -- "-$PGID"
    wait "$POSTGRES_PID" || true
    rm -f "$POSTGRES_OUT_FILE"
}

function cleanup {
    set +e
    echo_pretty "Cleaning up"

    stop_engine
    stop_postgres

    rm -f "$MEM_CPU_OUTFILE"

    echo "Done"
}
trap cleanup EXIT

# Get a memory high water mark for replace_metadata
# Must be executed from a benchmark set directory
function init_and_replace_metadata {
    echo_pretty "Initializing and doing some replace_metadata"
    gunzip -c dump.sql.gz | PGPASSWORD=postgres psql -h 127.0.0.1 -p 25432 postgres -U postgres &>/dev/null

    # run replace_metadata a few times (once to initialize schema, a few more to get good high water mark)
    curl  -X POST -H 'Content-Type: application/json' -d @replace_metadata.json http://127.0.0.1:8181/v1/query
    curl  -X POST -H 'Content-Type: application/json' -d @replace_metadata.json http://127.0.0.1:8181/v1/query
    curl  -X POST -H 'Content-Type: application/json' -d @replace_metadata.json http://127.0.0.1:8181/v1/query
    echo
}

### Metadata operations and Baseline + peak memory ####################

if true; then
    ## huge_schema: ########
    start_postgres
    start_engine

    cd "$REPO_TOPLEVEL/server/benchmarks/benchmark_sets/huge_schema"
    init_and_replace_metadata
    echo_pretty "Sleeping for 30 seconds and then checking  for baseline memory usage"
    sleep 30
    MEM_BASELINE_HUGE_SCHEMA=$(ps -e -o pid,ppid,pgid,rss,comm | awk '$3 == '"$GRAPHQL_ENGINE_PID" | grep graphql-engine | awk '{print $4}')

    stop_engine
    stop_postgres
    echo "sleeping..." && sleep 30  # TODO wait for all in process group
    MEM_HIGHWATER_HUGE_SCHEMA=$(tail -n1 "$MEM_CPU_OUTFILE" | awk '{print $1}')


    ## chinook: ########
    start_postgres
    start_engine

    cd "$REPO_TOPLEVEL/server/benchmarks/benchmark_sets/chinook"
    init_and_replace_metadata
    echo_pretty "Sleeping for 30 seconds and then checking  for baseline memory usage"
    sleep 30
    MEM_BASELINE_CHINOOK=$(ps -e -o pid,ppid,pgid,rss,comm | awk '$3 == '"$GRAPHQL_ENGINE_PID" | grep graphql-engine | awk '{print $4}')

    stop_engine
    stop_postgres
    echo "sleeping..." && sleep 30  # TODO wait for all in process group
    MEM_HIGHWATER_CHINOOK=$(tail -n1 "$MEM_CPU_OUTFILE" | awk '{print $1}')
fi

### Throughput limit and Peak memory under load ####################

if true; then
    cd "$REPO_TOPLEVEL/server/benchmarks"
    start_engine no_wait

    ./bench.sh chinook_throughput

    stop_engine
    echo "sleeping..." && sleep 30  # TODO wait for all in process group
    MEM_HIGHWATER_CHINOOK_UNDER_LOAD=$(tail -n1 "$MEM_CPU_OUTFILE" | awk '{print $1}')
    CPU_CHINOOK_UNDER_LOAD=$(tail -n1 "$MEM_CPU_OUTFILE" | awk '{print $2}')
fi

set +e
echo_pretty "#######################  RAW MEASUREMENTS  ###########################"
echo_pretty ""
echo_pretty "Memory usage in KB:"
(echo "| SCHEMA_BASELINE REPLACE_METADATA_PEAK UNDER_LOAD_PEAK" ;\
 echo "huge_schema $MEM_BASELINE_HUGE_SCHEMA $MEM_HIGHWATER_HUGE_SCHEMA N/A" ;\
 echo "chinook     $MEM_BASELINE_CHINOOK $MEM_HIGHWATER_CHINOOK $MEM_HIGHWATER_CHINOOK_UNDER_LOAD") |\
 column --table -R1,2,3,4
echo_pretty ""
echo_pretty "Avg CPU During Chinook throughput tests:  $CPU_CHINOOK_UNDER_LOAD "
echo        "NOTE: The utility of the script relies on the assumption that the throughput "
echo        "    tests here are  mostly CPU bound.  we want the value above to be between 150% and "
echo        "    ${NUM_SERVER_CORES}00% (using all $NUM_SERVER_CORES cores allotted to server)"
echo        "    FYI: complex_query_high_load_large_result appears to be IO bound, "
echo        "    with the server at only 100% CPU (on two cores)"
echo_pretty ""
# TODO add uncompressed response body sizes here:
echo_pretty "Peak sustained throughput for our Chinook queries having different uncompressed response body sizes (server given $NUM_SERVER_CORES cores)"
paste -d ' ' <(echo -e "simple_query_high_load(600B): \n complex_query_high_load_small_result(650B): \n complex_query_high_load_large_result(33KB): \n full_introspection(190KB):") \
             <(jq '.[] .requests.average |floor' "$REPO_TOPLEVEL/server/benchmarks/benchmark_sets/chinook_throughput/report.json" ) \
             <(echo -e "RPS\nRPS\nRPS\nRPS") |\
             column --table 
echo_pretty ""
echo_pretty "#######################  INTERPRETATION    ###########################"
CHINOOK_PEAK_MEM=$(( MEM_HIGHWATER_CHINOOK_UNDER_LOAD > MEM_HIGHWATER_CHINOOK ? MEM_HIGHWATER_CHINOOK_UNDER_LOAD : MEM_HIGHWATER_CHINOOK ))
CHINOOK_MEM_SCALE=$(bc -l <<< "scale=1; $CHINOOK_PEAK_MEM/$MEM_BASELINE_CHINOOK")
HUGE_SCHEMA_MEM_SCALE=$(bc -l <<< "scale=1; $MEM_HIGHWATER_HUGE_SCHEMA/$MEM_BASELINE_HUGE_SCHEMA")
echo_pretty "Under peak sustained throughput and with some replace_metadata, peak memory usage is typically between..."
echo_pretty "    ${CHINOOK_MEM_SCALE}x and ${HUGE_SCHEMA_MEM_SCALE}x "
echo_pretty "...above the idle baseline (i.e. the schema overhead)"

# TODO automate this
echo_warn ""
echo_warn "ABOVE WAS RUN WITH SERVER ALLOCATED   < $NUM_SERVER_CORES >   CORES."
echo_warn ""
echo_warn "Rerun this with one, two and three cores ( this is about the limit you can do on an"
echo_warn "8 core laptop  and still get meaningful numbers). Run a linear regression for each:"
echo_warn '  $ echo "1 2266\\n2 3587\\n 3 5270" | gblreg'
echo_warn '  7.036667e+02  1.502000e+03'
echo_warn '  A^            B^   in:   PEAK_THROUGHPUT=A+B*SERVER_CORES'

echo_pretty "Done. Shutting down"
