#!/usr/bin/env bash
set -euo pipefail

CIRCLECI_FOLDER="${BASH_SOURCE[0]%/*}"
cd $CIRCLECI_FOLDER
CIRCLECI_FOLDER="$PWD"

SERVER_ROOT="$CIRCLECI_FOLDER/../server"

i=1
echoInfo() {
  echo -e "\033[36m$i. $*\033[0m"
  i=$[i+1]
}

fail_if_port_busy() {
    local PORT=$1
    if nc -z localhost $PORT ; then
        echo "Port $PORT is busy. Exiting"
        exit 1
    fi
}

wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for _ in $(seq 1 30);
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 0.2
    done
    echo "Failed waiting for $PORT" && exit 1
}

test_export_metadata_with_access_key() {
  curl -f   -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/query -H "X-Hasura-Access-Key: $1" > /dev/null
}

cd $SERVER_ROOT

if [ -z "${HASURA_GRAPHQL_DATABASE_URL:-}" ] ; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL is not set"
	exit 1
fi

if ! stack --allow-different-user exec -- which graphql-engine > /dev/null && [ -z "${GRAPHQL_ENGINE:-}" ] ; then
	echo "Do 'stack build' before tests, or export the location of executable in the GRAPHQL_ENGINE envirnoment variable"
	exit 1
fi

GRAPHQL_ENGINE=${GRAPHQL_ENGINE:-"$(stack --allow-different-user exec -- which graphql-engine)"}
if ! [ -x "$GRAPHQL_ENGINE" ] ; then
	echo "$GRAPHQL_ENGINE is not present or is not an executable"
	exit 1
fi

HGE_PID=""

run_hge_with_flags() {
    fail_if_port_busy 8080
    set -x
    stdbuf -o0 "$GRAPHQL_ENGINE" serve $*  > "$OUTPUT_FOLDER/graphql-engine.log" & HGE_PID=$!
    set +x
    wait_for_port 8080
}

kill_hge() {
  kill -s INT $HGE_PID || true
  wait $HGE_PID || true
}

trap kill_hge ERR
trap kill_hge INT

OUTPUT_FOLDER=${OUTPUT_FOLDER:-"$CIRCLECI_FOLDER/test-server-flags-output"}
mkdir -p "$OUTPUT_FOLDER"

################### Test deprecated flag --access-key

key="HGE$RANDOM$RANDOM"

run_hge_with_flags --access-key="$key"

echoInfo "Test deprecated flag --access-key=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null
test_export_metadata_with_access_key "$key"

kill_hge

################## Test deprecated EnvVar HASURA_GRAPHQL_ACCESS_KEY=XXXX

key="HGE$RANDOM$RANDOM"

export HASURA_GRAPHQL_ACCESS_KEY="$key"

run_hge_with_flags

echoInfo "Test deprecated EnvVar HASURA_GRAPHQL_ACCESS_KEY=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)
test_export_metadata_with_access_key "$key"

kill_hge

unset $HASURA_GRAPHQL_ACCESS_KEY
