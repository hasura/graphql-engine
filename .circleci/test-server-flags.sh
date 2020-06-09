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
    for _ in $(seq 1 60);
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 0.2
    done
    echo "Failed waiting for $PORT" && exit 1
}

test_export_metadata_with_admin_secret() {
  curl -f   -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/query -H "X-Hasura-Admin-Secret: $1" > /dev/null
}

cd $SERVER_ROOT

if [ -z "${HASURA_GRAPHQL_DATABASE_URL:-}" ] ; then
	echo "Env var HASURA_GRAPHQL_DATABASE_URL is not set"
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


########## Test that we didn't compile with +developer by accident
echoInfo "Test we didn't compile in the deveoper-only APIs"

run_hge_with_flags

code=$(curl -s -o /dev/null -w "%{http_code}"  http://localhost:8080/dev/plan_cache)
if [ "$code" != "404" ]; then  
  echo "Expected a dev endpoint to return 404, but got: $code"
  exit 1
fi

kill_hge

########## Test --use-prepared-statements=false and flag --admin-secret

key="HGE$RANDOM$RANDOM"

run_hge_with_flags --use-prepared-statements=false --admin-secret="$key"

echoInfo "Test flag --admin-secret=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null
test_export_metadata_with_admin_secret "$key"

echoInfo "Test flag --use-prepared-statements=false"
grep -F '"use_prepared_statements":false' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null

kill_hge

###### Test --use-prepared-statements=true
key="HGE$RANDOM$RANDOM"

run_hge_with_flags --use-prepared-statements=true

echoInfo "Test --use-prepared-statements=true"
grep -F '"use_prepared_statements":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)

kill_hge

######### Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd


export HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd

fail_if_port_busy 8080
timeout 3 stdbuf -o0 "$GRAPHQL_ENGINE" serve  > "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait $PID || true

echoInfo "Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd"
grep -F 'Not a valid boolean text'  "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)


######### Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false and HASURA_GRAPHQL_ADMIN_SECRET=XXXX
key="HGE$RANDOM$RANDOM"

export HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false

export HASURA_GRAPHQL_ADMIN_SECRET="$key"

run_hge_with_flags

echoInfo "Test flag HASURA_GRAPHQL_ADMIN_SECRET=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)
test_export_metadata_with_admin_secret "$key"


echoInfo "Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false"
grep -F '"use_prepared_statements":false' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)

kill_hge

unset HASURA_GRAPHQL_ADMIN_SECRET

unset HASURA_GRAPHQL_USE_PREPARED_STATEMENTS
