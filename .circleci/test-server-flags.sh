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

test_export_metadata_with_admin_secret() {
  curl -f   -d'{"type" : "export_metadata", "args" : {} }' localhost:8080/v1/query -H "X-Hasura-Admin-Secret: $1" > /dev/null
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

OUTPUT_FOLDER=${OUTPUT_FOLDER:-"$CIRCLECI_FOLDER/test-server-flags-output"}
mkdir -p "$OUTPUT_FOLDER"


########## Test --use-prepared-statements=false and flag --access-key (deprecated)

key="HGE$RANDOM$RANDOM"

stdbuf -o0 "$GRAPHQL_ENGINE" serve --use-prepared-statements=false --access-key="$key"  > "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

wait_for_port 8080

echoInfo "Test flag --access-key=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null
test_export_metadata_with_admin_secret "$key"

echoInfo "Test flag --use-prepared-statements=false"
grep -F '"use_prepared_statements":false' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null

kill $PID || true
wait $PID || true


###### Test --use-prepared-statements=true and --admin-secret
key="HGE$RANDOM$RANDOM"

stdbuf -o0 "$GRAPHQL_ENGINE" serve --use-prepared-statements=true --admin-secret="$key" > "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

wait_for_port 8080

echoInfo "Test flag --admin-secret=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)
test_export_metadata_with_admin_secret "$key"

echoInfo "Test --use-prepared-statements=true"
grep -F '"use_prepared_statements":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)

kill $PID || true
wait $PID || true


######### Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd


export HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd

timeout 3 stdbuf -o0 "$GRAPHQL_ENGINE" serve  > "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait $PID || true

echoInfo "Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd"
grep -F 'Not a valid boolean text'  "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)


######### Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false and deprecated EnvVar HASURA_GRAPHQL_ACCESS_KEY=XXXX
key="HGE$RANDOM$RANDOM"

export HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false

export HASURA_GRAPHQL_ACCESS_KEY="$key"

stdbuf -o0 "$GRAPHQL_ENGINE" serve  > "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

echoInfo "Test deprecated flag HASURA_GRAPHQL_ACCESS_KEY=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)
test_export_metadata_with_admin_secret "$key"


echoInfo "Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false"
grep -F '"use_prepared_statements":false' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)

kill $PID || true
wait $PID || true

unset HASURA_GRAPHQL_ACCESS_KEY

unset HASURA_GRAPHQL_USE_PREPARED_STATEMENTS

######### Test HASURA_GRAPHQL_ADMIN_SECRET=XXXX
key="HGE$RANDOM$RANDOM"

export HASURA_GRAPHQL_ADMIN_SECRET="$key"

stdbuf -o0 "$GRAPHQL_ENGINE" serve  > "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

wait_for_port 8080

echoInfo "Test deprecated flag HASURA_GRAPHQL_ADMIN_SECRET=XXXX"
grep -F '"admin_secret_set":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null || (cat "$OUTPUT_FOLDER/graphql-engine.log" && false)
test_export_metadata_with_admin_secret "$key"

kill $PID || true
wait $PID || true

unset $HASURA_GRAPHQL_ADMIN_SECRET

