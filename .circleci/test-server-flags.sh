#!/usr/bin/env bash
set -euo pipefail

CIRCLECI_FOLDER="${BASH_SOURCE[0]%/*}"
cd $CIRCLECI_FOLDER
CIRCLECI_FOLDER="$PWD"

SERVER_ROOT="$CIRCLECI_FOLDER/../server"

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


########## Test --use-prepared-statements

"$GRAPHQL_ENGINE" serve --use-prepared-statements=false > "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

sleep 1

kill $PID || true

 grep --color -F '"use_prepared_statements":false' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null

"$GRAPHQL_ENGINE" serve --use-prepared-statements=true > "$OUTPUT_FOLDER/graphql-engine.log" & PID=$!

sleep 1

kill $PID || true

 grep --color -F '"use_prepared_statements":true' "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null

######### Test HASURA_GRAPHQL_USE_PREPARED_STATEMENTS environmental variable

export HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=abcd

"$GRAPHQL_ENGINE" serve  > "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

sleep 1

kill $PID || true

grep --color -F 'Not a valid boolean text'  "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null


export HASURA_GRAPHQL_USE_PREPARED_STATEMENTS=false

"$GRAPHQL_ENGINE" serve  > "$OUTPUT_FOLDER/graphql-engine.log" 2>&1 & PID=$!

sleep 1

kill $PID || true

grep --color -F '"use_prepared_statements":false'  "$OUTPUT_FOLDER/graphql-engine.log" >/dev/null
