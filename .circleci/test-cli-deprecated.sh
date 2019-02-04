#!/usr/bin/env bash
set -euo pipefail

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

HGE_PID=""

kill_hge() {
  kill $HGE_PID || true
  wait $HGE_PID || true
}

trap kill_hge ERR
trap kill_hge INT

CIRCLECI_FOLDER="${BASH_SOURCE[0]%/*}"
cd $CIRCLECI_FOLDER
CIRCLECI_FOLDER="$PWD"

if ! stack --allow-different-user exec -- which graphql-engine > /dev/null && [ -z "${GRAPHQL_ENGINE:-}" ] ; then
	echo "Do 'stack build' before tests, or export the location of executable in the GRAPHQL_ENGINE envirnoment variable"
	exit 1
fi

GRAPHQL_ENGINE=${GRAPHQL_ENGINE:-"$(stack --allow-different-user exec -- which graphql-engine)"}
if ! [ -x "$GRAPHQL_ENGINE" ] ; then
	echo "$GRAPHQL_ENGINE is not present or is not an executable"
	exit 1
fi

OUTPUT_FOLDER=${OUTPUT_FOLDER:-"$CIRCLECI_FOLDER/test-cli-output"}

echo -e "\nINFO: GraphQL Executable : $GRAPHQL_ENGINE"
echo -e "INFO: Logs Folder        : $OUTPUT_FOLDER\n"

key="abcd$RANDOM"

"$GRAPHQL_ENGINE" serve --access-key=$key > "$OUTPUT_FOLDER/graphql-engine.log" & HGE_PID=$!

cd $OUTPUT_FOLDER

cliFolder="test-cli-cmds-folder"
rm -rf "$cliFolder"

#Test hasura init with access-key
hasura init --access-key=$key --endpoint="http://localhost:8080" --directory=$cliFolder

cd $cliFolder

mv config.yaml config.yaml.backup

hasura console --access-key=$key --endpoint="http://localhost:8080" --no-browser


