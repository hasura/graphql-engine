#! /usr/bin/env bash

set -euo pipefail

# # keep track of the last executed command
# trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# # echo an error message before exiting
# trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT

ROOT="${BASH_SOURCE[0]%/*}"

fail_if_port_busy() {
    local PORT=$1
    if nc -z localhost $PORT ; then
        echo "Port $PORT is busy. Exiting"
        exit 1
    fi
}

# wait_for_port PORT [PID] [LOG_FILE]
wait_for_port() {
    local PORT=$1
    local PIDMSG=""
    local PID=${2:-}
    if [ -n "$PID" ] ; then
        PIDMSG=", PID ($PID)"
    fi
    echo "waiting for ${PORT}${PIDMSG}"
    for i in `seq 1 60`;
    do
        nc -z localhost $PORT && echo "port $PORT is ready" && return
        echo -n .
        sleep 1
	      if [ -n "$PID" ] && ! ps $PID >/dev/null ; then
		        echo "Process $PID has exited"
		        if [ -n "${3:-}" ] ; then
			          cat $3
		        fi
            exit 1
	      fi
    done
    echo "Failed waiting for $PORT" && exit 1
}

log() { echo $'\e[1;33m'"--> $*"$'\e[0m'; }

# TODO: Fix me
# The release hasura cli will not work with the current build graphql-engine.
# So we cannot do a migration from the current build graphql-engine with hasura cli
# Since downgrade command is not present, we cannot downgrade and then do down 
# migration with the release graphql-engine
# Hacking around this issue
# When Hasura downgrade command is present, Change this to hasura downgrade
# followed by hasura down migration with the latest release graphql-engine
do_down_migration() {
	cat $(find "${ROOT}/hasura/migrations/" | grep down.yaml | sort -r) | python -c '
import sys, yaml, json
bulk_conf = yaml.safe_load(sys.stdin)
json.dump( {
		"type": "bulk",
		"args": bulk_conf
	}, sys.stdout, indent=2
)
' | curl -d @- http://localhost:$HASURA_GRAPHQL_SERVER_PORT/v1/query
}

# env HASURA_GRAPHQL_DATABASE_URL
: ${HASURA_GRAPHQL_SERVER_PORT:=8080}
: ${API_SERVER_PORT:=3000}
: ${HASURA_PROJECT_DIR:=$ROOT/hasura}
: ${API_SERVER_DIR:=$ROOT/api-server}
: ${SERVER_OUTPUT_DIR:=/build/_server_output}
: ${SERVER_BINARY:=/build/_server_output/graphql-engine}
: ${LATEST_SERVER_BINARY:=/bin/graphql-engine-latest}
: ${HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES:=true}

LATEST_SERVER_LOG=$SERVER_OUTPUT_DIR/upgrade-test-latest-release-server.log
CURRENT_SERVER_LOG=$SERVER_OUTPUT_DIR/upgrade-test-current-server.log
API_SERVER_LOG=$SERVER_OUTPUT_DIR/api-server.log

API_SERVER_ENDPOINT=http://localhost:$API_SERVER_PORT
HGE_ENDPOINT=http://localhost:$HASURA_GRAPHQL_SERVER_PORT
PYTEST_DIR="${ROOT}/../../server/tests-py"

pip3 install -r "${PYTEST_DIR}/requirements.txt"

# event trigger and remote schema urls
: ${ET_ECHO_URL:=$API_SERVER_ENDPOINT/trigger/echo}
: ${RS_HELLO_URL:=$API_SERVER_ENDPOINT/remote-schema/hello}

# export them so that GraphQL Engine can use it
export ET_ECHO_URL=$ET_ECHO_URL
export RS_HELLO_URL=$RS_HELLO_URL
export HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES="$HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES"
export GHCRTS='-N1'

fail_if_port_busy ${HASURA_GRAPHQL_SERVER_PORT}

log "setting up directories"
mkdir -p $SERVER_OUTPUT_DIR
touch $LATEST_SERVER_LOG
touch $CURRENT_SERVER_LOG

# installing deps
log "installing deps"
yarn --cwd $ROOT install

# updating hasura cli
log "updating hasura cli"
hasura update-cli

# start api server for event triggers and remote schemas
log "starting api server for triggers and remote schemas"
yarn --cwd $API_SERVER_DIR install
PORT=$API_SERVER_PORT yarn --cwd $API_SERVER_DIR start-prod > $API_SERVER_LOG 2>&1 &
API_SERVER_PID=$!

wait_for_port $API_SERVER_PORT $API_SERVER_PID

# download latest graphql engine release
log "downloading latest release of graphql engine"
curl -Lo $LATEST_SERVER_BINARY https://graphql-engine-cdn.hasura.io/server/latest/linux-amd64
chmod +x $LATEST_SERVER_BINARY

# start graphql engine
log "starting latest graphql engine"
$LATEST_SERVER_BINARY serve > $LATEST_SERVER_LOG 2>&1 &
LAST_REL_HGE_PID=$!
trap "kill $LAST_REL_HGE_PID" ERR

wait_for_port $HASURA_GRAPHQL_SERVER_PORT $LAST_REL_HGE_PID $LATEST_SERVER_LOG
log "graphql engine started"

# apply migrations
log "applying migrations"
hasura --project $HASURA_PROJECT_DIR migrate apply --endpoint $HGE_ENDPOINT

# make a test query
# log "executing the test query"
# node $ROOT/make_test_query.js $HGE_ENDPOINT/v1alpha1/graphql
# log

# kill graphql engine
log "kill the server"
kill $LAST_REL_HGE_PID || true
wait $LAST_REL_HGE_PID || true

# start the current build
log "start the current build"
$SERVER_BINARY serve > $CURRENT_SERVER_LOG 2>&1 &
CURR_HGE_PID=$!
trap "kill $CURR_HGE_PID" ERR

wait_for_port $HASURA_GRAPHQL_SERVER_PORT $CURR_HGE_PID $CURRENT_SERVER_LOG
log "server started"

# make a test query
# log "executing test query"
# node $ROOT/make_test_query.js $HGE_ENDPOINT/v1alpha1/graphql
# log

do_down_migration

log "kill the server"
kill $CURR_HGE_PID || true
wait $CURR_HGE_PID || true

log "kill the api server"
kill $API_SERVER_PID || true
wait $API_SERVER_PID || true

log "Run some pytests with server upgrade"

WORKTREE_DIR="$(mktemp -d)"
rm_worktree(){
	rm -rf $WORKTREE_DIR
}
trap rm_worktree ERR

make_latest_release_worktree() {
	version="$( $LATEST_SERVER_BINARY version | cut -d':' -f2 | awk '{print $1}' )"
	git worktree add --detach "$WORKTREE_DIR" "$version"
}

cleanup_hasura_metadata() {
	psql "$HASURA_GRAPHQL_DATABASE_URL" -c 'drop schema if exists hdb_catalog cascade;
		drop schema if exists hdb_views cascade'
}

get_classes_in_module() {
	  python3 -c '
import pyclbr, os, re
folder, file = os.path.split("'"$1"'")
for c in pyclbr.readmodule(re.sub(".py$","", file), [folder]).keys():
  print(file + "::" + c)
'
}

get_pytest_classes() {
	  cd $PYTEST_DIR
	  for module in test_graphql_queries.py test_graphql_mutations.py ; do
		    get_classes_in_module $module
	  done
	  cd - >/dev/null
}

# TODO: Fix me
# The right way to run the server-upgrade tests is get the worktree of the latest release, and run the pytests in it.
# This will not work for us for this release cycle (Since the old pytest do not have the options needed to run server-upgrade tests)
# So for now we are running pytests in the current build, but filtering out only the the tests which are also present in the previous release
# From the next release onwards, we should run tests corresponding to the latest release (and not pytests in the current build)
run_server_upgrade_pytest() {
	local RELEASE_PYTEST_DIR="${WORKTREE_DIR}/server/tests-py"
	local HGE_URL="http://localhost:${HASURA_GRAPHQL_SERVER_PORT}"
	collect_previous_release_tests() {
		cd "$RELEASE_PYTEST_DIR"
		# TODO: Fix me
		# The test TestGraphqlUpdateBasic::test_set_author_name corresponds to a bug in the latest release
		# Ignoring this test. This issue is occuring because we are running the pytests of the current build
		# This issue should not occur once we move to testing with the old set of tests
		pytest --hge-urls "${HGE_URL}"  --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --collect-only -q "$1" 2>/dev/null | grep '::' \
        | grep -v 'test_graphql_mutations.py::TestGraphqlUpdateBasic::test_set_author_name'
	}
	local tests_to_run="$(collect_previous_release_tests $1)"
	[ -n "$tests_to_run" ] || ( echo "could not collect any tests for $1" && false )

	# Connect to Postgres and remove metadata schemas
	cleanup_hasura_metadata

	# Start the old GraphQL Engine
	log "starting latest graphql engine release"
	$LATEST_SERVER_BINARY serve > $LATEST_SERVER_LOG 2>&1 &
	HGE_PID=$!
	cleanup_hge(){
		kill $HGE_PID || true
		wait $HGE_PID || true
		cleanup_hasura_metadata || true
		rm_worktree
	}
	trap cleanup_hge ERR

	# Wait for server start
	wait_for_port $HASURA_GRAPHQL_SERVER_PORT $HGE_PID $LATEST_SERVER_LOG

	log "Run pytest for latest graphql-engine release while skipping schema teardown"
	cd $PYTEST_DIR
	set -x
	# We are only going to throw warnings if the error message has changed between releases
	pytest --hge-urls "${HGE_URL}"  --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --skip-schema-teardown --avoid-error-message-checks -v $tests_to_run
	set +x
	cd -

	log "kill the api server"
	kill $HGE_PID || true
	wait $HGE_PID || true

	log "the current list of tables in public schema are: "
	psql $HASURA_GRAPHQL_DATABASE_URL -c '\d'

	log "start the current build"
	$SERVER_BINARY serve > $CURRENT_SERVER_LOG 2>&1 &
	HGE_PID=$!

	# Wait for server start
	wait_for_port $HASURA_GRAPHQL_SERVER_PORT $HGE_PID $CURRENT_SERVER_LOG

	log "Run pytest for the current build while skipping schema setup"
	cd $PYTEST_DIR
	set -x
	pytest -vv --hge-urls "$HGE_URL"  --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --skip-schema-setup -v $tests_to_run
	set +x
	cd -

	log "kill the api server"
	kill $HGE_PID || true
	wait $HGE_PID || true
}

make_latest_release_worktree

for pytest in $(get_pytest_classes) ; do
	echo "Running tests $pytest"
	run_server_upgrade_pytest "$pytest"
done
cleanup_hasura_metadata

exit 0
