#! /usr/bin/env bash

# If no arguments are provided to this script, all the server upgrade tests will be run
# With arguments, you can specify which server upgrade pytests should be run
# Any options provided to this script will be applied to the
# pytest command collecting server upgrade tests

set -euo pipefail

# # keep track of the last executed command
# trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# # echo an error message before exiting
# trap 'echo "\"${last_command}\" command filed with exit code $?."' EXIT

ROOT="${BASH_SOURCE[0]%/*}"

download_with_etag_check() {
	URL="$1"
	FILE="$2"
	ETAG="$(curl -I $URL | grep etag: | awk '{print $2}' | sed 's/\r$//')"
	set -x
	if ! ( [ -f "$FILE" ] && [ "$(cat "$FILE.etag" 2>/dev/null)" == "$ETAG" ] ) ; then
		curl -Lo "$FILE" "$URL"
		chmod +x "$FILE"
		echo -e -n "$ETAG" > "$FILE.etag"
	fi
	set +x
}

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

HGE_ENDPOINT=http://localhost:$HASURA_GRAPHQL_SERVER_PORT
PYTEST_DIR="${ROOT}/../../server/tests-py"

pip3 -q install -r "${PYTEST_DIR}/requirements.txt"

# export them so that GraphQL Engine can use it
export HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES="$HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES"
# Required for testing caching
export GHCRTS='-N1'
# Required for event trigger tests
export WEBHOOK_FROM_ENV="http://127.0.0.1:5592"
export EVENT_WEBHOOK_HEADER="MyEnvValue"

# graphql-engine will be run on this port
fail_if_port_busy ${HASURA_GRAPHQL_SERVER_PORT}

# Remote graphql server of pytests run on this port
fail_if_port_busy 5000

log "setting up directories"
mkdir -p $SERVER_OUTPUT_DIR
touch $LATEST_SERVER_LOG
touch $CURRENT_SERVER_LOG

# download latest graphql engine release
log "downloading latest release of graphql engine"
download_with_etag_check 'https://graphql-engine-cdn.hasura.io/server/latest/linux-amd64' "$LATEST_SERVER_BINARY"

cur_server_version(){
	echo "$(curl http://localhost:${HASURA_GRAPHQL_SERVER_PORT}/v1/version -q 2>/dev/null)"
}

log "Run pytests with server upgrade"

WORKTREE_DIR="$(mktemp -d)"
rm_worktree(){
	rm -rf "$WORKTREE_DIR"
}
trap rm_worktree ERR

make_latest_release_worktree() {
	version="$( $LATEST_SERVER_BINARY version | cut -d':' -f2 | awk '{print $1}' )"
	git worktree add --detach "$WORKTREE_DIR" "$version"
}

cleanup_hasura_metadata() {
	set -x
	psql "$HASURA_GRAPHQL_DATABASE_URL" -c 'drop schema if exists hdb_catalog cascade;
		drop schema if exists hdb_views cascade' >/dev/null 2>/dev/null
	set +x
}


args=("$@")
get_server_upgrade_tests() {
	cd $PYTEST_DIR
	tmpfile="$(mktemp --dry-run)"
	set -x
	python3 -m pytest -q --collect-only --collect-upgrade-tests-to-file "$tmpfile" -m 'allow_server_upgrade_test and not skip_server_upgrade_test' "${args[@]}" 1>/dev/null 2>/dev/null
	set +x
	cat "$tmpfile"
	cd - >/dev/null
	rm "$tmpfile"
}

# TODO: Fix me
# The right way to run the server-upgrade tests is get the worktree of the latest release, and run the pytests in it.
# This will not work for us for this release cycle (Since the old pytest do not have the options needed to run server-upgrade tests)
# So for now we are running pytests in the current build, but filtering out only the the tests which are also present in the previous release
# From the next release onwards, we should run tests corresponding to the latest release (and not pytests in the current build)
run_server_upgrade_pytest() {
	local RELEASE_PYTEST_DIR="${WORKTREE_DIR}/server/tests-py"
	local HGE_URL="http://localhost:${HASURA_GRAPHQL_SERVER_PORT}"
	collect_common_tests() {
		cd "$RELEASE_PYTEST_DIR"
		set -x
		pytest --hge-urls "${HGE_URL}"  --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --collect-only -q  "$1" 2>/dev/null | grep '::'
		cd - > /dev/null
	}

	[ -n "$1" ] || ( echo "Got no test as input" && false )

	local tests_to_run="$(collect_common_tests $1)"
	[ -z "$tests_to_run" ] && log "could not collect any tests for $1" && return

	log "Removing schemas with hasura metadata"
	# This is required since we do not have a dowgrade command, yet.
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

	log "Run pytest for latest graphql-engine release $(cur_server_version) while skipping schema teardown"
	cd $PYTEST_DIR
	set -x
	# We are only going to throw warnings if the error message has changed between releases
	pytest --hge-urls "${HGE_URL}"  --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --skip-schema-teardown --avoid-error-message-checks -m 'allow_server_upgrade_test and not skip_server_upgrade_test' -v $tests_to_run
	set +x
	cd -

	log "kill the api server $(cur_server_version)"
	kill $HGE_PID || true
	wait $HGE_PID || true

	log "the tables of interest in the database are: "
	psql $HASURA_GRAPHQL_DATABASE_URL -P pager=off -c "
select table_schema as schema, table_name as name
from information_schema.tables
where table_schema not in ('hdb_catalog','hdb_views', 'pg_catalog', 'information_schema','topology', 'tiger')
  and (table_schema <> 'public'
         or table_name not in ('geography_columns','geometry_columns','spatial_ref_sys','raster_columns','raster_overviews')
      );
"

	if [[ "$1" =~ "test_schema_stitching" ]] ; then
		# Hasura metadata would have GraphQL servers defined as remote.
		# In such a case we need to have remote GraphQL server running
		# for the graphql-engine to start
		cd $PYTEST_DIR
		python3 graphql_server.py & REMOTE_GQL_PID=$!
		wait_for_port 5000
		cd -
	fi

	log "start the current build"
	$SERVER_BINARY serve > $CURRENT_SERVER_LOG 2>&1 &
	HGE_PID=$!

	# Wait for server start
	wait_for_port $HASURA_GRAPHQL_SERVER_PORT $HGE_PID $CURRENT_SERVER_LOG

	if [[ "$1" =~ "test_schema_stitching" ]] ; then
		kill $REMOTE_GQL_PID || true
		wait $REMOTE_GQL_PID || true
	fi

	log "Run pytest for the current build $(cur_server_version) while skipping schema setup "
	cd $PYTEST_DIR
	set -x
	pytest -vv --hge-urls "$HGE_URL"  --pg-urls "$HASURA_GRAPHQL_DATABASE_URL" --skip-schema-setup -v $tests_to_run
	set +x
	cd -

	log "kill the api server $(cur_server_version)"
	kill $HGE_PID || true
	wait $HGE_PID || true
}

make_latest_release_worktree

for pytest in $(get_server_upgrade_tests) ; do
	log "Running pytest $pytest"
	run_server_upgrade_pytest "$pytest"
done
cleanup_hasura_metadata

exit 0
