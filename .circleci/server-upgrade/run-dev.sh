#! /usr/bin/env bash
# This is the non-circleci version of run.sh
set -euo pipefail
set -x
ROOT="${BASH_SOURCE[0]%/*}"

SERVER_DIR="$ROOT/../../server"

cd $SERVER_DIR
export SERVER_BINARY=$(cabal new-exec which graphql-engine)
cd -

export SERVER_OUTPUT_DIR="server-output"
export LATEST_SERVER_BINARY="./graphql-engine-latest"

if [ -z "${HASURA_GRAPHQL_DATABASE_URL:=}" ] ; then
	echo "Needs environmental variable HASURA_GRAPHQL_DATABASE_URL"
	false
fi

if ! [ -f ".venv/bin/activate" ] ; then
	virtualenv .venv
fi

. .venv/bin/activate

set +x
"$ROOT"/run.sh

