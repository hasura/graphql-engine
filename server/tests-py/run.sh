#!/usr/bin/env bash

# This allows a developer to easily run the Python integration tests using the
# `--hge-bin` flag.
#
# The Pytest runner will start a new HGE instance for each test class, with the
# default arguments and the environment variables provided below.
#
# This is a work in progress.

set -e
set -u
set -o pipefail

cd -- "$(dirname -- "${BASH_SOURCE[0]}")"

DATABASES=(postgres citus sqlserver redis)

# If `HGE_BIN` is not set, build and use `graphql-engine:exe:graphql-engine`
if [[ -z "${HGE_BIN:-}" ]]; then
  (
    cd ../..
    echo '*** Building HGE ***'
    cabal build graphql-engine:exe:graphql-engine
    echo
  )
  HGE_BIN="$(cabal list-bin graphql-engine:exe:graphql-engine)"
fi

(
  echo '*** Installing test dependencies ***'
  make -C ../.. server/tests-py/.hasura-dev-python-venv server/tests-py/node_modules remove-tix-file
)

# shellcheck disable=SC1091
source .hasura-dev-python-venv/bin/activate

echo
echo '*** Starting databases ***'
docker compose up -d --wait "${DATABASES[@]}"

HASURA_GRAPHQL_PG_SOURCE_URL_1="postgresql://postgres:hasura@localhost:$(docker compose port --index 1 postgres 5432 | sed -E 's/.*://')/postgres"
HASURA_GRAPHQL_PG_SOURCE_URL_2="postgresql://postgres:hasura@localhost:$(docker compose port --index 2 postgres 5432 | sed -E 's/.*://')/postgres"
HASURA_GRAPHQL_CITUS_SOURCE_URL="postgresql://postgres:hasura@localhost:$(docker compose port citus 5432 | sed -E 's/.*://')/postgres"
HASURA_GRAPHQL_MSSQL_SOURCE_URL="DRIVER={ODBC Driver 18 for SQL Server};SERVER=localhost,$(docker compose port sqlserver 1433 | sed -E 's/.*://');Uid=sa;Pwd=Password!;Encrypt=optional"
HASURA_GRAPHQL_REDIS_URL="redis://localhost:$(docker compose port redis 6379 | sed -E 's/.*://')/"
export HASURA_GRAPHQL_PG_SOURCE_URL_1 HASURA_GRAPHQL_PG_SOURCE_URL_2 HASURA_GRAPHQL_CITUS_SOURCE_URL HASURA_GRAPHQL_MSSQL_SOURCE_URL

echo
echo '*** Running tests ***'
export SQLALCHEMY_SILENCE_UBER_WARNING=1 # disable warnings about upgrading to SQLAlchemy 2.0

COMMAND=(
  pytest
    --dist=loadscope
    -n auto
    --hge-bin="$HGE_BIN"
    --pg-urls "$HASURA_GRAPHQL_PG_SOURCE_URL_1" "$HASURA_GRAPHQL_PG_SOURCE_URL_2"
    --redis-url "$HASURA_GRAPHQL_REDIS_URL"
    "$@"
)

echo '+' "${COMMAND[@]}"
"${COMMAND[@]}"
