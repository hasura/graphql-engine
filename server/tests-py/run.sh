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

DATABASES=(postgres citus sqlserver)

(
  cd ../..
  echo '*** Building HGE ***'
  cabal build -j graphql-engine:exe:graphql-engine

  echo
  echo '*** Installing test dependencies ***'
  make server/tests-py/.hasura-dev-python-venv server/tests-py/node_modules remove-tix-file
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
export HASURA_GRAPHQL_PG_SOURCE_URL_1 HASURA_GRAPHQL_PG_SOURCE_URL_2 HASURA_GRAPHQL_CITUS_SOURCE_URL HASURA_GRAPHQL_MSSQL_SOURCE_URL

echo
echo '*** Running tests ***'
pytest \
  --dist=loadscope \
  -n auto \
  --hge-bin="$(cabal list-bin graphql-engine:exe:graphql-engine)" \
  --pg-urls "$HASURA_GRAPHQL_PG_SOURCE_URL_1" "$HASURA_GRAPHQL_PG_SOURCE_URL_2" \
  "$@"
