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

(
  cd ../..
  cabal build graphql-engine:exe:graphql-engine
  make server/tests-py/.hasura-dev-python-venv server/tests-py/node_modules
)

# shellcheck disable=SC1091
source .hasura-dev-python-venv/bin/activate

# Use the Azure SQL Edge image instead of the SQL Server image on arm64.
# The latter doesn't work yet.
if [[ "$(uname -m)" == 'arm64' ]]; then
  export MSSQL_IMAGE='mcr.microsoft.com/azure-sql-edge'
fi

docker compose rm -svf citus mssql postgres
docker compose up -d citus mssql postgres

HASURA_GRAPHQL_CITUS_SOURCE_URL="postgresql://postgres:hasura@localhost:$(docker compose port citus 5432 | sed -E 's/.*://')/postgres"
HASURA_GRAPHQL_MSSQL_SOURCE_URL="DRIVER={ODBC Driver 17 for SQL Server};SERVER=localhost,$(docker compose port mssql 1433 | sed -E 's/.*://');Uid=sa;Pwd=Password!;"
HASURA_GRAPHQL_PG_SOURCE_URL_1="postgresql://postgres:hasura@localhost:$(docker compose port --index 1 postgres 5432 | sed -E 's/.*://')/postgres"
HASURA_GRAPHQL_PG_SOURCE_URL_2="postgresql://postgres:hasura@localhost:$(docker compose port --index 2 postgres 5432 | sed -E 's/.*://')/postgres"
export HASURA_GRAPHQL_CITUS_SOURCE_URL HASURA_GRAPHQL_MSSQL_SOURCE_URL HASURA_GRAPHQL_PG_SOURCE_URL_1 HASURA_GRAPHQL_PG_SOURCE_URL_2

export EVENT_WEBHOOK_HEADER='MyEnvValue'
export EVENT_WEBHOOK_HANDLER='http://localhost:5592'
export ACTION_WEBHOOK_HANDLER='http://localhost:5593'
export SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN='http://localhost:5594'
export REMOTE_SCHEMAS_WEBHOOK_DOMAIN='http://localhost:5000'
export GRAPHQL_SERVICE_HANDLER='http://localhost:4001'
export GRAPHQL_SERVICE_1='http://localhost:4020'
export GRAPHQL_SERVICE_2='http://localhost:4021'
export GRAPHQL_SERVICE_3='http://localhost:4022'

pytest \
  --hge-bin="$(cabal list-bin graphql-engine:exe:graphql-engine)" \
  --pg-urls "$HASURA_GRAPHQL_PG_SOURCE_URL_1" "$HASURA_GRAPHQL_PG_SOURCE_URL_2" \
  "$@"
