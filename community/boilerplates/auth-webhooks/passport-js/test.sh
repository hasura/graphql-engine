#!/usr/bin/env bash

set -evo pipefail
export DATABASE_URL=${HASURA_GRAPHQL_DATABASE_URL}
echo "Migrate database"
knex migrate:latest
echo "Migrated database"
echo "Running tests"
