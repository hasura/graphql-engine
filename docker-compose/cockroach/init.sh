#!/bin/sh
set -e

cockroach sql --insecure --database "$COCKROACH_DATABASE" <<-EOSQL
    create schema hasura;
EOSQL
