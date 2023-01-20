#!/usr/bin/env bash

# required for test-server.sh

set -e
set -u
set -o pipefail

psql -c 'CREATE DATABASE gql_test2;'
psql -c 'CREATE DATABASE pg_source_1;'
psql -c 'CREATE DATABASE pg_source_2;'
