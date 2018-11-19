#!/usr/bin/env bash

set -evo pipefail

PGPASSWORD=gql_test psql -h localhost -p 5432 -U gql_test -c "CREATE DATABASE ${1};"
PGPASSWORD=gql_test psql -h localhost -p 5432 -U gql_test -c "ALTER DATABASE ${1} OWNER TO gql_test;"
docker ps
docker network ls
docker network inspect bridge
docker network inspect host
docker build -t ${1} -f ${2}/${3} ${2}
( docker run -e "DATABASE_NAME=${1}" -e "DATABASE_USER=gql_test" -e "DATABASE_HOST=localhost" -e "DATABASE_PORT=5432" -e "DATABASE_PASS=gql_test" --net host ${1} ) || true
PGPASSWORD=gql_test psql -h localhost -p 5432 -U gql_test -c "DROP DATABASE ${1};"
