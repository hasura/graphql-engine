#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

docker run --name server -d -e POSTGRES_HOST=$(docker inspect -f "{{ .NetworkSettings.IPAddress }}" postgres) -d -v /root/build:/root/build -v /root/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 bash -c "cd /root/graphql-engine && ./.circleci/start_console.sh"

CONSOLE_HOST=$(docker inspect -f "{{ .NetworkSettings.IPAddress }}" server)

cd "$CONSOLE_ROOT"

while [ ! -f ./webpack-assets.json ]
do
  sleep 2
done

# run console tests
docker run --name test1 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/relationships/test.js,cypress/integration/data/modify/test.js' -d -v /root/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run_test.sh"
docker run --name test2 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/insert-browse/test.js,cypress/integration/data/migration-mode/test.js,cypress/integration/remote-schemas/create-remote-schema/test.js' -d -v /root/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run_test.sh"
docker run --name test3 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/views/test.js,cypress/integration/events/create-trigger/test.js,cypress/integration/data/create-table/test.js' -d -v /root/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run_test.sh"
docker run --name test4 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/permissions/test.js,cypress/integration/data/raw-sql/test.js,cypress/integration/api-explorer/graphql/test.js,cypress/integration/data/404/test.js' -d -v /root/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run_test.sh"

docker wait test1
docker wait test2
docker wait test3
docker wait test4

docker logs test1
docker logs test2
docker logs test3
docker logs test4
