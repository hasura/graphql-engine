#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

docker run --privileged --name server -d -e POSTGRES_HOST=$(docker inspect -f "{{ .NetworkSettings.IPAddress }}" postgres) -d -v /home/circleci/build:/root/build -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 bash -c "cd /root/graphql-engine && ./.circleci/start-console.sh"

CONSOLE_HOST=$(docker inspect -f "{{ .NetworkSettings.IPAddress }}" server)

cd "$CONSOLE_ROOT"

while [ ! -f ./webpack-assets.json ]
do
  sleep 2
done

mkdir -p /home/circleci/build/_console_test_logs

# run console tests
if [ "$TEST_SUITE_NUMBER" = "1" ]; then
  docker run --name test1 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/relationships/test.js,cypress/integration/remote-schemas/create-remote-schema/test.js' -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run-console-test.sh"
  docker run --name test2 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/insert-browse/test.js,cypress/integration/data/views/test.js' -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run-console-test.sh"
  EXIT_CODE_1="$(docker wait test1)"
  docker logs test1 > /home/circleci/build/_console_test_logs/test1.log
  EXIT_CODE_2="$(docker wait test2)"
  docker logs test2 > /home/circleci/build/_console_test_logs/test2.log
  if [ "$EXIT_CODE_1" != "0" ] || [ "$EXIT_CODE_2" != "0" ]; then
    exit 1
  fi
fi
if [ "$TEST_SUITE_NUMBER" = "2" ]; then
  docker run --name migration_mode -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=ypress/integration/data/migration-mode/test.js' -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run-console-test.sh"
  EXIT_CODE_MIGRATION_MODE="$(docker wait migration_mode)"
  docker logs migration_mode > /home/circleci/build/_console_test_logs/migration_mode.log
  if [ "$EXIT_CODE_MIGRATION_MODE" != "0" ]; then
    exit 1
  fi
  docker run --name test3 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/permissions/test.js,cypress/integration/data/raw-sql/test.js,ypress/integration/events/create-trigger/test.js' -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run-console-test.sh"
  docker run --name test4 -e TEST_ENV=MIGRATE_URL=http://${CONSOLE_HOST}:9693/apis/migrate -e TEST_CONFIG=baseUrl=http://${CONSOLE_HOST}:3000 -e 'TEST_SPECS=cypress/integration/data/migration-mode/test.js,cypress/integration/api-explorer/graphql/test.js,cypress/integration/data/create-table/test.js,cypress/integration/data/modify/test.js,cypress/integration/data/404/test.js' -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine && ./.circleci/run-console-test.sh"
  EXIT_CODE_3="$(docker wait test3)"
  docker logs test3 > /home/circleci/build/_console_test_logs/test3.log
  docker wait test4
  EXIT_CODE_4="$(docker wait test4)"
  docker logs test4 > /home/circleci/build/_console_test_logs/test4.log
  if [ "$EXIT_CODE_3" != "0" ] || [ "$EXIT_CODE_4" != "0" ]; then
    exit 1
  fi
fi
