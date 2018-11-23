#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

wait_for_port() {
    local PORT=$1
    echo "waiting for $PORT"
    for i in `seq 1 60`;
    do
      nc -z localhost $PORT && echo "port $PORT is ready" && return
      echo -n .
      sleep 1
    done
    echo "Failed waiting for $PORT" && exit 1
}

cd "$CONSOLE_ROOT"

mkdir -p /home/circleci/build/_console_output
touch /home/circleci/build/_console_output/server.log
touch /home/circleci/build/_console_output/cli.log

# start graphql-engine
/home/circleci/build/_server_output/graphql-engine \
    --database-url postgres://gql_test@localhost:5432/gql_test serve > /home/circleci/build/_console_output/server.log 2>&1 &

wait_for_port 8080

# start cli
/home/circleci/build/_cli_output/binaries/cli-hasura-linux-amd64 init --directory gql-test && cd gql-test
/home/circleci/build/_cli_output/binaries/cli-hasura-linux-amd64 console --no-browser > /home/circleci/build/_console_output/cli.log 2>&1 &

cd ..

wait_for_port 9693

export PORT=3000
export NODE_ENV=development
export DATA_API_URL=http://localhost:8080
export API_HOST=http://localhost
export API_PORT=9693
export CONSOLE_MODE=cli
export DEV_DATA_API_URL=http://localhost:8080
export URL_PREFIX=/

# test console
npm run dev &
# wait for console to build
sleep 60
# run console tests
docker run --name test1 --net host -d -v /home/circleci/.cache/Cypress/3.1.0/Cypress:/usr/local/bin/Cypress -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 /bin/bash -c "cd /root/graphql-engine/console && node_modules/.bin/cypress open run --spec 'cypress/integration/data/relationships/test.js,cypress/integration/data/modify/test.js'"
#docker run --name test2 --net host -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 cypress run --spec 'cypress/integration/data/insert-browse/test.js,cypress/integration/data/migration-mode/test.js,cypress/integration/remote-schemas/create-remote-schema/test.js'
#docker run --name test3 --net host -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 cypress run --spec 'cypress/integration/data/views/test.js,cypress/integration/events/create-trigger/test.js,cypress/integration/data/create-table/test.js'
#docker run --name test4 --net host -d -v /home/circleci/graphql-engine:/root/graphql-engine hasura/graphql-engine-console-builder:v0.3 cypress run --spec 'cypress/integration/data/permissions/test.js,cypress/integration/data/raw-sql/test.js,cypress/integration/api-explorer/graphql/test.js,cypress/integration/data/404/test.js'

docker wait test1
#docker wait test2
#docker wait test3
#docker wait test4
