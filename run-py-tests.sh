set -euo pipefail

DB_NAME='hasura-tests'
POSTGRES="postgres://localhost:4200/$DB_NAME"

cd $(dirname $(realpath "$0"))
dropdb   -p 4200 ${DB_NAME} --if-exists
createdb -p 4200 ${DB_NAME}

cd server
source .python-venv/bin/activate
pip3 install -r tests-py/requirements.txt > /dev/null
export EVENT_WEBHOOK_HEADER=MyEnvValue
export WEBHOOK_FROM_ENV=http://localhost:5592/

cabal new-run -- exe:graphql-engine \
  --database-url=${POSTGRES}        \
  serve                             \
  --stringify-numeric-types         \
  2>&1 > tests-py/hasura.log &
sleep 10

cd tests-py
pytest --hge-urls http://localhost:8080 --pg-urls ${POSTGRES} "$@" || true
ps | grep graphql | cut -d ' ' -f 2 | xargs kill
