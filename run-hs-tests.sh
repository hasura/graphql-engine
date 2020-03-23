set -euo pipefail

DB_NAME='hasura-tests'
POSTGRES="postgres://localhost:4200/$DB_NAME"

cd $(dirname $(realpath "$0"))
dropdb   -p 4200 ${DB_NAME} --if-exists
createdb -p 4200 ${DB_NAME}

cd server
cabal new-run -- test:graphql-engine-tests --database-url="$POSTGRES"
