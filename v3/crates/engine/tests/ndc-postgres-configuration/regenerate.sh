#!/usr/bin/env bash

# Regenerates the configuration.json file from the current database schema.

set -e
set -u

cd "$(dirname -- "${BASH_SOURCE[0]}")"
ROOT_DIR="$(cd -- ../../../.. && pwd)"
NDC_POSTGRES_DIR="${ROOT_DIR}/../ndc-postgres"

function ndc-postgres-cli {
  "${NDC_POSTGRES_DIR}/target/debug/ndc-postgres-cli" "$@"
}

echo 'INFO: This script assumes that you have checked out and updated ndc-postgres next to this repository.'

echo 'Building ndc-postgres-cli...'
(
  cd "$NDC_POSTGRES_DIR"
  if [[ -e .envrc.local ]]; then
    eval "$(direnv export bash)"
  fi
  cargo build --bin ndc-postgres-cli
)

echo 'Starting PostgreSQL...'
docker compose up --wait postgres

echo 'Updating the configuration...'
export CONNECTION_URI='postgresql://postgres:password@localhost:5432'
ndc-postgres-cli update
