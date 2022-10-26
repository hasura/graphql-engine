#!/bin/bash

set -e
set -u
set -o pipefail

SERVER_PORT="${1:-1433}"
SLEEP=5
MAX_ATTEMPTS=50

if [[ -z "${SERVER_HOST-}" ]]; then
  echo 'The server host must be set.'
fi

echo "Initializing ${SERVER_HOST}:${SERVER_PORT}..."

sleep "${SLEEP}"
for i in $(seq 1 $MAX_ATTEMPTS); do
  echo "Attempt #${i} / ${MAX_ATTEMPTS}:"
  if /opt/mssql-tools/bin/sqlcmd -S "${SERVER_HOST},${SERVER_PORT}" -U SA -P "DockerComposePassword!" -i /init.sql; then
    break
  fi
  echo "Waiting ${SLEEP} seconds..."
  sleep "${SLEEP}"
done

echo Finished attempts.
