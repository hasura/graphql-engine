#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CONSOLE_ROOT="${BASH_SOURCE[0]%/*}/../console"

cd "$CONSOLE_ROOT"

#./node_modules/cypress/bin/cypress install
./node_modules/.bin/cypress run --env $TEST_ENV --config $TEST_CONFIG --spec $TEST_SPECS
