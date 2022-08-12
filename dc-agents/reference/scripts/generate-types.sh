#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$PROJECT_ROOT"

TYPES_DIR="./src/types"
SCHEMA_FILE="$TYPES_DIR/agent.openapi.json"

mkdir -p $TYPES_DIR

if [ ! -f $SCHEMA_FILE ] ; then
    echo "$SCHEMA_FILE does not exist, re-generating it using the agent test suite"
    cabal run test:tests-dc-api -- export-openapi-spec | tail -n 1 | jq > $SCHEMA_FILE
fi

echo "Deleting existing generated model..."
rm -rf "$TYPES_DIR/models"
rm -f "$TYPES_DIR/index.ts"
echo "Generating model from $SCHEMA_FILE..."
openapi --useUnionTypes --input $SCHEMA_FILE --output $TYPES_DIR --exportServices false --exportCore false --indent 2
