#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$PROJECT_ROOT"

TYPES_PROJECT_DIR="./dc-api-types"
TYPES_DIR="$TYPES_PROJECT_DIR/src"
SCHEMA_FILE="$TYPES_DIR/agent.openapi.json"

mkdir -p $TYPES_DIR

if [ ! -f $SCHEMA_FILE ] ; then
    echo "$SCHEMA_FILE does not exist, re-generating it using the agent test suite"

    if [ -z "$TESTS_DC_API" ]; then
      echo "Expected TEST_DC_API to be set to the path of the tests-dc-api executable"
      exit 1
    fi

    $TESTS_DC_API export-openapi-spec | tail -n 1 | jq . > $SCHEMA_FILE
fi

echo "Deleting existing generated model..."
rm -rf "$TYPES_DIR/models"
rm -f "$TYPES_DIR/index.ts"
echo "Generating model from $SCHEMA_FILE..."
npx openapi --useUnionTypes --input "$SCHEMA_FILE" --output "$TYPES_DIR" --exportServices false --exportCore false --indent 2

cd "$TYPES_PROJECT_DIR"

if ! git diff package.json | grep "+  \"version\":" > /dev/null; then
  echo "Bumping the minor version of dc-api-types..."
  echo "NOTE: If you don't like the new number, change it in dc-api-types' package.json and then run 'make update-api-types-deps'"
  npm version minor
  ../scripts/update-api-types-deps.sh
else
  echo "Skipping dc-api-types version bump since it seems like it has already been changed"
fi
