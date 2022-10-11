#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$PROJECT_ROOT"

TYPES_PROJECT_DIR="./dc-api-types"
PROJECT_DIR_NAMES=( "reference" "sqlite" )

TYPES_VERSION=$( jq '.version' "$TYPES_PROJECT_DIR/package.json" )
echo "Updating projects dependant on API types to version $TYPES_VERSION..."

for project in "${PROJECT_DIR_NAMES[@]}"; do
  PROJECT_DIR="./$project"
  echo "Updating $project..."

  TMP_FILE="$( mktemp )"
  jq ".dependencies[\"@hasura/dc-api-types\"] = $TYPES_VERSION" "$PROJECT_DIR/package.json" > "$TMP_FILE"
  mv -f "$TMP_FILE" "$PROJECT_DIR/package.json"
done

npm install
make derive-lockfiles

echo "Done updating API types version in dependant projects"
