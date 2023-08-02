#!/usr/bin/env bash
set -euo pipefail

# Get path to schema file as an argument, and expand the path relative to the
# working directory the script was run from. 
SCHEMA_FILE=$(readlink -f "$1")

PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$PROJECT_ROOT/typescript"

echo "Deleting existing generated model..."
rm -rf src
echo "Generating model from $SCHEMA_FILE..."
npx openapi \
  --useUnionTypes \
  --input "$SCHEMA_FILE" \
  --output src \
  --exportServices false \
  --exportCore false \
  --indent 2

# To make a patch, commit the unpatched file, make the changes you want, and run
# `git diff > patches/my-patch.patch`
echo "Applying patches to generated files..."
for PATCH in patches/*.patch; do
  # Don't crash if there are no patches.
  if [ -f "$PATCH" ]; then
    git apply "$PATCH"
  fi
done
