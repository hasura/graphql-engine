#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841

LANG="$1"
FILE="$2"
# We are generating types from already generated Typescript types. 
# `index.js`` is the file that contains the Typescript types
INDEX_JS="${PROJECT_ROOT}/typescript/src/index.ts" 

# Dir where generated types will be published. 
DIR="${PROJECT_ROOT}/${LANG}"

if [ ! -d "${DIR}" ]; then
    mkdir "${DIR}"
fi

# npm package used to generate types. 
# Use `quicktype --help` to see a list of supported languages. 
# Use `npm i -g quicktype` to install.
quicktype --lang "${LANG}" \
    --out "${DIR}/${FILE}" \
    --src-lang typescript \
    --src "${INDEX_JS}" # add --quiet to silence output
