#!/usr/bin/env bash
set -euo pipefail

# You can use this to update the mimalloc source code vendored here, if you
# wish to upgrade the version for instance. This should be idempotent.
#
# NOTE!: v2.1.2 regresses from our point of view. See:
# https://github.com/microsoft/mimalloc/issues/776
VERSION=v2.1.1
 

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"   # ... https://stackoverflow.com/a/246128/176841
cd "$THIS_DIR"

rm -rf mimalloc
git clone git@github.com:microsoft/mimalloc.git 2>/dev/null
(
cd mimalloc
git checkout "$VERSION" 2>/dev/null
# stuff we don't need:
rm -rf .git docs doc cmake bin azure-pipelines.yml CMakeLists.txt .gitattributes .gitignore ide mimalloc.pc.in test
) # in mimalloc/

echo "Done. $VERSION vendored in 'mimalloc/'"
