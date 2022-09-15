#!/usr/bin/env bash
#
# tag-release.sh
#
# This script is executed before every OSS release.
#
# Usage: ./tag-release.sh <oss_tag>
#
# Example: ./tag-release.sh v2.10.0 release/v2.10
#

# exit on error
set -eo pipefail

# get the repo root
ROOT="$(readlink -f "${BASH_SOURCE[0]%/*}/../" || greadlink -f "${BASH_SOURCE[0]%/*}/../")"

# assign arguments to variables
OSS_TAG="$1"
RELEASE_BRANCH="$2"

# check if required argument is set
if [[ -z "$OSS_TAG" || -z "$RELEASE_BRANCH" ]]; then
    echo "Please mention both OSS_TAG and RELEASE_BRANCH"
    echo ""
    echo "Usage: ./tag-release.sh <oss_tag> <release_branch>"
    exit 1
fi

echo "Please make that your $RELEASE_BRANCH is up-to date with hasura/graphql-engine-mono repo."

# add the latest tag to the catalog_versions file
pushd "$ROOT"
[ -n "$(tail -c1 "$ROOT/server/src-rsr/catalog_versions.txt")" ] && echo >> "$ROOT/server/src-rsr/catalog_versions.txt"
echo "$OSS_TAG $(git show "${RELEASE_BRANCH}:server/src-rsr/catalog_version.txt")" >> "$ROOT/server/src-rsr/catalog_versions.txt"
popd

git add "$ROOT/server/src-rsr"

git commit -m "ci: tag release $OSS_TAG"
