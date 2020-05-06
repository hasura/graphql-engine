#!/usr/bin/env bash
#
# tag-release.sh
#
# Update installation manifests with the release tag, and execute other release
# hygiene tasks, makes a commit and tags that commit with the given tag.
#
# Usage: ./tag-release.sh <tag> [<optional-tag-message>]
#
# Example: ./tag-release.sh v1.1.0
#

# exit on error
set -e

# get the repo root
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

# check if required argument is set
if [ -z "$1" ]; then
    echo "Usage: ./tag-release.sh <tag> [<optional-tag-message>]"
    exit 1
fi

# assign arguments to variables
TAG=$1
MESSAGE=$2

# default message to tag
if [ -z "$MESSAGE" ]; then
    MESSAGE="$TAG"
fi

# replace the image version with latest tag for all references in install-manifests
find "$ROOT/install-manifests" \
     "$ROOT/scripts/cli-migrations" \
     -type f -exec sed -i -E \
     's#(hasura/graphql-engine:)v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(\-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(.*)*$#\1'"${TAG}"'\9#' {} \;

# add the latest tag to the catalog_versions file
[ -n "$(tail -c1 "$ROOT/server/src-rsr/catalog_versions.txt")" ] && echo >> "$ROOT/server/src-rsr/catalog_versions.txt"
echo $TAG $(cat "$ROOT/server/src-rsr/catalog_version.txt") >> "$ROOT/server/src-rsr/catalog_versions.txt"

git add "$ROOT/install-manifests" \
        "$ROOT/scripts/cli-migrations" \
        "$ROOT/server/src-rsr"

git commit -m "tag release $TAG"

git tag -a "$TAG" -m "$MESSAGE"

echo "tagged $TAG"
