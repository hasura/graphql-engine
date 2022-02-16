#!/usr/bin/env bash
#
# tag-release.sh
#
# This script is executed before every OSS and Pro releases.
#
# Usage: ./tag-release.sh <oss_tag> <pro_tag>
#
# Example: ./tag-release.sh v1.1.0 v1.1.0-pro.1
#

# exit on error
set -eo pipefail

# get the repo root
ROOT="$(readlink -f "${BASH_SOURCE[0]%/*}/../")"

# assign arguments to variables
OSS_TAG="$1"
PRO_TAG="$2"

# check if required argument is set
if [[ -z "$OSS_TAG" || -z "$PRO_TAG" ]]; then
    echo "Please mention both OSS_TAG and PRO_TAG"
    echo ""
    echo "Usage: ./tag-release.sh <oss_tag> <pro_tag>"
    exit 1
fi

# add the latest tag to the catalog_versions file
[ -n "$(tail -c1 "$ROOT/server/src-rsr/catalog_versions.txt")" ] && echo >> "$ROOT/server/src-rsr/catalog_versions.txt"
echo "$OSS_TAG $(cat "$ROOT/server/src-rsr/catalog_version.txt")" >> "$ROOT/server/src-rsr/catalog_versions.txt"

# update OSS changelog
OSS_VERSION_ENTRY="## Next release\n\n### Bug fixes and improvements\n\n## $OSS_TAG"
sed -i "s/## Next release/${OSS_VERSION_ENTRY}/" "$ROOT/CHANGELOG.md"

# update Pro changelog
PRO_NEXT_RELEASE_TEXT="## Next release\n(Add entries below in the order of multitenant, server, console, cli, docs, others)"
sed -i "N;s/${PRO_NEXT_RELEASE_TEXT}/${PRO_NEXT_RELEASE_TEXT}\n\n## ${PRO_TAG}/" "$ROOT/pro/CHANGELOG.md"

git add "$ROOT/server/src-rsr" \
    "$ROOT/CHANGELOG.md" \
    "$ROOT/pro/CHANGELOG.md"

git commit -m "ci: tag release $OSS_TAG and $PRO_TAG"

git tag -a "$OSS_TAG" -m "tag release $OSS_TAG"
git tag -a "$PRO_TAG" -m "tag release $PRO_TAG"

echo "tagged $OSS_TAG and $PRO_TAG"
