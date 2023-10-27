#!/usr/bin/env bash
#
# post-release.sh
#
# Used after a release to update the latest stable version
# at various places in our codebase
#
# Usage: ./post-release.sh <oss_tag>
#
# Example: ./post-release.sh v2.2.0
#

set -e

# get the repo root
ROOT="$(readlink -f "${BASH_SOURCE[0]%/*}/../")"

TAG="$1"

if [ -z "$TAG" ]; then
    echo "Usage: ./post-release.sh <oss_tag>"
    exit 1
fi

# replace the image version with latest tag for all references in install-manifests
find "$ROOT/install-manifests" \
     -type f -exec sed -i -E \
     's#(hasura/graphql-engine:)v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(\-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(.*)*$#\1'"${TAG}"'\9#' {} \;

# replace the data-connector version with latest tag for all references in install-manifests
find "$ROOT/install-manifests" \
     -type f -exec sed -i -E \
     's#(hasura/graphql-data-connector:)v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(\-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(.*)*$#\1'"${TAG}"'\9#' {} \;


# update version in CLI installation instructions
sed -i -E 's#(.*)v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(\-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(.*)*$#\1'"${TAG}"'\9#' \
    "${ROOT}/cli/README.md" \
    "${ROOT}/cli/get.sh" \
    "${ROOT}/docs/docs/hasura-cli/install-hasura-cli.mdx"

## update version in CI image scanning tags
#sed -i -E 's#v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)#'"${TAG}"'#' \
#    "${ROOT}/.buildkite/pipeline-gen/pipeline/scan_graphql_engine_images.go" \
#    "${ROOT}/.buildkite/pipeline-gen/pipeline/scan_graphql_engine_pro_images.go"


git add "$ROOT/install-manifests" \
        "${ROOT}/cli" \
        "${ROOT}/docs/docs/hasura-cli/install-hasura-cli.mdx"
#        "${ROOT}/.buildkite/pipeline-gen/pipeline/scan_graphql_engine_images.go" \
#        "${ROOT}/.buildkite/pipeline-gen/pipeline/scan_graphql_engine_pro_images.go"

git commit -m "ci: update latest stable release as $TAG"

echo "updated $TAG"
