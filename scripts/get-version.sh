#!/usr/bin/env bash

# Outputs a version name for the hasura servers and their components, given the
# checked out state of the repo:
#  - if HEAD has a tag, return that
#  - else return a combination of branch and SHA hash
#
# In both of above we strip any non alpha-numeric, excluding: ._-
#
# This is called in a compile time macro to bake the version into the server
# binaries, and also at various points during CI (and must agree every time).

GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
GIT_SHA="$(git rev-parse --short HEAD)"
# NOTE: a commit may have multiple tags; this should return the most recent
# result of `git tag` if any:
GIT_TAG_EXACT="$(git describe --tags --exact-match --dirty 2>/dev/null)"
GIT_DIRTY=$(test -n "`git status --porcelain`" && echo "-dirty" || echo "")

VERSION="${GIT_TAG_EXACT}"
test -n "$VERSION" || VERSION="${GIT_BRANCH}-${GIT_SHA}${GIT_DIRTY}"

VERSION="$(echo $VERSION | tr -cd '[[:alnum:]]._-')"

# Sanity check: currently some parts of CI reference BUILDKITE_TAG, others call
# this script again and these must agree.
if [ -n "$BUILDKITE_TAG" ] && [ "$VERSION" != "$BUILDKITE_TAG" ]; then
    echo "BUILDKITE_TAG is set as \"$BUILDKITE_TAG\" but somehow does not agree with repo tag \"$VERSION\"!" 1>&2
    exit 1
fi

echo "$VERSION"
