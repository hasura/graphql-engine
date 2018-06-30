#!/usr/bin/env bash

GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
GIT_TAG="$(git describe --tags --dirty 2>/dev/null)"
GIT_TAG_EXACT="$(git describe --tags --exact-match --dirty 2>/dev/null)"

VERSION="${GIT_TAG_EXACT}"
test -n "$VERSION" || VERSION="${GIT_TAG}-${GIT_BRANCH}"

echo "$VERSION"
