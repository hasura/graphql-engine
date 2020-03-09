#!/usr/bin/env bash

GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
GIT_SHA="$(git rev-parse --short HEAD)"
GIT_TAG_EXACT="$(git describe --tags --exact-match --dirty 2>/dev/null)"
GIT_DIRTY=$(test -n "`git status --porcelain`" && echo "-dirty" || echo "")

VERSION="${GIT_TAG_EXACT}"
test -n "$VERSION" || VERSION="${GIT_BRANCH}-${GIT_SHA}${GIT_DIRTY}"

VERSION="$(echo $VERSION | tr -cd '[[:alnum:]]._-')"

echo "$VERSION"