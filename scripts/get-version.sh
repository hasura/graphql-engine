#!/usr/bin/env bash

GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
GIT_SHA="$(git rev-parse --short HEAD)"
GIT_TAG="$(git describe --tags 2>/dev/null)"
GIT_TAG_EXACT="$(git describe --tags --exact-only 2>/dev/null)"
GIT_DIRTY=$(test -n "`git status --porcelain`" && echo "-dirty" || echo "")
VERSION="${GIT_TAG_EXACT}"

if [ -z "$GIT_TAG" ]; then
    VERSION="${GIT_BRANCH}-${GIT_SHA}${GIT_DIRTY}"
fi

if [ -z "$GIT_TAG_EXACT" ]; then
    VERSION="${GIT_TAG}-${GIT_BRANCH}${GIT_DIRTY}"
fi

echo "$VERSION"
