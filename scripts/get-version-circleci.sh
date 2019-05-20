#!/usr/bin/env bash

set -eo pipefail
IFS=$'\n\t'

ROOT="${BASH_SOURCE[0]%/*}"

SEMVER_REGEX="^v(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)(\\-[0-9A-Za-z-]+(\\.[0-9A-Za-z-]+)*)?(\\+[0-9A-Za-z-]+(\\.[0-9A-Za-z-]+)*)?$"
RELEASE_BRANCH_REGEX="^release-v(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)$"

VERSION=""
if [ ! -z "${CIRCLE_TAG}" ]; then
    if [[ "$CIRCLE_TAG" =~ $SEMVER_REGEX ]]; then
        major=${BASH_REMATCH[1]}
        minor=${BASH_REMATCH[2]}
        VERSION="v$major.$minor"
    fi
elif  [ ! -z "$CIRCLE_BRANCH" ]; then
    if [[ "$CIRCLE_BRANCH" =~ $RELEASE_BRANCH_REGEX ]]; then
        major=${BASH_REMATCH[1]}
        minor=${BASH_REMATCH[2]}
        VERSION="v$major.$minor"
    fi
fi

if [ -z "$VERSION" ]; then VERSION="$($ROOT/get-version.sh)"; fi

VERSION="$(echo $VERSION | tr -cd '[[:alnum:]]._-')"

echo $VERSION
