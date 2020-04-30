#!/usr/bin/env bash

set -eo pipefail
IFS=$'\n\t'

ROOT="${BASH_SOURCE[0]%/*}"

SEMVER_REGEX="v?([0-9]+)(\.[0-9]+)?(\.[0-9]+)?(-([0-9A-Za-z\-]+(\.[0-9A-Za-z\-]+)*))?(\+([0-9A-Za-z\-]+(\.[0-9A-Za-z\-]+)*))?"
CHANNEL_REGEX="^[a-z]+"
LATEST_TAG=$(git describe --tags --abbrev=0)

VERSION=""
channel="stable"

if [[ "$LATEST_TAG" =~ $SEMVER_REGEX ]]; then
    major=${BASH_REMATCH[1]}
    minor=${BASH_REMATCH[2]}
    release=${BASH_REMATCH[5]}
    if [[ "$release" =~ $CHANNEL_REGEX ]]; then
        channel="${BASH_REMATCH[0]}"
    fi
    if [[ ! -z "$EXPECTED_CHANNEL" ]] && [[ "${EXPECTED_CHANNEL}" != "${channel}" ]]; then
        exit
    fi
    VERSION="channel/$channel/v$major$minor"
fi

if [ -z "$VERSION" ]; then VERSION="versioned/$($ROOT/get-version.sh)"; fi

echo $VERSION
