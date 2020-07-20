#!/usr/bin/env bash

set -evo pipefail

ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"
LATEST_TAG=$(git describe --tags `git rev-list --tags --max-count=1`)

PATCH_RELEASE_FILENAME_HEAD="${LATEST_TAG}+cli."
PATCH_NUMBER=1

for PATCH_NUMBER in 1 2 3 4 5 .. 50
do
  if [ ! -f "${ROOT}/cli/patch_releases/${PATCH_RELEASE_FILENAME_HEAD}${PATCH_NUMBER}" ]; then
      break
  fi
done

PATCH_RELEASE_FILENAME=${LATEST_TAG}+cli.${PATCH_NUMBER}

# create patch release file with template
cp ${ROOT}/cli/scripts/patch-release-template.md ${ROOT}/cli/patch_releases/${PATCH_RELEASE_FILENAME}

# TODO: append asset urls to file
