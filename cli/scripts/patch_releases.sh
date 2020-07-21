#!/usr/bin/env bash

set -evo pipefail

ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"
LATEST_TAG=$(git describe --tags `git rev-list --tags --max-count=1`)

create_patch_release_file() {
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
}

is_patch_release() {
  # expected to run on CI
  PR_NUMBER="5432"
  # check if the number of files is 
  NO_OF_FILES=$(curl -s https://api.github.com/repos/hasura/graphql-engine/pulls/5432/files | jq '. | length')
  FILENAME=$(curl -s https://api.github.com/repos/hasura/graphql-engine/pulls/5432/files | jq '.[].filename')
  echo "FILENAME: ${FILENAME}"
}


if [ "$#" -lt 1 ]; then
    echo "requires arguments"
    exit 1
fi

case $1 in 
  "create-patch-release-file")
    create_patch_release_file
    ;;
  "is_patch_release")
    is_patch_release
    ;;
esac
