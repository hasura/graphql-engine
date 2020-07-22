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
  cp ${ROOT}/cli/scripts/patch-release-template.md ${ROOT}/cli/patch_releases/${PATCH_RELEASE_FILENAME}.md

  # TODO: append asset urls to file
}

is_patch_release() {
  if [ -n "${CIRCLECI}" ]; then
    echo "getting PR number from circle CI"
    PR_NUMBER=${CIRCLE_PR_NUMBER}
  else
    # get PR number as an argument
    PR_NUMBER=$1
  fi
  
  if [ ! -n "${PR_NUMBER}" ]; then
    echo "cannot determine PR number :( exiting"
    exit 1
  fi

  SEMVER_REGEX='((([0-9]+)\.([0-9]+)\.([0-9]+)(?:-([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)'
  FILE_REGEX="^cli\/patch_releases\/v${SEMVER_REGEX}\.md$"

  # check if the number of files is (using github API)
  NO_OF_FILES=$(curl -s https://api.github.com/repos/hasura/graphql-engine/pulls/${PR_NUMBER}/files | jq '. | length')
  PR_JSON=$(curl -s https://api.github.com/repos/hasura/graphql-engine/pulls/${PR_NUMBER}/files | jq )

  if [[ ! $NO_OF_FILES -eq 1 ]] 
  then
    echo "diff has more than one file change, this is not expected. exiting"
    echo $PR_JSON | jq 
    exit 1
  fi

  FILENAME=$(echo ${PR_JSON} | '.[].filename')
  
  # if a file inside cli/patch_releases/<semver>.md changed exit with success
  # grep options: -o means output the match, -P means use Perl regex
  if (echo $FILENAME | grep -oP ${FILE_REGEX}) then
    exit 0
  fi
  
  exit 1
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
    is_patch_release $2
    ;;
esac
