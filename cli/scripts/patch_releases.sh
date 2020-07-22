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
  # check if a Github PR is a valid candidate for a patch release
  # this function can take an argument
  # which is expected to be the PR number
  # if this script is running in a circle CI environment then 
  # then PR number will be procured from the corresponding 
  # environment variable
  #
  # the conditions for a PR to be valid patch release are
  # * one an only one file should be changed in the PR
  # * this file should be cli/patch_releases/<semver>.md
  # * PR should have `c/cli` and `k/patch_release` labels

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

  GITHUB_API_BASE_URL="https://api.github.com/repos/hasura/graphql-engine/pulls"
  SEMVER_REGEX='((([0-9]+)\.([0-9]+)\.([0-9]+)(?:-([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)'
  FILE_REGEX="^cli\/patch_releases\/v${SEMVER_REGEX}\.md$"

  NO_OF_FILES=$(curl -s ${GITHUB_API_BASE_URL}/${PR_NUMBER}/files | jq '. | length')
  PR_FILES=$(curl -s ${GITHUB_API_BASE_URL}/${PR_NUMBER}/files | jq )

  # validate number of changed files
  if [[ ! $NO_OF_FILES -eq 1 ]] 
  then
    echo "diff has more than one file change, this is not expected. exiting"
    echo $PR_FILES | jq 
    exit 1
  fi

  # validate a patch_release file was changed  
  FILENAME=$(echo ${PR_FILES} | '.[].filename')
  # if a file inside cli/patch_releases/<semver>.md changed exit with success
  # grep options: -o means output the match, -P means use Perl regex
  if (echo $FILENAME | grep -oP ${FILE_REGEX}) then
    # validate PR has `c/cli` and `k/patch_release` label
    LABEL_COUNT=$(curl -s ${GITHUB_API_BASE_URL/${PR_NUMBER}} | jq '.labels[] | select( .name | contains("c/cli", "k/patch_release")) | .name' | wc -l)
    if [[ ! ${LABEL_COUNT} -eq 2 ]]
    then
      exit 1
    fi
  fi
  
  exit 0
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
