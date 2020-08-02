#!/usr/bin/env bash

set -evo pipefail

ROOT="$(readlink -f "${BASH_SOURCE[0]%/*}"/../../)"
LATEST_TAG=$(git describe --tags $(git rev-list --tags --max-count=1))

PATCH_RELEASE_FILENAME_HEAD="${LATEST_TAG}+cli."
PATCH_RELEASE_FILENAME_TAIL=".md"
PATCH_RELEASES_DIRECTORY="${ROOT}/cli/patch_releases"

PATCH_RELEASES_GCLOUD_BUCKET="gs://hasura-oss-cli-cdn/test-releases"
OUTPUT_DIR="${ROOT}/cli/_output"

create_patch_release_file() {
  PATCH_NUMBER=1

  for PATCH_NUMBER in 1 2 3 4 5 .. 50
  do
    if [ ! -f "${PATCH_RELEASES_DIRECTORY}/${PATCH_RELEASE_FILENAME_HEAD}${PATCH_NUMBER}${PATCH_RELEASE_FILENAME_TAIL}" ]; then
        break
    fi
  done

  PATCH_RELEASE_FILENAME=${PATCH_RELEASE_FILENAME_HEAD}${PATCH_NUMBER}${PATCH_RELEASE_FILENAME_TAIL}

  # create patch release file with template
  cp "${ROOT}/cli/scripts/patch-release-template.md" "${PATCH_RELEASES_DIRECTORY}/${PATCH_RELEASE_FILENAME}"
  
  # add assets links
  CDN_ROOT_URL="https://storage.googleapis.com/hasura-oss-cli-cdn/test-releases/${PATCH_RELEASE_FILENAME_HEAD}${PATCH_NUMBER}"
  echo [linux]'('${CDN_ROOT_URL}/cli-hasura-linux-amd64')' >> "${PATCH_RELEASES_DIRECTORY}/${PATCH_RELEASE_FILENAME}"
  echo [macos]'('${CDN_ROOT_URL}/cli-hasura-darwin-amd64')' >> "${PATCH_RELEASES_DIRECTORY}/${PATCH_RELEASE_FILENAME}" 
  echo [windows]'('${CDN_ROOT_URL}/cli-hasura-windows-amd64.exe')' >> "${PATCH_RELEASES_DIRECTORY}/${PATCH_RELEASE_FILENAME}" 
}

is_patch_release() {
  if ! command -v hub&> /dev/null
  then
    echo "installing hub"
      apt update && apt install -y hub
    exit
  fi

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
  echo "checking if PR ${PR_NUMBER} is a patch release"

  GITHUB_API_BASE_URL="https://api.github.com/repos/scriptonist/temp-hasura-patch-releases-cli/pulls"
  SEMVER_REGEX='((([0-9]+)\.([0-9]+)\.([0-9]+)(?:-([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)'
  FILE_REGEX="^cli\/patch_releases\/v${SEMVER_REGEX}\.md$"

  NO_OF_FILES=$( hub api ${GITHUB_API_BASE_URL}/${PR_NUMBER}/files | jq '. | length')
  PR_FILES=$(hub api "${GITHUB_API_BASE_URL}/${PR_NUMBER}/files" | jq)

  # validate number of changed files
  if [[ ! $NO_OF_FILES -eq 1 ]]
  then
    echo "diff has more than one file change, this is not expected. exiting"
    echo "$PR_FILES" | jq
    exit 1
  fi

  # validate a patch_release file was changed
  FILENAME=$(echo "${PR_FILES}" | jq --raw-output '.[] | .filename')
  echo "checking if ${FILENAME} matches the required regex"
  # if a file inside cli/patch_releases/<semver>.md changed exit with success
  # grep options: -o means output the match, -P means use Perl regex
  if (echo "${FILENAME}" | grep -oP "${FILE_REGEX}") then
    # validate PR has `c/cli` and `k/patch_release` label
    echo "checking if PR has the required labels"
    LABEL_COUNT=$(hub api ${GITHUB_API_BASE_URL}/${PR_NUMBER} | jq '.labels[] | select( .name | contains("c/cli", "k/patch_release")) | .name' | wc -l)
    if [[ ! ${LABEL_COUNT} -eq 2 ]]
    then
      exit 1
    fi
  else
    exit 1
  fi

  echo "voila this is a patch release!"
  exit 0
}

build_and_push_patch_release() {
  echo "starting patch release process"
  exit 1
  # find the latest patch release file
  PATCH_NUMBER=1

  for (( PATCH_NUMBER=50; PATCH_NUMBER > 0; --PATCH_NUMBER ))
  do
    FILENAME="${PATCH_RELEASES_DIRECTORY}/${PATCH_RELEASE_FILENAME_HEAD}${PATCH_NUMBER}${PATCH_RELEASE_FILENAME_TAIL}"
    if [ -f  "${FILENAME}" ]; then
        PATCH_VERSION_MD=$(basename -- "$FILENAME")
        PATCH_VERSION="${PATCH_VERSION_MD%.*}"
        break
    fi
  done
  if [ $PATCH_NUMBER -eq 0 ]
  then
    echo "no patch release found for ${LATEST_TAG}"
    exit 1
  fi
  echo "building binaries for patch release " "${PATCH_VERSION}"
  # build binaries
  "${ROOT}"/cli/scripts/build-cli.sh "${PATCH_VERSION}"
	ls "${OUTPUT_DIR}"/"${PATCH_VERSION}"/cli-hasura-* | xargs upx

  # push binaries to gcloud bucket
  gsutil -m cp -r "${ROOT}"/cli/_output/"${PATCH_VERSION}/*" ${PATCH_RELEASES_GCLOUD_BUCKET}/"${PATCH_VERSION}/"
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
    is_patch_release "$2"
    ;;
  "build_and_push_patch_release")
    build_and_push_patch_release
    ;;
esac

