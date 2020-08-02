#!/usr/bin/env bash

set -evo pipefail

ROOT="$(readlink -f "${BASH_SOURCE[0]%/*}"/../../)"
LATEST_TAG=$(git describe --tags $(git rev-list --tags --max-count=1))

PATCH_RELEASE_FILENAME_HEAD="${LATEST_TAG}+cli."
PATCH_RELEASE_FILENAME_TAIL=".md"
PATCH_RELEASES_DIRECTORY="${ROOT}/cli/patch_releases"

PATCH_RELEASES_GCLOUD_BUCKET="gs://hasura-oss-cli-cdn/patch-releases"
OUTPUT_DIR="${ROOT}/cli/_output"

GITHUB_REPO_OWNER="hasura"
GITHUB_REPO_NAME="graphql-engine"

# install required tools
if ! command -v hub&> /dev/null
then
  cd /tmp

  echo "installing hub"
  curl -LO https://github.com/github/hub/releases/download/v2.14.2/hub-linux-amd64-2.14.2.tgz
  tar -xvf hub-linux-amd64-2.14.2.tgz
  mv hub-linux-amd64-2.14.2/bin/hub /usr/bin/hub

  cd -
fi

if ! command -v jq&> /dev/null
then
  echo "installing jq"
  apt-get -qq update && apt-get -qq install -y jq
fi

# validate required environment variables
if [ -z "${GITHUB_USER}" ]
then
      echo "requires GITHUB_USER environment variable to be set"
      exit 1
fi

if [ -z "${GITHUB_TOKEN}" ]
then
      echo "requires GITHUB_TOKEN environment variable to be set"
      exit 1
fi

# setup gcloud cli tool
setup_gcloud() {
    echo "$GCLOUD_SERVICE_KEY" > "${HOME}"/gcloud-service-key.json
    gcloud auth activate-service-account --key-file="${HOME}"/gcloud-service-key.json
    gcloud --quiet config set project "${GOOGLE_PROJECT_ID}"
}

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
    PR_NUMBER=${CIRCLE_PR_NUMBER:-${CIRCLE_PULL_REQUEST##*/}}
  else
    # get PR number as an argument
    PR_NUMBER=$1
  fi
  
  if [ ! -n "${PR_NUMBER}" ]; then
    echo "cannot determine PR number, seems like this is not meant to be a patch release"
    exit 0
  fi
  echo "checking if PR ${PR_NUMBER} is a patch release"
  
  GITHUB_API_BASE_URL="https://api.github.com/repos/${GITHUB_REPO_OWNER}/${GITHUB_REPO_NAME}/pulls"
  SEMVER_REGEX='((([0-9]+)\.([0-9]+)\.([0-9]+)(?:-([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?)'
  FILE_REGEX="^cli\/patch_releases\/v${SEMVER_REGEX}\.md$"

  echo "getting details of PR"
  hub api "${GITHUB_API_BASE_URL}/${PR_NUMBER}/files"
  PR_FILES=$(hub api "${GITHUB_API_BASE_URL}/${PR_NUMBER}/files")
  NO_OF_FILES=$(echo "${PR_FILES}" | jq '. | length')
  echo ${NO_OF_FILES} ${PR_FILES}
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

  echo "this is a patch release!"
  touch /tmp/is_patch_release
  exit 0
}

build_and_push_patch_release() {
  if ! command -v gsutil&> /dev/null
  then
    GCLOUD_VERSION="207.0.0"
    curl -Lo /tmp/gcloud-${GCLOUD_VERSION}.tar.gz https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz
    tar -xzf /tmp/gcloud-${GCLOUD_VERSION}.tar.gz -C /usr/local
    /usr/local/google-cloud-sdk/install.sh --quiet
    export PATH="/usr/local/google-cloud-sdk/bin:$PATH"
  fi
  setup_gcloud

  echo "starting patch release process"
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

  echo "${PATCH_VERSION}" > /tmp/patch_release_version
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

