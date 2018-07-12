#!/usr/bin/env bash

set -eo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

## deploy functions
deploy_server() {
    echo "deploying server"
    cd "$ROOT/server"
    docker login -u "$DOCKER_USER" -p "$DOCKER_PASSWORD"
    make ci-load-image
    make push
}

deploy_server_latest() {
  echo "deloying server latest tag"
  cd "$ROOT/server"
  docker login -u "$DOCKER_USER" -p "$DOCKER_PASSWORD"
  make push-latest
}

# TODO: open pull requests

draft_github_release() {
    cd "$ROOT"
    echo "drafting github release"
    ghr -t "$GITHUB_TOKEN" \
        -u "$CIRCLE_PROJECT_USERNAME" \
        -r "$CIRCLE_PROJECT_REPONAME" \
        -b "$CIRCLE_TAG" \
        -draft \
     "$CIRCLE_TAG" /build/_cli_output/binaries/
}

deploy_console() {
    echo "deploying console"
    echo $GCLOUD_SERVICE_KEY > ${HOME}/gcloud-service-key.json
    gcloud auth activate-service-account --key-file=${HOME}/gcloud-service-key.json
    gcloud --quiet config set project ${GOOGLE_PROJECT_ID}

    cd "$ROOT/console"
    export VERSION=$(../scripts/get-version-circleci.sh)
    export DIST_PATH="/build/_console_output"
    make gzip-assets
    make gcloud-cp-stable
    make gcloud-set-metadata
    unset VERSION
    unset DIST_PATH
}
# skip deploy for pull requests
if [[ -n "${CIRCLE_PR_NUMBER:-}" ]]; then
    echo "not deploying for PRs"
    exit
fi

# required env vars
# DOCKER_USER
# DOCKER_PASSWORD
# GITHUB_TOKEN
# GCLOUD_SERVICE_KEY
# CIRCLE_PROJECT_USERNAME
# CIRCLE_PROJECT_REPONAME
# CIRCLE_TAG
# CIRCLE_PR_NUMBER
# CIRCLE_BRANCH

RELEASE_BRANCH_REGEX="^release-v(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)$"
if [[ "$CIRCLE_BRANCH" =~ $RELEASE_BRANCH_REGEX ]]; then
    # release branch, only update console
    echo "release branch, only deploying console"
    deploy_console
    exit
fi

deploy_console
deploy_server
if [[ ! -z "$CIRCLE_TAG" ]]; then
    deploy_server_latest
    draft_github_release
fi
