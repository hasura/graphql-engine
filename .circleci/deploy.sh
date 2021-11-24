#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

LATEST_TAG=$(git describe --tags --abbrev=0)
PREVIOUS_TAG=$(git describe --tags $(git rev-list --tags --max-count=2) --abbrev=0 | sed -n 2p)
CHANGELOG_TEXT=""

changelog() {
  CHANGELOG=$(git log ${PREVIOUS_TAG}..${LATEST_TAG} --pretty="tformat:- $1: %s" --reverse -- $ROOT/$1)
  if [ -n "$CHANGELOG" ]
  then
      if [ -n "$CHANGELOG_TEXT" ]
      then
          echo ""
      fi
      echo "${CHANGELOG}"
  fi
}

draft_github_release() {
    cd "$ROOT"
    export GITHUB_REPOSITORY="${CIRCLE_PROJECT_USERNAME}/${CIRCLE_PROJECT_REPONAME}"
    echo "drafting github release"
    hub release create \
        --draft \
        -a /build/_cli_output/binaries/cli-hasura-darwin-amd64 \
        -a /build/_cli_output/binaries/cli-hasura-linux-amd64 \
        -a /build/_cli_output/binaries/cli-hasura-darwin-arm64 \
        -a /build/_cli_output/binaries/cli-hasura-linux-arm64 \
        -a /build/_cli_output/binaries/cli-hasura-windows-amd64.exe \
        -m "$CIRCLE_TAG" \
        -m "${RELEASE_BODY}" \
     "$CIRCLE_TAG"

    unset GITHUB_REPOSITORY
}

configure_git() {
  git config --global user.email "build@hasura.io"
  git config --global user.name "hasura-bot"
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

if [[ ! -z "$CIRCLE_TAG" ]]; then
    # submit a release draft to github
    # build changelog
    CHANGELOG_TEXT=$(changelog server)
    CHANGELOG_TEXT+=$(changelog cli)
    CHANGELOG_TEXT+=$(changelog console)
    RELEASE_BODY=$(eval "cat <<EOF
$(<$ROOT/.circleci/release_notes.template.md)
EOF
")
    draft_github_release
fi
