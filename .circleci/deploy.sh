#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

LATEST_TAG=$(git describe --tags --abbrev=0)
PREVIOUS_TAG=$(git describe --tags $(git rev-list --tags --max-count=2) --abbrev=0 | sed -n 2p)
CHANGELOG_TEXT=""

# reviewers for pull requests opened to update installation manifests
REVIEWERS="shahidhk,coco98,arvi3411301"

IS_STABLE_RELEASE=false
STABLE_SEMVER_REGEX="^v(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)$"
if [ ! -z "${CIRCLE_TAG}" ]; then
    if [[ "$CIRCLE_TAG" =~ $STABLE_SEMVER_REGEX ]]; then
        echo
        echo "this is a stable release"
        echo
        IS_STABLE_RELEASE=true
    fi
fi

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

send_pr_to_repo() {
  configure_git
  git clone https://github.com/hasura/$1.git ~/$1
  cd ~/$1
  git checkout -b ${LATEST_TAG}
  find . -type f -exec sed -i -E 's#(hasura/graphql-engine:)v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(\-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?( \\)*$#\1'"${LATEST_TAG}"'\9#' {} \;
  git add .
  git commit -m "update image version to ${LATEST_TAG}"
  git push -q https://${GITHUB_TOKEN}@github.com/hasura/$1.git ${LATEST_TAG}
  hub pull-request -f -F- <<<"Update image version to ${LATEST_TAG}" -r ${REVIEWERS} -a ${REVIEWERS}
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
    # if this is a stable release, update all latest assets
    if [ $IS_STABLE_RELEASE = true ]; then
        send_pr_to_repo graphql-engine-heroku
    fi

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
