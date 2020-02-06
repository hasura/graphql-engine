#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

LATEST_TAG=$(git describe --tags --abbrev=0)
PREVIOUS_TAG=$(git describe --tags $(git rev-list --tags --max-count=2) --abbrev=0 | sed -n 2p)
CHANGELOG_TEXT=""

# reviewers for pull requests opened to update installation manifests
REVIEWERS="shahidhk,coco98,arvi3411301"

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
  echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USER" --password-stdin
  make push-latest
}

draft_github_release() {
    cd "$ROOT"
    echo "drafting github release"
    ghr -t "$GITHUB_TOKEN" \
        -u "$CIRCLE_PROJECT_USERNAME" \
        -r "$CIRCLE_PROJECT_REPONAME" \
        -b "${RELEASE_BODY}" \
        -draft \
     "$CIRCLE_TAG" /build/_cli_output/binaries/
}

configure_git() {
  git config --global user.email "build@hasura.io"
  git config --global user.name "hasura-bot"
}

send_pr_to_repo() {
  git clone https://github.com/hasura/$1.git ~/$1
  cd ~/$1
  git checkout -b ${LATEST_TAG}
  find . -type f -exec sed -i -E 's#(hasura/graphql-engine:)v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(\-[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?(\+[0-9A-Za-z-]+(\.[0-9A-Za-z-]+)*)?( \\)*$#\1'"${LATEST_TAG}"'\9#' {} \;
  git add .
  git commit -m "update image version to ${LATEST_TAG}"
  git push -q https://${GITHUB_TOKEN}@github.com/hasura/$1.git ${LATEST_TAG}
  hub pull-request -F- <<<"Update image version to ${LATEST_TAG}" -r ${REVIEWERS} -a ${REVIEWERS}
}

deploy_console() {
    echo "deploying console"

    cd "$ROOT/console"
    export VERSION=$(../scripts/get-console-assets-version.sh)
    export DIST_PATH="/build/_console_output"
    local GS_BUCKET_ROOT="gs://graphql-engine-cdn.hasura.io/console/assets/$VERSION"
    # assets are at /build/_console_output/assets/versioned, already gzipped
    gsutil cp "$DIST_PATH/assets/versioned/main.js.gz" "$GS_BUCKET_ROOT/main.js.gz"
    gsutil cp "$DIST_PATH/assets/versioned/main.css.gz" "$GS_BUCKET_ROOT/main.css.gz"
    gsutil cp "$DIST_PATH/assets/versioned/vendor.js.gz" "$GS_BUCKET_ROOT/vendor.js.gz"
    gsutil setmeta -h "Content-Encoding: gzip" "$GS_BUCKET_ROOT/*"

    unset VERSION
    unset DIST_PATH
}

# build and push container for auto-migrations
build_and_push_cli_migrations_image() {
    IMAGE_TAG="hasura/graphql-engine:${CIRCLE_TAG}.cli-migrations"
    LATEST_IMAGE_TAG="hasura/graphql-engine:latest.cli-migrations"
    cd "$ROOT/scripts/cli-migrations"
    cp /build/_cli_output/binaries/cli-hasura-linux-amd64 .
    docker build -t "$IMAGE_TAG" .
    docker push "$IMAGE_TAG"

    # push latest.cli-migrations tag
    docker tag "$IMAGE_TAG" "$LATEST_IMAGE_TAG"
    docker push "$LATEST_IMAGE_TAG"
}

# copy docker-compose-https manifests to gcr for digital ocean one-click app
deploy_do_manifests() {
    gsutil cp "$ROOT/install-manifests/docker-compose-https/docker-compose.yaml" \
           gs://graphql-engine-cdn.hasura.io/install-manifests/do-one-click/docker-compose.yaml
    gsutil cp "$ROOT/install-manifests/docker-compose-https/Caddyfile" \
           gs://graphql-engine-cdn.hasura.io/install-manifests/do-one-click/Caddyfile
}

# setup gcloud cli tool
setup_gcloud() {
    echo $GCLOUD_SERVICE_KEY > ${HOME}/gcloud-service-key.json
    gcloud auth activate-service-account --key-file=${HOME}/gcloud-service-key.json
    gcloud --quiet config set project ${GOOGLE_PROJECT_ID}
}

# push the server binary to google cloud storage
push_server_binary() {
    gsutil cp /build/_server_output/graphql-engine \
              gs://graphql-engine-cdn.hasura.io/server/latest/linux-amd64
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

setup_gcloud

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
    push_server_binary
    build_and_push_cli_migrations_image
    CHANGELOG_TEXT=$(changelog server)
    CHANGELOG_TEXT+=$(changelog cli)
    CHANGELOG_TEXT+=$(changelog console)
    RELEASE_BODY=$(eval "cat <<EOF
$(<$ROOT/.circleci/release_notes.template.md)
EOF
")
    draft_github_release
    configure_git
    send_pr_to_repo graphql-engine-heroku
    deploy_do_manifests
fi
