#!/usr/bin/env bash

set -evo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"

export VERSION=$(${ROOT}/scripts/get-version.sh)
export BUCKET_URL=https://github.com/hasura/graphql-engine/releases/download/${VERSION}
export LINUX_SHA256=$(cat ${ROOT}/cli-ext/bin/cli-ext-hasura-linux.tar.gz.sha256 | cut -f1 -d' ')
export MACOS_SHA256=$(cat ${ROOT}/cli-ext/bin/cli-ext-hasura-macos.tar.gz.sha256 | cut -f1 -d' ')
export WINDOWS_SHA256=$(cat ${ROOT}/cli-ext/bin/cli-ext-hasura-win.zip.sha256 | cut -f1 -d' ')

( echo "cat <<EOF >${ROOT}/cli-ext/bin/manifest.yaml";
  cat ${ROOT}/cli-ext/scripts/manifest.yaml;
) >${ROOT}/cli-ext/bin/tmp.yaml
. ${ROOT}/cli-ext/bin/tmp.yaml

export BUCKET_URL=https://${CIRCLE_BUILD_NUM}-137724480-gh.circle-artifacts.com/0/cli_ext

( echo "cat <<EOF >${ROOT}/cli-ext/bin/manifest-dev.yaml";
  cat ${ROOT}/cli-ext/scripts/manifest.yaml;
) >${ROOT}/cli-ext/bin/tmp.yaml
. ${ROOT}/cli-ext/bin/tmp.yaml