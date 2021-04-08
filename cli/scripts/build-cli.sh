#!/usr/bin/env bash
set -evo pipefail

ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"
PLUGINS_BRANCH="master"
OS="linux darwin windows"
OUTPUT_DIR="${ROOT}/cli/_output"
VERSION=$(../scripts/get-version.sh)

if [ $# -eq 1 ] 
then
  # if an arg is provided use it as version
  VERSION=$1
fi
 
CGO_ENABLED=0 gox -ldflags "-X github.com/hasura/graphql-engine/cli/version.BuildVersion=${VERSION} -X github.com/hasura/graphql-engine/cli/plugins.IndexBranchRef=${PLUGINS_BRANCH} -s -w -extldflags "-static"" \
  -rebuild \
  -os="${OS}" \
  -arch="amd64" \
  -output="${OUTPUT_DIR}/${VERSION}/cli-hasura-{{.OS}}-{{.Arch}}" \
  ./cmd/hasura/
