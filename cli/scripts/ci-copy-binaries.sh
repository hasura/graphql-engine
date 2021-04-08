#!/usr/bin/env bash

set -evo pipefail

ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../../)"
OUTPUT_DIR="${ROOT}/cli/_output"
VERSION=$("${ROOT}"/scripts/get-version.sh)

if [ -n "${1}" ]
then
  VERSION=${1}
fi

echo "version: ${VERSION}"
mkdir -p /build/_cli_output/binaries
cp ${OUTPUT_DIR}/${VERSION}/cli-hasura-* /build/_cli_output/binaries
echo "${VERSION}" > /build/_cli_output/version.txt
