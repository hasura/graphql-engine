#!/usr/bin/env bash

set -evo pipefail 

# check if yq is installed
if ! command -v yq &> /dev/null
then
    curl -LO https://github.com/mikefarah/yq/releases/download/3.3.2/yq_linux_amd64 && \ 
      chmod +x yq_linux_amd64 && \ 
      mv yq_linux_amd64 /usr/local/bin/yq
fi

BUILD_DIR=/build
BINARY=${BUILD_DIR}/_cli_output/binaries/cli-hasura-linux-amd64
CLI_EXT_BINARY_NAME=cli-ext-hasura-linux.tar.gz
CLI_EXT_LINUX_BINARY_PATH=${BUILD_DIR}/_cli_ext_output/${CLI_EXT_BINARY_NAME}
CLI_EXT_MANIFEST_FILE=${BUILD_DIR}/_cli_ext_output/manifest-dev.yaml
TEMPLATE_CLI_EXT_INDEX_DIR='hasura-home-dir-tmpl/index/plugins/cli-ext'

cp ${BINARY} .
# copy linux binary 
cp ${CLI_EXT_LINUX_BINARY_PATH} .

# edit manifest file cli-ext linux uri to file:///tmp/cli-ext-hasura-linux.tar.gz
yq write -i ${CLI_EXT_MANIFEST_FILE} "platforms[0].uri" "file:///tmp/cli-ext/${CLI_EXT_BINARY_NAME}"
cp ${CLI_EXT_MANIFEST_FILE} manifest.yaml

# edit hasura home template directory
CLI_EXT_VERSION=$(yq read manifest.yaml version)
mkdir -p ${TEMPLATE_CLI_EXT_INDEX_DIR}/${CLI_EXT_VERSION}
cp manifest.yaml ${TEMPLATE_CLI_EXT_INDEX_DIR}/${CLI_EXT_VERSION}/manifest.yaml
