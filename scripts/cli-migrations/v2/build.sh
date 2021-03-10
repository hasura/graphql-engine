#!/usr/bin/env bash

SERVER_BUILD_OUTPUT="$1"
BUILD_OUTPUT="$2"
CLI_MIGRATIONS_IMAGE="cli-migrations-v2"

SERVER_IMAGE=$(docker load -i "${SERVER_BUILD_OUTPUT}/image.tar" | grep "^Loaded image: " | sed "s/Loaded image: //g")
SERVER_IMAGE_TAG=$(echo "$SERVER_IMAGE" | sed "s/.*:\(.*\)$/\1/")

echo "SERVER_IMAGE is ${SERVER_IMAGE}"
echo "SERVER_IMAGE_TAG is ${SERVER_IMAGE_TAG}"

./prepare_docker_context.sh
docker build -t "${CLI_MIGRATIONS_IMAGE}" . --build-arg SERVER_IMAGE_TAG=$SERVER_IMAGE_TAG
docker save -o "${BUILD_OUTPUT}/v2.tar" "${CLI_MIGRATIONS_IMAGE}"