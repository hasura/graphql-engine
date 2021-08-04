#!/usr/bin/env bash

CONTAINER_NAME=graphql-remote-server

echo "Stopping graphql remote server container: $CONTAINER_NAME"
docker stop $CONTAINER_NAME
