#!/usr/bin/env bash

CONTAINER_NAME=graphql-remote-server
PORT=5000
echo "Launching graphql remote server container: $CONTAINER_NAME"
docker run \
    --name "$CONTAINER_NAME" \
    -v $(pwd)/graphql_server:/app \
    -w /app \
    -p "$PORT":$PORT \
    --rm \
    -d python:3.8-slim-buster \
    ./run_graphql_server.sh

until curl -s "http://127.0.0.1:$PORT" &>/dev/null; do
    echo -n '.' && sleep 0.2
done
