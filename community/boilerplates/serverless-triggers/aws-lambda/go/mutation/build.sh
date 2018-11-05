#!/bin/bash
set -e

echo "Fetching dependencies"

go get .

echo "Building binary"

env GOOS=linux GOARCH=amd64 go build

echo "Binary build complete. Zipping output."

zip -j ./mutation.zip mutation

echo "Zip complete. You can now upload the zip file"
