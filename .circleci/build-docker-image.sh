#!/usr/bin/env bash

set -e

DF_VERSION=$1

for FILE in *.dockerfile; do
    f=$(basename -- "$FILE")
    f="${f%.*}"
    TAG=hasura/graphql-engine-$f:$DF_VERSION
    echo
    echo "=============================================================="
    echo "Building and pushing $TAG"
    echo "=============================================================="
    echo
    echo "=======>>>> docker build -t $TAG -f $FILE ."
    echo
    docker build -t $TAG -f $FILE .
    echo "=======>>>> docker push $TAG"
    echo
    docker push $TAG
done