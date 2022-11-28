#!/usr/bin/env bash

set -e

usage="$0 postgresql://... "

if [ "$#" != 1 ];
then
echo "$usage"
exit 1
fi

pg_uri="$1"
shift

echo "Checking if 'docker compose' is free to use .."
running_containers=$(docker compose ls | wc -l)
if [ "$running_containers" -gt 1 ];
then
echo "docker appears to be running."
echo "Please finish what you're using docker for and run 'docker compose down' manually"
echo "before proceeding."
exit 1
fi

echo "Building test runner and report generator .."

cabal build api-tests

api_tests=$(cabal list-bin api-tests:exe:api-tests)
reporter=$(cabal list-bin api-tests:exe:render-feature-matrix)

rm tests-hspec.log || true

echo "Starting metadata db container .."
docker compose down -v && docker compose up postgres --wait

echo "Running integration test suite against '$pg_uri' .."
POSTGRES_VARIANT_URI="$pg_uri" $api_tests || true

echo "Tearing down metadata db container .."
docker compose down

echo "Generating feature matrix report: 'report.html' .."
$reporter < tests-hspec.log > report.html


