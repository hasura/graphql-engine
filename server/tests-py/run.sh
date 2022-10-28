#!/usr/bin/env bash

# This allows a developer, through Docker, to run the Python integration tests in pretty
# much exactly the same way as CI does (in contrast to `dev.sh test --integration`),
# allowing us to more readily diagnose issues locally.
#
# This takes an optional test configuration argument, corresponding to a name in
# `oss-.circleci/server-test-names.txt` (else defaulting to `no-auth`).
#
# See `case "$SERVER_TEST_TO_RUN"` in `oss-.circleci/test-server.sh` for what
# these actually do.

set -e
set -u
set -o pipefail

cd -- "$(dirname -- "${BASH_SOURCE[0]}")"

# This `PLATFORM` value is used to pick the correct server builder image and HGE binary path.
PLATFORM="$(uname -m)"
if [[ "$PLATFORM" == 'x86_64' || "$PLATFORM" == 'amd64' ]]; then
  CABAL_PLATFORM='x86_64'
  DOCKER_PLATFORM='amd64'
fi
if [[ "$PLATFORM" == 'aarch64' || "$PLATFORM" == 'arm64' ]]; then
  CABAL_PLATFORM='aarch64'
  DOCKER_PLATFORM='arm64'
fi
export CABAL_PLATFORM DOCKER_PLATFORM

# copied from images.go
HASURA_GRAPHQL_ENGINE_SERVER_BUILDER_SHA="$(
  sha256sum ../../.buildkite/dockerfiles/ci-builders/server-builder.dockerfile \
  | awk '{ print $1 }'
)"
export HASURA_GRAPHQL_ENGINE_SERVER_BUILDER_SHA

# copied from images.go
HASURA_GRAPHQL_ENGINE_SERVER_PYTEST_RUNNER_SHA="$(
  cat \
		../../.buildkite/dockerfiles/server-pytest-runner/Dockerfile \
		./requirements.txt \
		./package-lock.json \
		./package.json \
		./remote_schemas/nodejs/package.json \
  | sha256sum \
  | awk '{ print $1 }'
)"
export HASURA_GRAPHQL_ENGINE_SERVER_PYTEST_RUNNER_SHA

# Use the Azure SQL Edge image instead of the SQL Server image on arm64.
# The latter doesn't work yet.
if [[ "$(uname -m)" == 'arm64' ]]; then
  export MSSQL_IMAGE='mcr.microsoft.com/azure-sql-edge'
fi

if [[ $# -gt 0 ]]; then
  SERVER_TESTS_TO_RUN=("$@")
else
  SERVER_TESTS_TO_RUN=('no-auth')
fi

echo '*** Pulling images ***'
docker compose pull --ignore-pull-failures

echo
echo '*** Building images ***'
# We rebuild the images because on arm64, we end up with an amd64 Python test
# runner. This won't actually be able to run HGE. Until we build an arm64 image
# on CI, we need to instead build it locally. On amd64, this is benign; the
# `docker compose run tests-py` step would have built it anyway.
docker compose build

echo
echo '*** Building HGE ***'
docker compose run --rm hge-build

for SERVER_TEST_TO_RUN in "${SERVER_TESTS_TO_RUN[@]}"; do
  export SERVER_TEST_TO_RUN
  echo
  echo "*** Running test suite: ${SERVER_TEST_TO_RUN} ***"
  docker compose rm -svf citus mssql mssql-healthcheck postgres # tear down databases beforehand
  docker compose run --rm tests-py
done
