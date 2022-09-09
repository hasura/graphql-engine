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

if [[ $# -gt 0 ]]; then
  SERVER_TEST_TO_RUN="$1"
else
  SERVER_TEST_TO_RUN='no-auth'
fi
export SERVER_TEST_TO_RUN

# tear down databases beforehand
docker compose rm -svf postgres

docker compose up hge-build tests-py-on-postgres
