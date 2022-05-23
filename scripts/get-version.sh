#!/usr/bin/env bash

# Outputs a version name for the hasura servers and their components, given the
# checked out state of the repo:
#  - if HEAD has a tag, return that
#  - else return a combination of branch and SHA hash
#
# In both of above we strip any non alpha-numeric, excluding: ._-
#
# This is called in a compile time macro to bake the version into the server
# binaries, and also at various points during CI (and must agree every time).

# When 'ci/server-test-mode' label is present in a PR, the CI runs in a
# so-called "server test mode". In server test mode, a server from some other 
# CI build is used to run all the server test jobs. This causes some tests, 
# which assert the version from /v1/version to be equal to the one 
# returned by this script, to fail. SERVER_TEST_MODE env var is used to 
# force the version to be set to the value in /build/_server_output/version.txt.
# This is done only for test_oss_server_pg_* CI jobs in server test mode.
if [[ "$SERVER_TEST_MODE" == "true" ]]; then
  VERSION="$(cat /build/_server_output/version.txt)"
  echo "$VERSION"
  exit 0
fi

GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
GIT_SHA="$(git rev-parse --short HEAD)"
# NOTE: a commit may have multiple tags; this should return the most recent
# result of `git tag` if any:
GIT_TAG_EXACT="$(git describe --tags --exact-match --dirty 2>/dev/null)"
GIT_DIRTY=$(test -n "`git status --porcelain`" && echo "-dirty" || echo "")

VERSION="${GIT_TAG_EXACT}"
# IMPORTANT: SHA hash needs to come first so we get a unique version string,
# even in the presence of truncation below
test -n "$VERSION" || VERSION="dev-${GIT_SHA}${GIT_DIRTY}-${GIT_BRANCH}"

VERSION="$(echo $VERSION | tr -cd '[[:alnum:]]._-')"
# Truncate to 50 chars. See: https://github.com/hasura/graphql-engine-mono/pull/4183
echo "${VERSION:0:50}"
