#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

# This prints one or more PR numbers, in reverse history order, from the merge
# base of the branch we're on.
# e.g. `pr-merge-base.sh main 1` will return the PR number corresponding to the
# benchmark set we should use to measure the relative performance of the
# changes we've made in this branch.

# Usage:
#   ./pr-merge-base.sh <base_branch> <lim>
BASE_BRANCH=$1
LIM=$2

# This is meant to be the first commit _after_ `git merge-base` since we need
# the range below to include the merge-base commit...
OUR_FIRST_COMMIT=$(git rev-list ^"$BASE_BRANCH" HEAD | tail -n 1)

git log \
  --grep='https://github.com/hasura/graphql-engine-mono/pull/[0-9]*$' \
  "$OUR_FIRST_COMMIT"^@  \
  -"$LIM" \
  | grep  -oP 'https://github.com/hasura/graphql-engine-mono/pull/\K[0-9]*$'
