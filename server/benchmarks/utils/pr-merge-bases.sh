#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

# The goal of this script is to help answer the question "which set of
# benchmark results should we use as the baseline performance for this branch".
# It returns a list of PR numbers, in reverse history order (in priority order)
# since the first commit might not have any associated benchmark results (e.g.
# because the benchmarks failed to run).
#
# e.g. `pr-merge-base.sh main 1` will return the PR number corresponding to the
# benchmark set we should use to measure the relative performance of the
# changes we've made in this branch.

# Usage:
#   ./pr-merge-base.sh <base_branch> <lim>
BASE_BRANCH=$1
LIM=$2

# For now we ignore BASE_BRANCH and just get the list of commits with possible
# associated benchmark results from the history of this branch. These may be
# inter-mixed with changes from this branch (if the PR owner did "update
# branch"), but that doesn't matter.
#
# TODO we'd like to filter only commits from this list that are also in
# BASE_BRANCH, and also generally figure out if this can work well for PRs
# where target is not `main` (at the moment kodiak won't manage merges into
# non-protected branches anyway).
git log \
  --grep='https://github.com/hasura/graphql-engine-mono/pull/[0-9]*$' \
  -"$LIM" \
  | grep  -oP '^    PR-URL: https://github.com/hasura/graphql-engine-mono/pull/\K[0-9]*$'
  # ^ TODO we'd also like to make this a multi-line regex so we can be sure
  # these are just the kodiak-added lines
