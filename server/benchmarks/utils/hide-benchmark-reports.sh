#!/usr/bin/env bash
set -euo pipefail

# This script looks at the comments on a hasura/graphql-engine-mono issue (or
# pull request) and rewrites all of the benchmark reports to hide them behind
# `<details>` tags. Our goal is to make benchmark reports less noisy on our
# PRs, especially when there are many reports, while retaining the visual
# prominence of the most recent report. To that end, this script is intended to
# be run just before posting a new benchmark report to a PR.
#
# This script depends on the `GITHUB_TOKEN` and `PR_NUMBER` environment
# variables. It identifies a comment as a benchmark report by checking whether
# the comment starts with "## Benchmark Results".


# Get issue comments.
# Note: We currently only fetch the first 100 comments. This may not be
# sufficient in the future.
# https://docs.github.com/en/rest/reference/issues#list-issue-comments
curl \
  -s \
  -H "Authorization: token ${GITHUB_TOKEN}" \
  -X GET "https://api.github.com/repos/hasura/graphql-engine-mono/issues/${PR_NUMBER}/comments?per_page=100" | \
# Keep only the benchmark reports.
jq -r -c '.[] | {id: .id, body: .body} | select(.body | startswith("## Benchmark Results"))' | \
# Update the benchmark reports to hide them.
while read -r report; do
  id=$(echo "${report}" | jq -r '.id')
  echo "Updating existing benchmark report (comment id=${id})"

  echo "${report}" | \
    jq -r '{body: .body} | .body |= "<details>\n<summary>Outdated benchmark report (click to expand)</summary>\n\n" + . + "\n</details>"' | \
  # https://docs.github.com/en/rest/reference/issues#update-an-issue-comment
  curl \
    -s \
    -X PATCH \
    -H "Accept: application/vnd.github.v3+json" \
    -H "Authorization: token ${GITHUB_TOKEN}" \
    "https://api.github.com/repos/hasura/graphql-engine-mono/issues/comments/${id}" \
    -d @-
done
