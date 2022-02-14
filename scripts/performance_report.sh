#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar
set -o errtrace

function usage {
    echo "Show performance of the current branch over the last N commits. Note that only commits"
    echo "that touch server/ are benchmarked, so this may display fewer than N results."
    echo "This opens your default browser."
    echo
    echo "  Usage:"
    echo "      $ $0 <num_commits> <benchmark_set> [<extra_mono_pr_number>]"
    echo
    echo "e.g. to look at how performance of the 'chinook' benchmarks have changed over the last"
    echo "ten commits, alongside the PR #1337 that you have opened:"
    echo "    $ $0 10 chinook 1337"
}
trap usage ERR
[ -z ${1:+x} ] && false
[ -z ${2:+x} ] && false

LIM=$1
BENCH_SET=$2

# NOTE: These two utilities copied from the lux repo:
open_webpage_cmd() {
  if [[ "$(uname)" == "Darwin" ]]; then
    open $1
  elif [[ "$(expr substr $(uname -s) 1 5)" == "Linux" ]]; then
    xdg-open $1
  elif [[ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]]; then
    start $1
  elif [[ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]]; then
    start $1
  fi
}

open_webpage() {
  if ! open_webpage_cmd $1 >/dev/null; then
    echo "failed to open browser"
    echo "navigate to: $1"
  fi
}

# see also server/benchmarks/utils/pr-merge-bases.sh 
pr_nums=$(git log \
  --grep='https://github.com/hasura/graphql-engine-mono/pull/[0-9]*$' \
  -"$LIM" \
  | grep  -oP '^    PR-URL: https://github.com/hasura/graphql-engine-mono/pull/\K[0-9]*$' \
  # ^ TODO we'd also like to make this a multi-line regex so we can be sure
  # these are just the kodiak-added lines
)

# Add the optional additional PR number to the charts:
if [ ! -z ${3+x} ]; then
    pr_nums="$3 $pr_nums"
fi

echo -n "Collecting merged PRs with associated benchmarks: "  # progress printed below
fragment=""
for num in $pr_nums; do
    # See also `server/benchmarks/fabfile.py` and https://github.com/hasura/graphql-bench 
    RESULTS_S3_BUCKET="hasura-benchmark-results"
    json_url="https://$RESULTS_S3_BUCKET.s3.us-east-2.amazonaws.com/mono-pr-$num/$BENCH_SET.json"
    if curl --output /dev/null --silent --head --fail "$json_url"; then
      echo -n .
    else
      echo -n x
      continue
    fi
    fragment="$fragment,mono-pr-$num/$BENCH_SET"
done
echo

url="https://hasura.github.io/graphql-bench/app/web-app/#${fragment:1}"

open_webpage_cmd $url
