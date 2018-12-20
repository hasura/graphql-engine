#!/usr/bin/env bash

# Exit with error if diff with origin/master only contains files mentioned in
# .ciignore so that the build can be stopped.
#
# Adapted from:
# https://circleci.com/blog/circleci-hacks-automate-the-decision-to-skip-builds-using-a-git-hook/
# https://github.com/dollarshaveclub/harmless-changes/blob/master/index.sh

set -eo pipefail
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

# succeed until the script is fixed: https://github.com/hasura/graphql-engine/issues/1161
exit

# always build tagged builds
if [[ ! -z "$CIRCLE_TAG" ]]; then
    echo "Skipping check for tags"
    exit
fi

# always build default branch
if [[ "$CIRCLE_BRANCH" == "master" ]]; then
    echo "Skipping check for master branch"
    exit
fi

if [[ ! -a "$ROOT/.ciignore" ]]; then
    echo "Skipping check since .ciignore is not found"
	  exit # If .ciignore doesn't exists, just quit this script
fi

# Check CIRCLE_COMPARE_URL first and if its not set, check for diff with master.
    
if [[ ! -z "$CIRCLE_COMPARE_URL" ]]; then
    # CIRCLE_COMPARE_URL is not empty, use it to get the diff
    if [[ $CIRCLE_COMPARE_URL = *"commit"* ]]; then
        COMMIT_RANGE=$(echo $CIRCLE_COMPARE_URL | sed 's:^.*/commit/::g')~1
    else
        COMMIT_RANGE=$(echo $CIRCLE_COMPARE_URL | sed 's:^.*/compare/::g')
    fi
    echo "Diff: $COMMIT_RANGE"
    changes="$(git diff $COMMIT_RANGE --name-only)"
else
    # CIRCLE_COMPARE_URL is not set, diff with origin/master
    echo "Diff: origin/master..HEAD"
    changes="$(git diff-tree --no-commit-id --name-only -r origin/master..HEAD)"
fi

echo "Changes in this build:"
echo $changes
echo

# Load the patterns we want to skip into an array
mapfile -t blacklist < "$ROOT/.ciignore"

for i in "${blacklist[@]}"
do
	  # Remove the current pattern from the list of changes
	  changes=( ${changes[@]/$i/} )

	  if [[ ${#changes[@]} -eq 0 ]]; then
		    # If we've exhausted the list of changes before we've finished going
		    # through patterns, that's okay, just quit the loop
		    break
	  fi
done

if [[ ${#changes[@]} -gt 0 ]]; then
	  # If there's still changes left, then we have stuff to build, leave the commit alone.
    echo "Files that are not ignored present in commits, need to build, succeed the job"
	  exit
fi

echo "Only ignored files are present in commits, no need to build, fail the job"
exit 1
