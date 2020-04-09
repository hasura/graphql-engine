#!/usr/bin/env bash

# Exit with error if diff with origin/master only contains files mentioned in
# .ciignore so that the build can be stopped.
#
# Adapted from:
# https://circleci.com/blog/circleci-hacks-automate-the-decision-to-skip-builds-using-a-git-hook/
# https://github.com/dollarshaveclub/harmless-changes/blob/master/index.sh

set -eo pipefail
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

# make build directory
mkdir -p /build/ciignore

# always build tagged builds
if [[ ! -z "$CIRCLE_TAG" ]]; then
    echo "Skipping check for tags"
    exit
fi

# always build release branch
if [[ "$CIRCLE_BRANCH" = "release-"* ]]; then
    echo "Skipping check for release branch"
    exit
fi

# get the diff
if [[ ! -z "$CIRCLE_COMPARE_URL" ]]; then
    # CIRCLE_COMPARE_URL is not empty, use it to get the diff
    if [[ $CIRCLE_COMPARE_URL = *"commit"* ]]; then
        COMMIT_RANGE=$(echo $CIRCLE_COMPARE_URL | sed 's:^.*/commit/::g')~1
    else
        COMMIT_RANGE=$(echo $CIRCLE_COMPARE_URL | sed 's:^.*/compare/::g')
    fi
    echo "Diff: $COMMIT_RANGE"
    changes="$(git diff $COMMIT_RANGE --name-only -- . ':!scripts' ':!assets' ':!docs' ':!community' ':!install-manifests' ':!github' ':!*.md' ':!.ciignore' ':!.gitignore' ':!LICENSE' ':!.github')"
elif [[ "$CIRCLE_BRANCH" == "master" ]]; then
    # CIRCLE_COMPARE_URL is not set, but branch is master, diff with last commit
    echo "Diff: HEAD~1"
    changes="$(git diff HEAD~1 --name-only -- . ':!scripts' ':!assets' ':!docs' ':!community' ':!install-manifests' ':!github' ':!*.md' ':!.ciignore' ':!.gitignore' ':!LICENSE' ':!.github')"
else
    # CIRCLE_COMPARE_URL is not set, branch is not master, diff with origin/master
    echo "Diff: origin/master..HEAD"
    changes="$(git diff-tree --no-commit-id --name-only -r origin/master..HEAD -- . ':!scripts' ':!assets' ':!docs' ':!community' ':!install-manifests' ':!github' ':!*.md' ':!.ciignore' ':!.gitignore' ':!LICENSE' ':!.github')"
fi

echo "Changes in this build:"
echo $changes
echo

if [[ ! -z "$changes" ]]; then
	# If there's still changes left, then we have stuff to build, leave the commit alone.
    echo "Files that are not ignored present in commits, need to build, succeed the job"
	exit
fi

echo "Only ignored files are present in commits, build is not required, write the skip_job file"
echo "true" > /build/ciignore/skip_job.txt
exit
