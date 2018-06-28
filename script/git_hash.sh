#!/usr/bin/env sh
set -e

get_hash()
{
    DEP_DIRS="$1"
    # are there any new additions
    GIT_STATUS="$(git status "$DEP_DIRS" --porcelain)"
    # are there any changes to existing files
    GIT_DIFF_INDEX="$(git diff-index -p HEAD -- "$DEP_DIRS")"
    # has anything changed in the repo
    export GIT_DIRTY="$GIT_STATUS$GIT_DIFF_INDEX"
    if [ -n "$GIT_DIRTY" ]; then
        DIRTY_HASH_SHORT="$(echo "$GIT_DIRTY" | sha256sum | awk '{print $1}' | tail -c 9)"
        CHANGES_HASH="-dirty-$DIRTY_HASH_SHORT"
    else
        CHANGES_HASH=""
    fi
    # Get the current commit id
    COMMIT_HASH_SHORT="$(git log -n 1 --pretty=format:%h -- "$DEP_DIRS")"
    echo "$COMMIT_HASH_SHORT$CHANGES_HASH"
}

get_hash "$1"
