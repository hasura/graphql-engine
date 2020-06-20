#!/usr/bin/env sh
set -e

get_changes_hash()
{
    # We use this to determine if there are any new additions
    local GIT_STATUS="$(git status --porcelain)"
    # To determine if there are any changes
    local GIT_DIFF_INDEX="$(git diff-index -p HEAD --)"
    # Whether anything changed in the repo
    export GIT_DIRTY="$GIT_STATUS$GIT_DIFF_INDEX"
    if [ -n "$GIT_DIRTY" ]; then
        DIRTY_HASH_SHORT="$(echo $GIT_DIRTY | sha256sum | awk '{print $1}' | tail -c 9)"
        echo -dirty-$DIRTY_HASH_SHORT
    else
        echo ''
    fi
}

get_main_version()
{
    # Get the branch name
    local GIT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
    # Get the current commit id
    local COMMIT_HASH_SHORT="$(git rev-parse --short HEAD)"

    case "$GIT_BRANCH" in

        # The master branch
        'master')
            echo $COMMIT_HASH_SHORT;;

        # The release branches
        release-*)
            local RELEASE_VER="$(git describe --match "v[0-9]*" HEAD 2>/dev/null)"
            test -n "$RELEASE_VER" ||
                RELEASE_VER="$(expr "$GIT_BRANCH" : release-*'\(.*\)')"-$COMMIT_HASH_SHORT
            echo $RELEASE_VER;;

        # Everything else
        *)
            echo $GIT_BRANCH-$COMMIT_HASH_SHORT;;
    esac
}

echo "$(get_main_version)$(get_changes_hash)"
