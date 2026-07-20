#!/usr/bin/env bash
set -euo pipefail

# Drop-in replacement for `git cherry-pick <commit>` that also handles merge
# commits sanely for our release-branch process.
#
# Background: when a PR is merged into main via the "merge commit" method, the
# resulting merge commit's message carries the "PR-URL: ..." trailer that our
# release automation depends on. But by the time you're replaying main's
# history onto a release branch commit-by-commit, that PR's real commits have
# already been cherry-picked individually (they precede the merge commit in
# history) -- so `git cherry-pick -m 1 <merge-commit>` always produces a
# zero-diff commit. GitHub then silently strips empty commits when the
# release PR itself gets merged, so the PR-URL trailer -- and whatever
# downstream automation depends on it -- is lost.
#
# The fix: for merge commits, don't try to replay a diff at all. Instead,
# stage a tiny marker file (so the commit is non-empty and survives GitHub's
# merge) and reuse the merge commit's exact message/author via `git commit -C`.
#
# For ordinary (non-merge) commits, this just forwards straight to
# `git cherry-pick`.
#
# Usage:
#   cherry-pick.sh <commit-ish>
#   cherry-pick.sh --continue|--abort|--quit   # forwarded to `git cherry-pick`

echo_pretty() { echo ">>> $(tput setaf 2 2>/dev/null)${1:-}$(tput sgr0 2>/dev/null)"; }
echo_error() {  echo ">>> $(tput setaf 1 2>/dev/null)${1:-}$(tput sgr0 2>/dev/null)"; }
echo_warn() {   echo ">>> $(tput setaf 3 2>/dev/null)${1:-}$(tput sgr0 2>/dev/null)"; }

if [ $# -eq 0 ]; then
    echo_error "Usage: $0 <commit-ish>"
    echo_error "       $0 --continue|--abort|--quit"
    exit 1
fi

# Pass through cherry-pick's own control flags untouched.
case "$1" in
    --continue|--abort|--quit)
        exec git cherry-pick "$@"
        ;;
esac

if [ $# -ne 1 ]; then
    echo_error "Usage: $0 <commit-ish>"
    exit 1
fi

COMMIT_ISH="$1"

if ! REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)"; then
    echo_error "Not inside a git repository."
    exit 1
fi
MARKER_DIR="$REPO_ROOT/cherry-pick-markers"

SHA="$(git rev-parse "$COMMIT_ISH")"
PARENT_COUNT="$(git cat-file -p "$SHA" | grep -c '^parent ' || true)"

if [ "$PARENT_COUNT" -le 1 ]; then
    exec git cherry-pick "$SHA"
fi

echo_pretty "$SHA is a merge commit; replaying its message via a marker commit instead of its (already-applied) diff."

if ! git diff --quiet || ! git diff --cached --quiet; then
    echo_error "Working tree or index is not clean. Commit, stash, or clean up before running this."
    exit 1
fi

MARKER_FILE="$MARKER_DIR/$SHA"

if [ -e "$MARKER_FILE" ]; then
    echo_error "Marker $MARKER_FILE already exists -- this merge commit looks like it's already been cherry-picked onto this branch."
    exit 1
fi

mkdir -p "$MARKER_DIR"
touch "$MARKER_FILE"
git add "$MARKER_FILE"
git commit -C "$SHA"

echo_pretty "Committed marker for merge commit $SHA (message and authorship reused)."
