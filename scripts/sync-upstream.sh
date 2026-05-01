#!/usr/bin/env bash
# Sync the local master with hasura/graphql-engine upstream/master, then replay
# our local-only commits on top.
#
# Upstream and our fork are independent exports of the same private mono repo,
# so commit hashes never line up — but the file content is the same project.
# We rebase rather than merge to keep history linear.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

UPSTREAM_REMOTE="${UPSTREAM_REMOTE:-upstream}"
UPSTREAM_BRANCH="${UPSTREAM_BRANCH:-master}"
LOCAL_BRANCH="${LOCAL_BRANCH:-master}"

step() { printf '\n=== %s ===\n' "$*"; }

if ! git remote get-url "$UPSTREAM_REMOTE" >/dev/null 2>&1; then
  echo "error: remote '$UPSTREAM_REMOTE' is not configured" >&2
  exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
  echo "error: working tree is dirty — commit or stash first" >&2
  exit 1
fi

current_branch="$(git rev-parse --abbrev-ref HEAD)"
if [ "$current_branch" != "$LOCAL_BRANCH" ]; then
  echo "error: expected to be on '$LOCAL_BRANCH', currently on '$current_branch'" >&2
  exit 1
fi

step "Fetching $UPSTREAM_REMOTE"
git fetch "$UPSTREAM_REMOTE" "$UPSTREAM_BRANCH"

upstream_ref="$UPSTREAM_REMOTE/$UPSTREAM_BRANCH"
old_head="$(git rev-parse HEAD)"
upstream_head="$(git rev-parse "$upstream_ref")"

if [ "$old_head" = "$upstream_head" ]; then
  echo "already up to date with $upstream_ref"
  exit 0
fi

# Snapshot pre-sync state to a dated backup branch so we can recover if anything
# goes sideways. Force-update if it already exists today.
backup_branch="master-pre-sync-$(date +%Y%m%d-%H%M%S)"
step "Creating backup branch $backup_branch"
git branch -f "$backup_branch" "$old_head"

step "Rebasing $LOCAL_BRANCH onto $upstream_ref"
if ! git rebase "$upstream_ref"; then
  cat >&2 <<EOF

rebase failed — resolve conflicts, then either:
  - finish:  git rebase --continue (and re-run any remaining steps manually)
  - abort:   git rebase --abort && git reset --hard $backup_branch
EOF
  exit 1
fi

step "Done"
new_head="$(git rev-parse HEAD)"
echo "  was:    $old_head"
echo "  now:    $new_head ($(git rev-parse --short "$upstream_ref") + local commits)"
echo "  backup: $backup_branch"
echo
echo "review with:  git log --oneline $upstream_ref..HEAD"
echo "publish with: git push --force-with-lease origin $LOCAL_BRANCH"
