#!/usr/bin/env bash
#
# Regression test for the inbound rsync in .github/workflows/migrate-hge-pr.yml.
#
# Background
# ----------
# `docs/k8s-manifest/` is mono-only docs deployment infrastructure that does not
# exist in OSS graphql-engine. The outbound Copybara shadow-push workflows already
# keep it mono-only via the `docs/k8s-manifest/**` origin_files exclusions in
# copy.bara.sky. The *inbound* OSS->mono PR migration instead uses rsync:
#
#     rsync -av --delete ./* ../graphql-engine-mono --exclude .git --exclude v3 --exclude scripts/release
#
# Because `docs/` exists in OSS but has no `k8s-manifest/` subtree, the recursive
# `--delete` removed the mono-only directory (see graphql-engine-mono#11771, migrated
# from graphql-engine#10885). The fix adds `--exclude '/docs/k8s-manifest/'` so rsync
# neither deletes nor overwrites those files on inbound import.
#
# This test rebuilds disposable source (OSS) and destination (mono) trees and runs
# the REAL migration rsync command/options to prove:
#   1. The original command reproduces loss of a mono-only manifest.
#   2. The fixed command preserves all five mono-only files byte-for-byte.
#   3. Changed OSS docs/server files still sync, and stale ordinary files are deleted.
#   4. Source-side files placed under the excluded directory cannot overwrite the
#      mono-only deployment files.
#
# The test is fail-closed: any failed assertion or unexpected rsync exit code aborts
# with a non-zero status.

set -Eeuo pipefail

# The fix under test: the extra exclude added to the inbound migration rsync.
# Pass empty string to run the ORIGINAL (pre-fix) command.
FIX_OPT="--exclude=/docs/k8s-manifest/"

# The five mono-only files that must survive inbound migration.
K8S_FILES=(
  "docs/k8s-manifest/README.md"
  "docs/k8s-manifest/cloudbuild.yaml"
  "docs/k8s-manifest/k8s/deployment.yaml"
  "docs/k8s-manifest/k8s/hpa.yaml"
  "docs/k8s-manifest/k8s/service.yaml"
)

PASS=0
FAIL=0

pass() { echo "ok   - $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL - $1"; FAIL=$((FAIL + 1)); }

assert_exists()  { if [[ -e "$1" ]]; then pass "$2"; else fail "$2 (missing: $1)"; fi; }
assert_missing() { if [[ ! -e "$1" ]]; then pass "$2"; else fail "$2 (unexpectedly present: $1)"; fi; }
assert_content() {
  # $1=file $2=expected-content $3=message
  if [[ -f "$1" && "$(cat "$1")" == "$2" ]]; then pass "$3"; else fail "$3 (content mismatch: $1)"; fi
}

WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/migrate-hge-rsync-test.XXXXXX")"
cleanup() { rm -rf "$WORKDIR"; }
trap cleanup EXIT

echo "rsync: $(rsync --version | head -1)"
echo "workdir: $WORKDIR"
echo

# Build a fresh (OSS source, mono destination) pair of trees.
# OSS source mirrors graphql-engine: has docs/ (but NO k8s-manifest), server code,
# an excluded scripts/release, and a .git dir. The mono destination additionally
# holds the mono-only docs/k8s-manifest tree plus a stale file to exercise --delete.
build_trees() {
  local root="$1"
  local src="$root/graphql-engine"
  local dst="$root/graphql-engine-mono"
  rm -rf "$root"
  mkdir -p "$src" "$dst"

  # --- OSS source tree (what the migration reads from) ---
  mkdir -p "$src/docs" "$src/server/lib" "$src/scripts/release" "$src/.git"
  echo "oss-docs-index-UPDATED" > "$src/docs/index.md"
  echo "oss-server-UPDATED"      > "$src/server/lib/Main.hs"
  echo "should-not-sync"         > "$src/scripts/release/release.sh"
  echo "should-not-sync"         > "$src/.git/config"

  # --- mono destination tree (what must be protected) ---
  mkdir -p "$dst/docs/k8s-manifest/k8s" "$dst/server/lib"
  echo "mono-k8s-readme"       > "$dst/docs/k8s-manifest/README.md"
  echo "mono-k8s-cloudbuild"   > "$dst/docs/k8s-manifest/cloudbuild.yaml"
  echo "mono-k8s-deployment"   > "$dst/docs/k8s-manifest/k8s/deployment.yaml"
  echo "mono-k8s-hpa"          > "$dst/docs/k8s-manifest/k8s/hpa.yaml"
  echo "mono-k8s-service"      > "$dst/docs/k8s-manifest/k8s/service.yaml"
  # Pre-existing docs/server files with stale content (should be overwritten by sync).
  echo "stale-docs-index"      > "$dst/docs/index.md"
  echo "stale-server"          > "$dst/server/lib/Main.hs"
  # A stale ordinary file inside a synced directory (should be deleted by --delete).
  echo "stale-ordinary"        > "$dst/server/lib/Deleted.hs"
}

# Snapshot checksums of the mono-only files in a destination tree.
k8s_checksums() {
  local dst="$1" f
  for f in "${K8S_FILES[@]}"; do
    if [[ -f "$dst/$f" ]]; then
      echo "$f  $(cksum < "$dst/$f")"
    else
      echo "$f  MISSING"
    fi
  done
}

run_rsync() {
  # Runs the REAL inbound migration rsync from inside the OSS source dir, exactly
  # like migrate-hge-pr.yml does (`./*` must glob-expand inside the source dir, and
  # `../graphql-engine-mono` is the sibling destination). The command below is a
  # verbatim copy of the workflow line; keep it in sync with that workflow.
  #   $1 = source dir
  #   $2 = optional extra arg (the fix's --exclude, or "" for the original command)
  local src="$1"
  local extra="${2-}"
  local rc=0
  # rsync's own (verbose) output goes to stderr so that this function's stdout
  # carries only the captured exit code for the caller's `$(...)`.
  if [[ -n "$extra" ]]; then
    ( cd "$src" && rsync -av --delete ./* ../graphql-engine-mono --exclude .git --exclude v3 --exclude scripts/release "$extra" ) >&2 || rc=$?
  else
    ( cd "$src" && rsync -av --delete ./* ../graphql-engine-mono --exclude .git --exclude v3 --exclude scripts/release ) >&2 || rc=$?
  fi
  echo "$rc"
}

#############################################
# Test 1: ORIGINAL command reproduces the loss
#############################################
echo "== Test 1: original command (no fix) wipes mono-only docs/k8s-manifest =="
T1="$WORKDIR/t1"
build_trees "$T1"
rc="$(run_rsync "$T1/graphql-engine")"
if [[ "$rc" == "0" ]]; then pass "original rsync exited 0"; else fail "original rsync exit code $rc"; fi
# The regression: at least these representative files must be gone.
assert_missing "$T1/graphql-engine-mono/docs/k8s-manifest/k8s/deployment.yaml" \
  "original command deletes mono-only deployment.yaml (reproduces bug)"
assert_missing "$T1/graphql-engine-mono/docs/k8s-manifest/README.md" \
  "original command deletes mono-only README.md (reproduces bug)"
# Sanity: it still copied the OSS docs file in.
assert_content "$T1/graphql-engine-mono/docs/index.md" "oss-docs-index-UPDATED" \
  "original command still syncs OSS docs/index.md"
echo

#############################################
# Test 2 + 3: FIXED command preserves + still syncs/deletes
#############################################
echo "== Test 2/3: fixed command preserves manifests, still syncs & deletes =="
T2="$WORKDIR/t2"
build_trees "$T2"
BEFORE="$(k8s_checksums "$T2/graphql-engine-mono")"
rc="$(run_rsync "$T2/graphql-engine" "$FIX_OPT")"
if [[ "$rc" == "0" ]]; then pass "fixed rsync exited 0"; else fail "fixed rsync exit code $rc"; fi

# (2) all five mono-only files preserved byte-for-byte
missing5=0
for f in "${K8S_FILES[@]}"; do
  if [[ ! -f "$T2/graphql-engine-mono/$f" ]]; then missing5=1; fi
done
if [[ "$missing5" == "0" ]]; then pass "all five mono-only files still present"; else fail "a mono-only file is missing after fixed sync"; fi
AFTER="$(k8s_checksums "$T2/graphql-engine-mono")"
if [[ "$BEFORE" == "$AFTER" ]]; then
  pass "all five mono-only files preserved byte-for-byte (cksum match)"
else
  fail "mono-only file checksums changed after fixed sync"
  diff <(printf '%s\n' "$BEFORE") <(printf '%s\n' "$AFTER") || true
fi

# (3) changed OSS files still sync; stale ordinary file still deleted
assert_content "$T2/graphql-engine-mono/docs/index.md" "oss-docs-index-UPDATED" \
  "changed OSS docs/index.md synced into mono"
assert_content "$T2/graphql-engine-mono/server/lib/Main.hs" "oss-server-UPDATED" \
  "changed OSS server/lib/Main.hs synced into mono"
assert_missing "$T2/graphql-engine-mono/server/lib/Deleted.hs" \
  "stale ordinary file still deleted by --delete"
# Excluded paths must not leak into mono.
assert_missing "$T2/graphql-engine-mono/scripts/release/release.sh" \
  "scripts/release still excluded"
assert_missing "$T2/graphql-engine-mono/.git/config" \
  ".git still excluded"
echo

#############################################
# Test 4: source-side files under the excluded dir cannot overwrite mono files
#############################################
echo "== Test 4: OSS source cannot overwrite mono-only deployment files =="
T4="$WORKDIR/t4"
build_trees "$T4"
# Simulate OSS carrying a colliding docs/k8s-manifest/ (e.g. a reimport attempt).
mkdir -p "$T4/graphql-engine/docs/k8s-manifest/k8s"
echo "OSS-OVERWRITE-ATTEMPT" > "$T4/graphql-engine/docs/k8s-manifest/README.md"
echo "OSS-OVERWRITE-ATTEMPT" > "$T4/graphql-engine/docs/k8s-manifest/k8s/deployment.yaml"
echo "OSS-NEW-FILE"          > "$T4/graphql-engine/docs/k8s-manifest/k8s/injected.yaml"
rc="$(run_rsync "$T4/graphql-engine" "$FIX_OPT")"
if [[ "$rc" == "0" ]]; then pass "fixed rsync (with colliding source) exited 0"; else fail "fixed rsync exit code $rc"; fi
assert_content "$T4/graphql-engine-mono/docs/k8s-manifest/README.md" "mono-k8s-readme" \
  "mono README.md not overwritten by OSS source"
assert_content "$T4/graphql-engine-mono/docs/k8s-manifest/k8s/deployment.yaml" "mono-k8s-deployment" \
  "mono deployment.yaml not overwritten by OSS source"
assert_missing "$T4/graphql-engine-mono/docs/k8s-manifest/k8s/injected.yaml" \
  "OSS-injected file not written into excluded dir"
echo

#############################################
echo "========================================"
echo "PASS: $PASS   FAIL: $FAIL"
if [[ "$FAIL" -ne 0 ]]; then
  echo "REGRESSION TEST FAILED"
  exit 1
fi
echo "ALL ASSERTIONS PASSED"
