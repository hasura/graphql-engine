#!/usr/bin/env bash
#
# PreToolUse hook: ensure `gh pr create` uses .github/PULL_REQUEST_TEMPLATE.md.
#
# Release automation parses sentinel HTML comments inside the template
# (component / product / type / changelog-entry / kodiak). If a PR is opened
# without them, the changelog & release notes pipeline breaks. This hook
# inspects the proposed `gh pr create` command and blocks it when those
# sentinels are absent, feeding the model a remediation message.
#
# Exits:
#   0 - allow the tool call (not a `gh pr create`, or body looks fine)
#   2 - block the tool call; stderr is shown to the model as feedback
#
set -euo pipefail

input="$(cat)"
command="$(printf '%s' "$input" | jq -r '.tool_input.command // ""')"

# Only inspect `gh pr create` invocations.
if ! printf '%s' "$command" | grep -qE '(^|[[:space:]])gh[[:space:]]+pr[[:space:]]+create([[:space:]]|$)'; then
  exit 0
fi

# Resolve the body. `--body-file PATH` points at a file on disk; for everything
# else (`--body '...'`, heredoc-expanded `--body "$(cat <<EOF ... EOF)"`) the
# literal text is already inlined into the command string, so scanning the
# command itself suffices.
body=""
if printf '%s' "$command" | grep -qE -- '--body-file[[:space:]]+'; then
  body_file_path="$(printf '%s' "$command" | sed -nE 's/.*--body-file[[:space:]]+([^[:space:]]+).*/\1/p')"
  if [ -n "$body_file_path" ] && [ -f "$body_file_path" ]; then
    body="$(cat "$body_file_path")"
  fi
fi
if [ -z "$body" ]; then
  body="$command"
fi

required_markers=(
  "component : end : DO NOT REMOVE"
  "product : end : DO NOT REMOVE"
  "type : end : DO NOT REMOVE"
  "changelog-entry : end : DO NOT REMOVE"
  "kodiak-commit-message-body-start"
)

missing=()
for marker in "${required_markers[@]}"; do
  if ! printf '%s' "$body" | grep -qF -- "$marker"; then
    missing+=("$marker")
  fi
done

if [ ${#missing[@]} -eq 0 ]; then
  exit 0
fi

{
  echo "PR body is missing markers from .github/PULL_REQUEST_TEMPLATE.md."
  echo "Read that file and use it as the basis for the PR body — release"
  echo "automation parses these sentinel HTML comments, so the structure"
  echo "must stay intact."
  echo
  echo "Missing sentinel(s):"
  for marker in "${missing[@]}"; do
    echo "  - <!-- ${marker} -->"
  done
  echo
  echo "Re-run \`gh pr create\` with a body that keeps the template's"
  echo "Description / Changelog (Component, Product, Type, Changelog entry) /"
  echo "Kodiak sections — filling in the relevant checkboxes."
} >&2
exit 2
