#!/usr/bin/env bash

# This runs `hpack`, but generates a custom header that instructs the reader not to edit the file.
# It instead tells them to edit package.yaml and run a specific `make` command.

set -e
set -u
set -o pipefail

root_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]:-$0}")/.." &> /dev/null && pwd 2> /dev/null)"

function usage {
  echo >&2 'Usage:'
  echo >&2 "  ${BASH_SOURCE[0]} [--check] TARGET"
  echo >&2
  # shellcheck disable=SC2016
  echo >&2 '--check: Do not write the file; instead verify that it is up-to-date.'
  echo >&2 'TARGET: The Cabal file to be generated from package.yaml in the same directory.'
  exit 2
}

# Prefix the target file with a custom header.
function modify {
  # Write the Cabal version information first; it must be before any comments.
  awk "{ print }; NR == 2 { print \"-- This file is generated using hpack.\n-- Do not modify it directly. Instead, edit package.yaml and then run:\n--     make ${target}\n\" }"
}

if [[ "$1" == '--check' ]]; then
  check=true
  shift
else
  check=false
fi

if [[ $# -ne 1 ]]; then
  usage
fi

# The target must be relative to the root of the project for the `make` command to be meaningful.
target="$(realpath --relative-to="$root_dir" "$1")"
source="$(dirname "$target")"

if $check; then
  output="$(mktemp -t "$(basename "$target").XXXXX")"
  trap 'rm -f "$output"' EXIT
  hpack "$source" - | modify > "$output"
  # `git diff` will return a non-zero exit status if the files are different.
  git diff --no-index "$target" "$output"
else
  hpack "$source" - | modify > "$target"
fi
