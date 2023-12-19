#!/bin/bash

set -e

export TEST_FAILURE=0
# NOTE: We only run unit tests via --lib and --bins flags
RUSTFLAGS="-Cinstrument-coverage" LLVM_PROFILE_FILE="cargo-test-%p-%m.profraw" cargo test --lib --bins || TEST_FAILURE=1
if [ "$TEST_FAILURE" -eq 1 ]; then 
  echo "WARNING: test failures found..." 
fi
echo -e "\ngenerating coverage report...\n"
mkdir -p coverage
grcov . --binary-path ./target/debug/deps/ -s . -t markdown -p "${CARGO_HOME:=.cargo}" --branch --ignore-not-existing --ignore "../*" --ignore "/*" -o coverage
# Strip header and footer and then sort
(tail -n +3 coverage/markdown.md | head -n -2 | sort) > /tmp/sorted.md
# Filter filepaths based on input argument
if [ -n "$1" ]; then
  (grep -E "$1" /tmp/sorted.md > /tmp/sorted_filtered.md) || (echo "No relevant files found for coverage in current changelist" > /tmp/sorted_filtered.md)
else
  cat /tmp/sorted.md > /tmp/sorted_filtered.md
fi
# Add header and footer back
(head -n 2 coverage/markdown.md && cat /tmp/sorted_filtered.md && tail -n 2 coverage/markdown.md ) > coverage/markdown_sorted.md
cat coverage/markdown_sorted.md
rm -rf **/*.profraw
