#! /usr/bin/env bash

set -euo pipefail

git diff --name-only master | sed 's%^server/%%' | egrep 'hs$' | xargs hlint
