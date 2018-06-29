#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
CLI_ROOT="${BASH_SOURCE[0]%/*}/../cli"

cd "$CLI_ROOT"

make assets
make build
make compress
