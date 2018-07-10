#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

echo "$ROOT"