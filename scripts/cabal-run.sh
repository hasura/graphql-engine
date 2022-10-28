#!/usr/bin/env bash

#   cabal-run.sh target args
#
# is equivalent to
#
#   cabal run target --args
#
# except that killing it will also kill the target.
#
# https://github.com/haskell/cabal/issues/7914

set -euo pipefail

target="$1"
shift

echo "building target: ${target}"
cabal build "$target"

executable=$(cabal list-bin "$target")

exec "$executable" "$@"
