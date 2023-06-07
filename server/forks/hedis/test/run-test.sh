#!/usr/bin/env sh
set -e

echo "**These tests assume there is a redis server running on port 6379**"

# The -M argument limits heap size for 'testConstantSpacePipelining'.
cabal exec cabal test doctest
cabal test hedis-test --test-options="+RTS -M10m"

echo "------------------"
echo "hlint suggestions:"
echo "------------------"
find src ! -name 'Commands.hs' ! -type d \
	| xargs -J % hlint % --ignore="Use import/export shortcut"
