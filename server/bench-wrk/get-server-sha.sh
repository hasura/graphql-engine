#!/bin/bash
# We are generating shasum of files in the server directory (excluding packaging and test files).
# This is to track whether server directory has changed or not
dir=$(dirname $0)
cd $dir/../..
git ls-files -- ':!tests-py' . ':!packaging' . ':!.*' . ':!bench-wrk' | sort | xargs cat | shasum | awk '{print $1}' | tail -c 9
