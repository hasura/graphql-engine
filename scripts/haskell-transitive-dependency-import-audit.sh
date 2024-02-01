#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

## This tries to audit our transitive dependencies for occurrences of
## problematic imports or function names.  very basic for now, can be 
## extended. For now depends on ripgrep.
if [ -z "$1" ]; then
    echo "pass search string as first argument"
    exit 1
fi

REPO_TOPLEVEL=$(git rev-parse --show-toplevel)
FREEZE_FILE="$REPO_TOPLEVEL/cabal.project.freeze"

if [ ! -f "$FREEZE_FILE" ]; then
    echo "Freeze file not found"
    exit 1
fi

# Temp dir in RAM so we don't thrash SSD
TEMP_DIR=$(mktemp -d /dev/shm/hasura_dep_audit.XXXXXX)
function cleanup {
    rmdir "$TEMP_DIR" || echo "$TEMP_DIR was not empty and could not be removed so it probably contains matching libraries you'll want to check out by hand"
}
trap cleanup EXIT

# Read the freeze file and extract package names and versions
rg '^.* any\.([^ ]*) ==([^,]*),?' -r '$1-$2' "$FREEZE_FILE" | while read -r pkg_identifier; do
    # Download the package
    cabal get -d "$TEMP_DIR" "$pkg_identifier" >/dev/null || echo "   continuing anyway..."

    if rg -q "$1" -ths "${TEMP_DIR:?}/$pkg_identifier"; then
        echo
        echo "Occurrence in $pkg_identifier"
    else
        echo -n .
        # Clean up if nothing to see
        rm -rf "${TEMP_DIR:?}/$pkg_identifier"
    fi
done

