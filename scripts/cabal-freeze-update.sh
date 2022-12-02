#!/usr/bin/env bash
set -euo pipefail
shopt -s globstar

help(){
cat << EOF
  USAGE: $0 (--normalize | --all | [--target <package_name> <package_version>]...)

This script is to help updating the freeze file to bring in newer versions of
dependencies. It takes care of a few gotchas and footguns for you (see source).

The simplest mode, when called with '--normalize', will reuse the existing
constraints and just re-freeze, to make sure the file doesn't have extra or
missing information.

When called with '--all', it removes all existing constraints, finds a brand
new build plan, and freezes it.

Instead if you want to upgrade one or more particular packages, while keeping
as many of the other dependencies the same as possible, you can use '--target
foo 1.2.3.4'. This may take a bit of time, and might fail to find a plan,
especially if you pass more than one target.
EOF
}

echo_pretty() { echo ">>> $(tput setaf 2)$1$(tput sgr0)" ; }
echo_error()  { echo ">>> $(tput setaf 1)$1$(tput sgr0)" ; }
echo_warn()   { echo ">>> $(tput setaf 3)$1$(tput sgr0)" ; }

# absolute paths to files we care about:
REPO_TOPLEVEL=$(git rev-parse --show-toplevel)
FREEZE_FILE="$REPO_TOPLEVEL/cabal.project.freeze"

# Make sure freeze file isn't dirty so we can freely write to it:
if ! git diff --quiet HEAD "$FREEZE_FILE" ; then
    echo_error "It looks like cabal.project.freeze already has some changes, which we don't want to clobber. Please commit or remove them."
    exit 1
fi


### Argument parsing / exit handling

# map of: package_name->package_version
declare -A PACKAGE_TARGETS
SKIP_UPDATE=false
UPGRADE_ALL=false
KEEP_TRYING=true

if [[ $# -eq 0 ]]; then
    echo_error "expecting at least one argument"
    help
    exit 1
fi

while [[ $# -gt 0 ]]; do
  case $1 in
    --normalize)
      SKIP_UPDATE=true
      KEEP_TRYING=false
      shift # past argument
      ;;
    --all)
      UPGRADE_ALL=true
      shift # past argument
      ;;
    --target)
      PACKAGE_TARGETS["$2"]="$3"
      shift ; shift ; shift # past values
      ;;
    *)
      echo_error "Unknown option $1"
      help
      exit 1
      ;;
  esac
done

# In case something goes wrong once we start messing with freeze file...
err_report() {
    echo_error "Error on line $(caller)"
    echo_error "Freeze file is probably messed up, so you probably want to:"
    echo_error "    $ git checkout $FREEZE_FILE"
}
trap '[[ $? != 0 ]] && err_report' EXIT


if "$UPGRADE_ALL"; then
    # Remove all constraints and write new build plan
    rm "$FREEZE_FILE"
    cabal update
    cabal freeze --enable-tests --enable-benchmarks --minimize-conflict-set
else
    if ! "$SKIP_UPDATE"; then
        echo_pretty "Doing 'cabal update'... "
        # First we need to remove the frozen `index-state` so that `cabal update` grabs the latest index
        sed -i '/^index-state:.*/d' "$FREEZE_FILE"
    fi

    cabal update

    echo_pretty "Trying to come up with a new plan with a minimal delta. This may take some time."
    # Replace target dependencies with requested versions:
    for package_name in "${!PACKAGE_TARGETS[@]}"; do
        package_version="${PACKAGE_TARGETS[$package_name]}";
        # Remove existing target entries (baked in flag lines may not be present):
        sed -ri "/\s+any.$package_name ==/d" "$FREEZE_FILE"
        sed -ri "/\s+$package_name /d" "$FREEZE_FILE"  # baked in flags
        # add back target version
        sed -i "\$s/\$/ $package_name ==$package_version,/" "$FREEZE_FILE"
    done

    if "$KEEP_TRYING"; then
        freeze_line_count_orig=$(wc -l "$FREEZE_FILE" | awk '{print $1}')
        freeze_line_count_prev=$freeze_line_count_orig # mutable
        while : ; do
            if out=$(cabal freeze --enable-tests --enable-benchmarks --minimize-conflict-set 2>&1); then
                break
            else
                # newline-separated:
                conflict_set=$(echo "$out" |  tr '\n' ' ' | sed -r 's/^.*conflict set: ([^\)]+)\).*$/\1/' | tr ',' '\n' | tr -d ' ')
                if [ -z "$conflict_set" ]; then
                    echo_error "Something went wrong :/"
                    exit 77
                fi
                # omit target packages:
                for package_name in "${!PACKAGE_TARGETS[@]}"; do
                    conflict_set=$(echo "$conflict_set" | sed "/^$package_name$/d")
                done
                # filter conflicts from the freeze file
                while IFS= read -r package_name; do
                    sed -ri "/\s+any.$package_name ==/d" "$FREEZE_FILE"
                    sed -ri "/\s+$package_name /d" "$FREEZE_FILE"  # baked in flags
                done <<< "$conflict_set"

                freeze_line_count=$(wc -l "$FREEZE_FILE" | awk '{print $1}')
                if [ "$freeze_line_count" -eq "$freeze_line_count_prev" ]; then
                    # No longer making progress, so...
                    echo_error "It looks like we can't find a build plan :("
                    echo_error "With the freeze file in its current state, try doing:"
                    echo_error "    $ cabal freeze --enable-tests --enable-benchmarks --minimize-conflict-set"
                    echo_error "Exiting"
                    exit 31
                else
                    echo -ne "Relaxed $((freeze_line_count_orig-freeze_line_count)) constraints so far...\r"
                    freeze_line_count_prev=$freeze_line_count
                    # ...and try again
                fi
            fi
        done
    else
        cabal freeze --enable-tests --enable-benchmarks --minimize-conflict-set
    fi
    echo
fi

### Finally do a little cleanup/normalizing:

# Remove graphql engine internal mono-repo packages. This doesn't matter unless
# we happen to bump the version number in one of our cabal files.
sed -ri "/\s+graphql-engine/d" "$FREEZE_FILE"
# Remove all flags from the freeze file. By default cabal bakes in the default
# flags for a library; This makes it very difficult to review the freeze file
# to determine where we might be either intentionally or unintentionally
# overriding default flags, and it's easy for flags from a local developer’s
# environment to get accidentally committed. This is checked in CI.  For
# discussion, see:
#   https://hasurahq.slack.com/archives/CV3UR1MT2/p1654544760362949
#   https://github.com/hasura/graphql-engine-mono/pull/4618
sed -ri "/\s+\S+ [+-]/d" "$FREEZE_FILE"

echo_pretty "Success!(?) Be sure to check that we were actually able to get a plan with package versions you hoped for"
