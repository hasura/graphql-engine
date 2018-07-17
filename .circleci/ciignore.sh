#!/usr/bin/env bash

set -eo pipefail
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

if [[ ! -a "$ROOT/.ciignore" ]]; then
	  exit # If .ciignore doesn't exists, just quit this script
fi

COMMIT_RANGE=$(echo $CIRCLE_COMPARE_URL | sed 's:^.*/compare/::g')
echo "COMMIT RANGE: $COMMIT_RANGE"
changes="$(git diff $COMMIT_RANGE --name-only)"

echo "CHANGES:"
echo $changes
echo

# Load the patterns we want to skip into an array
mapfile -t blacklist < "$ROOT/.ciignore"

for i in "${blacklist[@]}"
do
	  # Remove the current pattern from the list of changes
	  changes=( ${changes[@]/$i/} )

	  if [[ ${#changes[@]} -eq 0 ]]; then
		    # If we've exhausted the list of changes before we've finished going
		    # through patterns, that's okay, just quit the loop
		    break
	  fi
done

if [[ ${#changes[@]} -gt 0 ]]; then
	  # If there's still changes left, then we have stuff to build, leave the commit alone.
    echo "Need to build, succeed the job"
	  exit
fi

echo "No need to build, fail the job"
exit 1
