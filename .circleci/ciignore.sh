#!/usr/bin/env bash

set -eo pipefail
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

if [[ ! -a "$ROOT/.ciignore" ]]; then
	  exit # If .ciignore doesn't exists, just quit this script
fi

changes="$(git diff-tree --no-commit-id --name-only -r origin/master..HEAD)"

echo "CHANGES FROM ORIGIN/MASTER:"
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
    echo "Files that are not ignored present in commits, need to build, succeed the job"
	  exit
fi

echo "Only ignored files are present in commits, no need to build, fail the job"
exit 1
