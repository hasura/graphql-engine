#! /usr/bin/env bash
set -euo pipefail
shopt -s globstar

# This script extracts all long-form Notes from the source, and creates a
# corresponding markdown file for each, with links to where it is mentioned.
# All files are created in server/documentation/notes.


################################################################################
# Helper functions

# This function takes a file name, and generates the corresponding Haskell
# module name.
#   server/src-lib/Hasura/SQL/Types.md -> Hasura.SQL.Types
function filenameToModule() {
  echo "$1" | sed 's%.*src-lib/\(.*\)\.hs%\1%' | tr "/" "."
}

# This function converts a note's name into a markdown file name.
#   "Tying the knot" -> tying-the-knot
function noteNameToFileName() {
  echo "$1" \
    | tr '[:upper:]' '[:lower:]' \
    | tr -cd 'a-z0-9 ' \
    | tr -s ' ' '-'
}

# Makes a github link out of a filename and line number
function toGithubLink() {
  echo "https://github.com/hasura/graphql-engine/blob/master/server/$1#L$2"
}

# Generate the custom header at the top of the note file.
function generateHeader() {
  local title="$1"
  local filename="$2"
  local line="$3"
  local mentions

  # Find all places that mention the note. This will match the note itself, and
  # we will therefore only consider mentions is there's more than one. We filter
  # the "erroneous" mention when generating the list.
  # The sort expression is here to make sure the cross-references are always in
  # the same order. We cut along the ':', and then sort first by filename, the
  # first key, key "1,1", then by line number, the second key, "2n,2", using a
  # numerical sort "n".
  mentions=$(grep -inR "note \[$title\]" src-lib/ | cut -d : -f 1,2 | sort -t : -k 1,1 -k 2n,2 || true)

  echo "This note is in [$(filenameToModule "$filename")]($(toGithubLink "$filename" "$line"))."
  if [[ $(echo "$mentions" | wc -l) -gt 1 ]]; then
    echo "It is referenced at:"
    for mention in $mentions; do
      read -r f l < <(echo "$mention" | tr ':' ' ')
      if [[ "$f" != "$filename" || "$l" != "$line" ]]; then
        echo "  - line $l of [$(filenameToModule "$f")]($(toGithubLink "$f" "$l"))"
      fi
    done
  fi

  echo
  echo "# $title"
  echo
}


################################################################################
# Main

# Switch to the server folder.
# https://stackoverflow.com/a/246128/176841
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../server" &> /dev/null && pwd -P)
cd "$ROOT" &> /dev/null

# Generate index file
mkdir -p "documentation/notes/"
cat > "documentation/notes/README.md" <<EOF
# Notes

EOF

# Iterate over files that contain a Note.
grep -ERl "^{- Note \[" src-lib | sort -u | while read -r file; do
  lc=0
  skip=0
  note=""
  target=""

  function startNote() {
    note="$1"
    basename="$(noteNameToFileName "$note").md"
    target="documentation/notes/$basename"
    startline="$lc"
    echo "INFO $file:$startline: found note \"$note\""
    generateHeader "$note" "$file" "$lc" > "$target"
    # Register that note in the index.
    echo "  - [$note]($basename)" >> "documentation/notes/README.md"
    # Set skip to 1 to avoid keeping the ~~~~~ line
    skip=1
  }

  # Loop over the file's line
  # Setting IFS to null avoids discarding leading whitespace.
  while IFS= read -r line; do
    lc=$(( lc + 1 ))
    if test -z "$note"; then
      # We are not in a note yet.
      note=$(echo "$line" | sed -n 's/^{- Note \[\(.*\)\]$/\1/p')
      if test -n "$note"; then
        # The current line is the start of one: we create the markdown file.
        startNote "$note"
      fi
    else
      # Sometimes, the same comment block contains more than one note.
      nested=$(echo "$line" | sed -n 's/^Note \[\(.*\)\]$/\1/p')
      if test -n "$nested"; then
        startNote "$nested"
      else
        # We are in a note: the current line should be copied verbatim to the
        # target file, unless we are on the closing line or on a skipped line.
        if [[ $skip -eq 1 ]]; then
          skip=0
        else
          # shellcheck disable=SC2001
          echo "$line" | sed 's/ *-}$//' >> "$target"
        fi
        # If this was the closing line, we revert to normal iteration.
        if echo "$line" | grep -qE -- '-}$'; then
          note=""
        fi
      fi
    fi
  done < "$file"
done
