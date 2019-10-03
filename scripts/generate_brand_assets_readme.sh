#!/usr/bin/env bash
#
# update readme file in the assets/brand folder

# strict mode
set -Eeuo pipefail

IFS=$'\n\t'

# get the repo root
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

cd "$ROOT/assets/brand"

README_CONTENT=$(cat <<EOF
# Hasura Brand Assets

All assets are available on a Google Cloud Storage Bucket CDN at \`https://graphql-engine-cdn.hasura.io/img/<name>\`.

*Note: some logos maybe invisible depending on the page background color.*

| name | asset |
| ---- | ----- |
EOF
)

for svg in *.svg; do
  if [[ "$svg" == *"white"* ]]; then
    BG='style="background-color: black;"'
  elif [[ "$svg" == *"black"* ]] || [[ "$svg" == *"blue"* ]]; then
    BG='style="background-color: white;"'
  else
    continue
  fi
  README_CONTENT="$(cat <<EOF
$README_CONTENT
| \`$svg\` | <img src="$svg" width="150px" $BG /> |
EOF
)"
done

for svg in *.svg; do
  if [[ "$svg" == *"white"* ]] || [[ "$svg" == *"black"* ]] || [[ "$svg" == *"blue"* ]]; then
    continue
  fi
  README_CONTENT="$(cat <<EOF
$README_CONTENT
| \`$svg\` | <img src="$svg" width="150px"/> |
EOF
)"
done

echo "$README_CONTENT" > "$ROOT/assets/brand/README.md"
