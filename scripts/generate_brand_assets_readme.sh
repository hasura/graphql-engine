#!/usr/bin/env bash
#
# update readme file in the assets/brand folder

# exit on error
set -e

IFS=$""

# get the repo root
ROOT="$(readlink -f ${BASH_SOURCE[0]%/*}/../)"

cd "$ROOT/assets/brand"

README_CONTENT=$(cat <<EOF
# Hasura Brand Assets

All assets are available on a Google Cloud Storage Bucket CDN at \`https://graphql-engine-cdn.hasura.io/img/<name>\`.

| name | asset |
| ---- | ----- |
EOF
)

for svg in *.svg; do
    if [[ "$svg" = *"white"* ]]; then
        BG='style="background-color: black;"'
    else
        BG='style="background-color: white;"'
    fi
    README_CONTENT=$(cat <<EOF
$README_CONTENT
| \`$svg\` | <img src="$svg" width="150px" $BG /> |
EOF
)
done

echo $README_CONTENT > "$ROOT/assets/brand/README.md"
