#!/usr/bin/env sh
#!/bin/bash

set -e

NGINX_LOCATIONS=""
CURRENT_BRANCH=$(git name-rev --name-only HEAD)

checkout () {
    echo "$1 Release"
    if git checkout -b release-$1; then
        echo "New Branch pulled"
    else
        git checkout release-$1
        echo "Pulled existing branch"
    fi
    git pull origin release-$1
    BUILDDIR="_build/$1" BUILDVERSION="$1" make html-images
    mv _build/algolia_index _build/$1
    NGINX_LOCATIONS="$NGINX_LOCATIONS $1"
}

for branch in $(git branch -r); do
    REGEX_MATCH=$(echo "$branch" | cut -d'-' -f 2)
    echo "$REGEX_MATCH"  | grep -Eq '^[-+]?[0-9]+\.?[0-9]*$' && checkout $REGEX_MATCH
done

python ./script/template.py "$NGINX_LOCATIONS"
cp ./script/conf/mime.types ./_build/conf/mime.types
cp ./script/conf/robots.txt ./_build/robots.txt

git checkout $CURRENT_BRANCH
