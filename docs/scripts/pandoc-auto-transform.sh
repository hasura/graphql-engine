#!/usr/bin/env bash

# Run this from root of repo - graphql-engine-mono
# ./docs/scripts/pandoc-auto-transform.sh "docs/docs"

# Make sure globstar is enabled
# shopt -s globstar
# cd docs
# for f in **/*.rst; do # Whitespace-safe and recursive
#     # echo "../$1/${f%.*}"
#     mkdir -pv -- "../$1/${f%/*}" && touch -- "../$1/${f%}.txt";
#     pandoc "$f" -f rst -t gfm -s -o "../$1/${f%}.txt";
# done
# cd ..


# wc -l graphql/**/* > files.txt
# tree -f

# run on only one file
# ./docs/scripts/pandoc-auto-transform.sh "graphql/core/actions/create.rst"
# pandoc "docs-old/$1" -f rst -t gfm -s -o "docs/docs/${1%}.txt";

# run on list of files
# Ref: https://stackoverflow.com/a/424142/10208226 to get list of changed files in a commit
# git diff-tree --no-commit-id --name-only -r 90c8f75
# ./docs/scripts/pandoc-auto-transform.sh
# files=( "graphql/core/databases/ms-sql-server/index.rst" "graphql/core/mutations/ms-sql-server/index.rst" "graphql/core/mutations/ms-sql-server/insert.rst" "graphql/core/mutations/ms-sql-server/upsert.rst" "graphql/core/mutations/postgres/upsert.rst" )
# for f in "${files[@]}"; do
#     mkdir -pv -- "docs/${f%/*}" && touch -- "docs/docs/${f%}.txt";
#     pandoc "docs-old/$f" -f rst -t gfm -s -o "docs/docs/${f%}.txt";
# done

  # mkdir -pv -- "$1/${f%/*}" && touch -- "$1/${f%}.txt";
  # pandoc "$f" -f rst -t gfm -s -o "$1/${f%}.txt";