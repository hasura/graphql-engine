#!/usr/bin/env bash

# Run this from root of repo - graphql-engine-mono
# ./docs-new/scripts/pandoc-auto-transform.sh "docs-new/docs"

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
# ./docs-new/scripts/pandoc-auto-transform.sh "graphql/core/actions/create.rst"
# pandoc "docs/$1" -f rst -t gfm -s -o "docs-new/docs/${1%}.txt";

# run on list of files
# ./docs-new/scripts/pandoc-auto-transform.sh
# files=( "graphql/core/databases/postgres/schema/remote-relationships/remote-schema-relationships.rst" "graphql/core/remote-schemas/adding-schema.rst" "graphql/core/remote-schemas/auth/remote-schema-permissions.rst" )
# for f in "${files[@]}"; do
#     mkdir -pv -- "docs/${f%/*}" && touch -- "docs-new/docs/${f%}.txt";
#     pandoc "docs/$f" -f rst -t gfm -s -o "docs-new/docs/${f%}.txt";
# done

  # mkdir -pv -- "$1/${f%/*}" && touch -- "$1/${f%}.txt";
  # pandoc "$f" -f rst -t gfm -s -o "$1/${f%}.txt";