#!/usr/bin/env bash

# Run this from root of repo - graphql-engine-mono
# ./docs-new/scripts/pandoc-auto-transform.sh "docs-new/docs"

# Make sure globstar is enabled
shopt -s globstar
cd docs
for f in **/*.rst; do # Whitespace-safe and recursive
    # echo "../$1/${f%.*}"
    mkdir -pv -- "../$1/${f%/*}" && touch -- "../$1/${f%}.txt";
    pandoc "$f" -f rst -t gfm -s -o "../$1/${f%}.txt";
done
# wc -l graphql/**/* > files.txt
# tree -f
cd ..

  # mkdir -pv -- "$1/${f%/*}" && touch -- "$1/${f%}.txt";
  # pandoc "$f" -f rst -t gfm -s -o "$1/${f%}.txt";