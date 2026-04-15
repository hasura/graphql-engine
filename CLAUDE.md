# CLAUDE.md

## Pull Requests

When creating pull requests, always read `.github/PULL_REQUEST_TEMPLATE.md`
first and use it as the basis for the PR body. Make sure to fill in the correct
checkboxes and put your changelog entry in the appropriate spot. This is used
for release automation so the structure needs to stay intact.

## Building and typechecking

For any haskell code changes use `cabal build all --enable-tests --enable-benchmarks` 
to check your work and iterate until it builds cleanly.
