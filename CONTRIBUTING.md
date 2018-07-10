# Contributing

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) is a mono-repo consisting of 3 components. Each have their own contributing guides:

1. [Server (Haskell)](server/CONTRIBUTING.md)
2. [CLI (Go)](cli/CONTRIBUTING.md)
3. [Console (JavaScript)](console/CONTRIBUTING.md)

All of the three components have a single version, denoted by either the git tag or a combination of branch name and git commit sha.

## Common guidelines

- Please make sure there is an issue associated with the work the work you're doing.
- If you're working on an issue, please comment that you are doing so to prevent duplicate work by others also.
- Squash your commits and refer to the issue using `fix #<issue-no>` or `close #<issue-no>` in the commit message.
- Commit messages:
  - Please keep the it under 100 characters. This allows the message to be easier to read on github as well as in various git tools and produces a nice, neat commit log
  - Use the imperative present tense: "change" not "changed" nor "changes"
  - Don't capitalize the first letter
  - Do not add a dot (.) at the end