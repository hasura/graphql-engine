# Contributing to Hasura graphql-engine

*First*: if you're unsure or afraid of anything, just ask, or submit the issue or
pull request anyway. You won't be yelled at for giving your best effort. The
worst that can happen is that you'll be politely asked to change something. We
appreciate any contributions, and we don't want a wall of rules to get in
the way of that.

However, for those individuals who want a bit more guidance on the best way to
contribute to the project, read on. This document will cover what we're looking
for. By addressing all the points we're looking for, the chances that we
can quickly merge or address your contributions will increase.


## Overview

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) is a mono-repo
consisting of 3 components. Each has their own contributing guides:

1. [Server (Haskell)](server/CONTRIBUTING.md)
2. [CLI (Go)](cli/CONTRIBUTING.md)
3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

All of the three components have a single version, denoted by either the git
tag or a combination of branch name and git commit SHA.

For all contributions, a CLA (Contributor License Agreement) needs to be signed [here](https://cla-assistant.io/hasura/graphql-engine) before (or after) the pull request has been submitted. A bot will prompt contributors to sign the CLA via a pull request comment, if necessary.


### Docs

Contributing guide for docs can be found at [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### Community content

There is no specific contributing guide for community content. Anything that can
help GraphQL Engine community/users can go into the section. We have identified
[boilerplates](community/boilerplates), [examples](community/examples) and
[tools](community/tools) as primary candidates. Feel free to submit a pull
request if you have something to add (not necessarily belonging to the
aforementioned sections).

## Issues

### Reporting an Issue

- Make sure you test against the latest released version. It is possible that we
  may have already fixed the bug you're experiencing.

- Provide steps to reproduce the issue, including Postgres version,
  graphql-engine version and the provider you are running on (Heroku, Docker,
  etc.).

- Please include logs of the server, if relevant.


## Common guidelines

- Please make sure there is an issue associated with the work that you're doing.

- If you're working on an issue, please comment that you are doing so to prevent
  duplicate work by others also.

- Squash your commits and refer to the issue using `fix #<issue-no>` or `close
  #<issue-no>` in the commit message, at the end.
  For example: `resolve answers to everything (fix #42)` or `resolve answers to everything, fix #42`

- Rebase master with your branch before submitting a pull request.

## Commit messages

- The first line should be a summary of the changes, not exceeding 50
  characters, followed by an optional body which has more details about the
  changes. Refer [to this link](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  for more information on writing good commit messages.

- Use the imperative present tense: "add/fix/change", not "added/fixed/changed" nor "adds/fixes/changes".

- Don't capitalize the first letter of the summary line.

- Don't add a period/dot (.) at the end of the summary line.

## Translations

This document is available in the following translations:

- [French :fr:](translations/CONTRIBUTING.french.md)

(Credits: Some sections are adapted from https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
