# Contributing to JSON Data Import

*First*: if you're unsure or afraid of anything, just ask or submit the issue or
pull request anyway. You won't be yelled at for giving your best effort. The
worst that can happen is that you'll be politely asked to change something. We
appreciate any sort of contributions, and don't want a wall of rules to get in
the way of that.

However, for those individuals who want a bit more guidance on the best way to
contribute to the project, read on. This document will cover what we're looking
for. By addressing all the points we're looking for, it raises the chances we
can quickly merge or address your contributions.

## Issues

### Reporting an Issue

- Make sure you test against the latest released version. It is possible we
  already fixed the bug you're experiencing.

- Provide steps to reproduce the issue.

- Please include logs, if relevant.

## Common guidelines

- Please make sure there is an issue associated with the work that you're doing.

- If you're working on an issue, please comment that you are doing so to prevent
  duplicate work by others also.

- Squash your commits and refer to the issue using `fix #<issue-no>` or `close
  #<issue-no>` in the commit message, at the end.
  For example: `resolve answers to everything (fix #42)` or `resolve answers to everything, fix #42`

- Rebase master with your branch before submitting pull request.

## Commit messages

 - The first line should be a summary of the changes - not execeeding 50
   characters. Followed by an optional body which has more details about the
   changes. (https://github.com/erlang/otp/wiki/writing-good-commit-messages)

 - Use the imperative present tense: "add/fix/change", not "added/fixed/changed" nor "adds/fixes/changes".

 - Don't capitalize the first letter of the summary line.

 - Do not add a period/dot (.) at the end in the summary line.


(Credits: Some sections are adapted from https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)

## Testing

No pull request will be merged if it does not pass the tests.

To run the tests locally, you will need an instance of [Hasura GraphQL Engine](https://github.com/hasura/graphql-engine) running. To run the tests, run the command:

```
$ TEST_HGE_URL=https://hge.herokuapp.com npm test
```
