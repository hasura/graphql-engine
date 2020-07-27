# Contributing to Hasura graphql-engine

_First_: if you feel insecure about how to start contributing, feel free to ask us on our [Discord channel](https://discordapp.com/invite/hasura), in the #contrib channel or as a DM to Marion (@marion#9554). You can also just go ahead with your contribution and we'll give you feedback. Don't worry - the worst that can happen is that you'll be politely asked to change something. We appreciate any contributions, and we don't want a wall of rules to stand in the way of that.

However, for those individuals who want a bit more guidance on the best way to contribute to the project, read on. This document will cover what we're looking for. By addressing the points below, the chances that we
can quickly merge or address your contributions will increase.

## Table of contents

[1. Code of conduct ](#code-of-conduct)

[2. Repo overview ](#overview)

[3. First time contributors welcome! ](#first-timers)

[4. Areas for contributing ](#areas)

[5. Ways of contributing ](#ways)

[6. Commit messages ](#commit-messages)

[7. Translations ](#translations)

<a name="code-of-conduct"></a>

## 1. Code of conduct

Please follow our [Code of conduct](code-of-conduct.md) in the context of any contributions made to Hasura.

<a name="overview"></a>

## 2. Repo overview

[hasura/graphql-engine](https://github.com/hasura/graphql-engine) is a mono-repo
consisting of 3 components. Each has their own contributing guides:

1. [Server (Haskell)](server/CONTRIBUTING.md)

2. [CLI (Go)](cli/CONTRIBUTING.md)

3. [Console (JavaScript)](console/README.md#contributing-to-hasura-console)

All of the three components have a single version, denoted by either the git tag or a combination of branch name and git commit SHA.

For all contributions, a CLA (Contributor License Agreement) needs to be signed [here](https://cla-assistant.io/hasura/graphql-engine) before (or after) the pull request has been submitted. A bot will prompt contributors to sign the CLA via a pull request comment, if necessary.

<a name="first-timers"></a>

## 3. First time contributors welcome!

We appreciate first time contributors and we are happy to assist you in getting started. In case of questions, just reach out to us!

You find all issues suitable for first time contributors [here](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

<a name="areas"></a>

## 4. Areas for contributing

Of course, we appreciate contributions to all components of Hasura. However, we have identified three areas that are particularly suitable for open source contributions.

### Docs

Our goal is to keep our docs comprehensive and updated. If you would like to help us in doing so, we are grateful for any kind of contribution:

- Report missing content

- Fix errors in existing docs

- Help us in adding to the docs

The contributing guide for docs can be found at [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

### Community content

Since we launched our [learn page](https://hasura.io/learn/), we are happy about contributions:

- Fix errors in existing learn tutorials

- Add new tutorials (please reach out to us if you have ideas to avoid duplicate word)

The README of the learn repository can be found [here](https://github.com/hasura/learn-graphql).

Apart from the learn content, we have identified three other ways of contributing with technical community content:

- [Boilerplates](https://github.com/hasura/graphql-engine/tree/master/community/boilerplates)

- [Sample apps](https://github.com/hasura/graphql-engine/tree/master/community/sample-apps)

- [Tools](community/tools)

If you would like to contribute to the community by

- writing a technical blogpost

- speaking at an event

- organizing a workshop

check out our [community wiki](https://github.com/hasura/graphql-engine/wiki/Community-Wiki).

Feel free to submit a pull request if you have something to add even if it's not related to anything mentioned above.

### Hasura CLI

We have some issues on the CLI that are suitable for open source contributions. If you know Go or if you would like to learn it by doing, check out the following [issues](https://github.com/hasura/graphql-engine/issues?q=is%3Aopen+is%3Aissue+label%3Ac%2Fcli+label%3A%22help+wanted%22).

The README of the CLI repository can be found [here](https://github.com/hasura/graphql-engine/tree/master/cli).

<a name="ways"></a>

## 5. Ways of contributing

### Reporting an Issue

- Make sure you test against the latest released version. It is possible that we may have already fixed the bug you're experiencing.

- Provide steps to reproduce the issue, including Postgres version,
  graphql-engine version and the provider you are running on (Heroku, Docker, etc.).

- Please include logs of the server, if relevant.

### Working on an issue

- We use the [fork-and-branch git workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Please make sure there is an issue associated with the work that you're doing.

- If you're working on an issue, please comment that you are doing so to prevent duplicate work by others also.

- Squash your commits and refer to the issue using `fix #<issue-no>` or `close #<issue-no>` in the commit message, at the end.
  For example: `resolve answers to everything (fix #42)` or `resolve answers to everything, fix #42`

- Rebase master with your branch before submitting a pull request.

<a name="commit-messages"></a>

## 6. Commit messages

- The first line should be a summary of the changes, not exceeding 50
  characters, followed by an optional body which has more details about the
  changes. Refer to [this link](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
  for more information on writing good commit messages.

- Use the imperative present tense: "add/fix/change", not "added/fixed/changed" nor "adds/fixes/changes".

- Don't capitalize the first letter of the summary line.

- Don't add a period/dot (.) at the end of the summary line.

<a name="translations"></a>

## 7. Translations

This document is available in the following translations:

- [French 🇫🇷](translations/CONTRIBUTING.french.md)

(Credits: Some sections are adapted from https://github.com/PostgREST/postgrest/blob/master/.github/CONTRIBUTING.md)
