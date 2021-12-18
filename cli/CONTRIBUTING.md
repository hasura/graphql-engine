# Contributing

Thank you for your interest in the Hasura GraphQL Engine CLI.

The CLI is written in [Go](https://golang.org/) using the popular
package [`spf13/cobra`](https://github.com/spf13/cobra).

## Find an issue to work on

All CLI related issues are labelled as [`c/cli`](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Ac%2Fcli+). If you're new to the CLI codebase, you can check out the
[`good-first-issue`](https://github.com/hasura/graphql-engine/issues?q=is%3Aissue+is%3Aopen+label%3Ac%2Fcli+label%3A%22good+first+issue%22) label for issues that
should be fairly easy to implement.

For first-time contributors, we have set aside some time slots for
one-on-one pair programming with the team, to get you started quickly. :smile:
If you're interested in setting up a chat, please feel free to pick a slot
from [shahidhk](https://github.com/shahidhk)'s [calendar](https://calendly.com/shahidhk).

## Pre-requisites

The following dependencies need to be installed in order to work on the CLI codebase:

- [Go >= 1.11](https://golang.org/doc/install)
- [GNU Make](https://www.gnu.org/software/make/) (optional)

## Set up the repository

You can follow your existing Golang workflow to fork, work on a branch and
submit PR. If you're new to forking and using branches, please
execute the following commands and make sure the import paths are correct:

- Fork the repo on GitHub
- `git clone https://github.com/<your-username>/graphql-engine`
- `cd graphql-engine/cli`
- `git remote add upstream https://github.com/hasura/graphql-engine`
- `git checkout -b <branch-name>`

## Install dependencies

Install the dependencies as follows:

```bash
make deps
```

## Development

Once everything is installed and running, you can start working on the feature/fix that you picked up.

For a faster development workflow, you may use tools that watch the directory for changes and rebuilds the cli whenever a new change happens. [realize](https://github.com/oxequa/realize) and [watchrun](https://github.com/loov/watchrun) are two such examples. The configuration file for `realize` is already included in the repo at `.realize/realize.yaml`.

### Run the server

### Assets

If you modify files in `assets/`, run `make assets`.

### Submit a PR

Commit, push and submit the PR.

## Tests

When you're adding a new feature, it is encouraged to add integration tests
and unit tests wherever possible. Please run all the tests
and make sure everything passes before submitting the PR.

The tests expect a GraphQL Engine server instance to be running. You can point
the tests to any GraphQL Engine server but please note that **the database
should be empty**. The easiest way to do this is to run Postgres and GraphQL
Engine using [Docker
Compose](https://github.com/hasura/graphql-engine/tree/stable/install-manifests/docker-compose).
Once the server is running, you can run the tests by executing the make command:

```bash
HASURA_GRAPHQL_TEST_ENDPOINT=http://localhost:8080 VERSION=dev make test
```

## Builds

To build a binary, execute the following command:

```bash
# make deps to install all go packages
make build
```

This will output cross-platform binaries to the `_output` directory.
