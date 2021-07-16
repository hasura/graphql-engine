# Contributing

Thanks for your interest in Hasura GraphQL Engine CLI.

The CLI is written in [Go](https://golang.org/) using the popular
package [`spf13/cobra`](https://github.com/spf13/cobra).

All CLI related issues are labelled as [`c/cli`](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Ac%2Fcli+) which stands for 
component/cli. If you're new to the CLI codebase, you can checkout the
[`good-first-issue`](https://github.com/hasura/graphql-engine/issues?q=is%3Aissue+is%3Aopen+label%3Ac%2Fcli+label%3A%22good+first+issue%22) label for issues that
are fairly easy to implement.

If you are a first-time contributor, feel free to post your doubts/questions in `#contrib` channel on [hasura discord server](https://discord.com/invite/hasura).

## Pre-requisites

- [Go >= 1.16](https://golang.org/doc/install)
- [Node.js >= 10.19.0 and npm >= 6.14.4](https://nodejs.org/en/download/)
- [GNU Make](https://www.gnu.org/software/make/) (optional)

You can follow your existing Golang workflow to fork, work on a branch and
submit PR. If you're new to forking and working on Golang repositories, please
follow the instructions below to make sure the import paths are correct:

- Fork the repo on GitHub
- `git clone https://github.com/<your-username>/graphql-engine`
- `cd graphql-engine/cli`
- `git remote add upstream https://github.com/hasura/graphql-engine`
- `git checkout -b <branch-name>`
- `make deps`
- `make build-cli-ext`
- Work on the feature/fix
- If you modify files in `assets/`, run `make assets`
- Add tests and ensure all tests are passing (check [Tests](#tests) section below)
- Commit, push and submit PR

## Development workflow

For faster development workflow, you may use tools that watch the directory for changes and rebuilds the cli whenever a new change happens. [realize](https://github.com/oxequa/realize) and [watchrun](https://github.com/loov/watchrun) are two such examples. The configuration file for `realize` is already included in the repo at `.realize/realize.yaml`.

## Tests

When you're adding a new feature, it is encouraged to add integration tests
(unit tests also if possible) for the functions/api. You should run all the tests
and make sure everything passes before submitting the PR. 

The tests expect a GraphQL Engine server instance to be running. You can point
the tests to any GraphQL Engine server but please note that **the database
should be empty**. The easiest way to do this is to run Postgres and GraphQL
Engine using [Docker
Compose](https://github.com/hasura/graphql-engine/tree/stable/install-manifests).
Once the server is running, you can run the tests by executing the make command:  

```bash
HASURA_TEST_CLI_HGE_DOCKER_IMAGE="hasura/graphql-engine:v2.0.1" HASURA_GRAPHQL_TEST_ENDPOINT=http://localhost:8080 VERSION=dev make test
```

## Builds

To build a binary, execute the following command:

```bash
# make deps to install all go packages
make build
```

This will output cross-platform binaries to `_output` directory.
