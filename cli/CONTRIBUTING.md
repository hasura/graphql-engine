# Contributing

Thank you for your interest in the Hasura GraphQL Engine CLI.

The CLI is written in [Go](https://golang.org/) using the popular
package [`spf13/cobra`](https://github.com/spf13/cobra).

## Find an issue to work on

All CLI related issues are labelled as [`c/cli`](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen+label%3Ac%2Fcli+). If you're new to the CLI codebase, you can check out the
[`good-first-issue`](https://github.com/hasura/graphql-engine/issues?q=is%3Aissue+is%3Aopen+label%3Ac%2Fcli+label%3A%22good+first+issue%22) label for issues that
should be fairly easy to implement.

If you are a first-time contributor, feel free to post your doubts/questions in `#contrib` channel on [hasura discord server](https://discord.com/invite/hasura).

## Pre-requisites

The following dependencies need to be installed in order to work on the CLI codebase:

- [Docker](https://www.docker.com/get-started/)
- [Docker Compose](https://docs.docker.com/compose/install/)
- [Go >= 1.16](https://golang.org/doc/install)
- [Node.js >= 10.19.0 and npm >= 6.14.4](https://nodejs.org/en/download/)
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

### Run the server

[Our Docker Compose manifest](https://github.com/hasura/graphql-engine/tree/stable/install-manifests/docker-compose) is the easiest way to run graphql-engine.

1. From the `graphql-engine` directory, run `cd install-manifests/docker-compose`.
2. From inside that directory, run `docker compose up -d`.

The GraphQL endpoint will be at `https://localhost:8080/v1/graphql`.

The console will be available at `https://localhost:8080/console`.

### Assets

If you modify files in `assets/`, run `make assets`.

### Submit a PR

Commit, push and submit the PR.

## Tests

When you're adding a new feature, it is encouraged to add integration tests
and unit tests wherever possible.

**Please run all the tests and make sure everything passes before submitting the PR.**

1. Set `HASURA_TEST_CLI_HGE_DOCKER_IMAGE` to the docker image of the server that you want the tests to run against. For example: `export HASURA_TEST_CLI_HGE_DOCKER_IMAGE="hasura/graphql-engine:v2.1.0"`.

2. e2e tests look for a hasura binary in PATH, you can either make sure it's available as `hasura` in your PATH or set `HASURA_TEST_CLI_PATH` to the binary path that you want to run the tests with. You'll get the path from running the build (see "Builds" below).

3. Run the full suite by executing `make test-all`.

## Builds

To build a binary, execute the following command:

```bash
make build
```

If you get an error running the above, run this command:

```
make build-cli-ext copy-cli-ext
```

This will output cross-platform binaries to the `_output` directory.
