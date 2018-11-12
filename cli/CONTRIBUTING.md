# Contributing

Thanks for your interest in Hasura GraphQL Engine CLI.

## Pre-requisites

- [Go 1.10](https://golang.org/doc/install)
- [GNU Make](https://www.gnu.org/software/make/)


You can follow your existing Golang workflow to fork, work on a branch and
submit PR. If you're new to forking and working on Golang repositories, please
follow the instructions below to make sure the import paths are correct:

- Fork the repo on GitHub
- `mkdir -p $GOPATH/src/github.com/hasura`
- `cd $GOPATH/src/github.com/hasura`
- `git clone https://github.com/<your-username>/graphql-engine`
- `cd graphql-engine`
- `git remote add upstream https://github.com/hasura/graphql-engine`
- `git checkout -b <branch-name>`
- `make deps`
- Work on the feature/fix
- If you modify files in `assets/`, run `make assets`
- Add tests and ensure all tests are passing (check [Tests](#tests) section below)
- Commit, push and submit PR

## Development workflow

We suggest using [realize](https://github.com/oxequa/realize) for faster dev
workflow. The `.realize.yaml` config is already included in the repo.

- Install realize
  ```bash
  go get github.com/oxequa/realize
  ```
- Start realize
  ```bash
  realize start
  ```

`realize` watches the directory for changes and rebuilds the cli whenever a new
change happens. The cli is installed to `$GOPATH/bin/hasura`, which should
already be in your `PATH`. The config is located at `.realize/realize.yaml`.

## Tests

When you're adding a new feature, it is encouraged to add integration tests
(unit tests also if possible) for the functions/api. You should run all the tests
and make sure everything passes before submitting the PR. 

The tests expect a GraphQL Engine server instance to be running. You can point
the tests to any GraphQL Engine server but please note that **the database
should be empty**. The easiest way to do this is to run Postgres and GraphQL
Engine using [Docker
Compose](https://github.com/hasura/graphql-engine/tree/master/install-manifests).
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

This will output cross-platform binaries to `_output` directory.
