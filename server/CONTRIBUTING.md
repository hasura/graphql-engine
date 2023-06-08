# Contributing

This guide explains how to set up the graphql-engine server for development on your own machine and how to contribute.

## Pre-requisites

- [GHC](https://www.haskell.org/ghc/) 9.4.5 and [cabal-install](https://cabal.readthedocs.io/en/latest/)
  - There are various ways these can be installed, but [ghcup](https://www.haskell.org/ghcup/) is a good choice if you’re not sure.
- There are few system packages required like `libpq-dev`, `libssl-dev`, etc. The best place to get the entire list is from the [Dockerfile](../packaging/graphql-engine-base/ubuntu.dockerfile)
- Additional Haskell tools (expected versions can be found in _VERSIONS.json_):
  - [HLint](https://github.com/ndmitchell/hlint), for linting Haskell code
  - [Ormolu](https://github.com/tweag/ormolu), for formatting Haskell code
- [Docker](https://www.docker.com/get-started/)
- [Docker Compose](https://docs.docker.com/compose/install/)

For running the test suite:

- [node.js](https://nodejs.org/en/) (see [.nvmrc](../.nvmrc) for the version), and the bundled NPM version
- Python >= 3.9 with pip3 and virtualenv

For building the Console:

- node.js, as above

Additionally, you will need a way to run a PostgreSQL database server. The `dev.sh` script (described below) can set up a PostgreSQL instance for you via [Docker](https://www.docker.com), but if you want to run it yourself, you’ll need:

- [PostgreSQL](https://www.postgresql.org) >= 10
- [postgis](https://postgis.net)

### Installing tooling with Nix

Simply [install Nix](https://nixos.org/download.html) and type `nix develop`.

If you don't want to start a new shell each time, you can also use [direnv](https://direnv.net/) and [nix-direnv](https://github.com/nix-community/nix-direnv), then create a _.envrc.local_ file with the contents:

```bash
#!/usr/bin/env bash

use flake
```

### Installing tooling with direnv

This project contains scripts for installing project dependencies automatically with [direnv](https://direnv.net/). For more information, see the `.envrc` file in the root.

## Development workflow

You should fork the repo on github and then `git clone https://github.com/<your-username>/graphql-engine`.
After making your changes

### Compile

...console assets:

    $ cd frontend
    $ nvm use
    $ npm ci
    $ npm run server-build:ce
    $ cd ..

...and the server:

    $ ln -s cabal/dev.project cabal.project.local
    $ cabal new-update
    $ cabal new-build graphql-engine

To set up the project configuration to coincide with the testing scripts below, thus avoiding recompilation when testing locally, rather use `cabal/dev-sh.project.local` instead of `cabal/dev.project`:

    $ ln -s cabal/dev-sh.project.local cabal.project.local

#### Compiling on MacOS

If you are on MacOS, or experiencing any errors related to missing dependencies on MacOS, please try [this alternative setup guide](COMPILING-ON-MACOS.md), or try Nix (as above).

### IDE Support

You may want to use [hls](https://github.com/haskell/haskell-language-server)/[ghcide](https://github.com/haskell/ghcide) if your editor has LSP support. A sample configuration has been provided which can be used as follows:

```
ln -s sample.hie.yaml hie.yaml
```

If you have to customise any of the options for ghcide/hls, you should instead copy the sample file and make necessary changes in `hie.yaml` file. Note that `hie.yaml` is gitignored so the changes will be specific to your machine.

```
cp sample.hie.yaml hie.yaml
```

### Run and test via `run-new.sh`

The `run-new.sh` scripts are an active work in progress, and will eventually replace the `dev.sh` option below.

Run the Python integration tests with `./server/tests-py/run-new.sh`.

Filter on specific test files with `./server/tests-py/run-new.sh -- create_async_action_with_nested_output_and_relation.py`

If you have any issues with `run-new.sh`, please create a [GitHub issue](https://github.com/hasura/graphql-engine/issues/new/choose) and run and test via `dev.sh` instead.

### Run and test via `dev.sh`

The `dev.sh` script in the top-level `scripts/` directory is a turnkey solution to build, run, and
test `graphql-engine` using a Docker container to run a Postgres database. **Docker is necessary to
use `dev.sh`.**

To use `dev.sh`, first launch a new postgres container with:

    $ scripts/dev.sh postgres

Then in a new terminal launch `graphql-engine` in dev mode with:

    $ scripts/dev.sh graphql-engine

This command also starts the GraphQL Engine console, which you can access at http://localhost:8181/console.

The `dev.sh` will print some helpful information and logs from both services
will be printed to screen.

You can run the test suite with:

    $ scripts/dev.sh test

This should run in isolation. The output format is described in the [pytest documentation](https://docs.pytest.org/en/latest/usage.html#detailed-summary-report). Errors and failures are indicated by `F`s and `E`s.

Optionally, launch a new container for alternative (MSSQL) backend with:

    $ scripts/dev.sh mssql

Tests can be run against a specific backend (defaulting to Postgres) with the `backend` flag, for example:

    $ scripts/dev.sh test --integration -k TestGraphQLQueryBasicCommon --backend (bigquery|citus|mssql|postgres)

### Run and test manually

If you want, you can also run the server and test suite manually against an instance of your choosing.

#### Run

The following command can be used to build and launch a local `graphql-engine` instance:

```
$ cabal new-run -- exe:graphql-engine \
    --database-url='postgres://<user>:<password>@<host>:<port>/<dbname>' \
    serve --enable-console --console-assets-dir=frontend/dist/apps/server-assets-console-ce
```

This will launch a server on port 8080, and it will serve the console assets if they were built with `npm run server-build:ce` as mentioned above.

#### Test

`graphql-engine` has several test suites, among them:

1. A small set of unit tests and integration tests written in Haskell, in `server/src-test`.

2. A new integration test suite written in Haskell, in `server/lib/api-tests`.

3. An extensive set of end-to-end tests written in Python, in `server/tests-py`.

All sets of tests require running databases:

- some unit tests hit the database, and running the unit test suite requires passing in a postgres connection string,
- the Haskell integration test suite requires databases to run (they can be started via the docker command listed below),
- the Python integration test suite also requires databases AND the engine to be running, which can be started via either the `dev.sh` script, or manually.

##### Running py tests

The easiest way to run the Python integration test suite is by running:

```sh
$ scripts/dev.sh test --integration
```

For more details please check out the [README](./tests-py/README.md).

##### Running the Haskell test suite

There are three categories of unit tests:

- unit tests
- PostgreSQL integration tests (requires a PostgreSQL instance)
- MS SQL Server integration tests (requires a MS SQL Server instance)

The easiest way to run these tests is through `make`, which will automatically spin up and shut down Docker containers for the databases:

```
$ make test-unit
$ make test-integration-postgres
$ make test-integration-mssql
```

If you want to limit to a specific set of tests, use `HSPEC_MATCH`:

```
$ make test-unit HSPEC_MATCH='Memoize'
```

Alternatively, you can use Cabal directly (though you'll have to start the databases yourself):

```
$ cabal run -- graphql-engine:test:graphql-engine-tests
$ HASURA_GRAPHQL_DATABASE_URL='postgres://<user>:<password>@<host>:<port>/<dbname>' \
    cabal run -- graphql-engine:test:graphql-engine-test-postgres
```

##### Running the Haskell integration test suite

Run `make test-backends`. This effectively runs the following two commands:

```
$ docker compose up --detach --wait
$ cabal run api-tests:exe:api-tests
```

For more details please check out the [README](./lib/api-tests/README.md).

##### Running unit tests and recompiling

While working on features, you might want to add unit tests and work through
getting them to pass. This is generally a slow process, but there is a
workaround to allow loading both the `graphql-engine` library and the unit
testing library in `ghcid` at the same time:

```sh
$ ghcid -a -c "cabal repl graphql-engine-tests -f -O0 -fghci-load-test-with-lib" --test Main.main
```

This assumes you already have `HASURA_GRAPHQL_DATABASE_URL` and `HASURA_MSSQL_CONN_STR`
exported as environment variables.

If you just want to run all unit tests, you can add ` --setup ":set args unit"`
to the command line above. If you want to run specific test(s), you can instead
do `--setup ":set args unit --match name_of_test(s)"`.

#### Building with profiling

To build with profiling support, you need to both enable profiling via `cabal`
and set the `profiling` flag. E.g.

```
$ cabal build exe:graphql-engine -f profiling --enable-profiling
```

### Create Pull Request

- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md).
- If you changed the versions of any dependencies, run
  `scripts/cabal-freeze-update.sh --all` to update the freeze file.
- Create a pull request from your forked repo to the main repo.
- Every pull request will automatically build and run the tests.

## Code conventions

The following conventions help us maintain a uniform style for all committers:
make sure your contributions are in line with them.

We enforce these by means of CI hooks which will fail the build if any of these
are not met.

- No compiler warnings: Make sure your code builds with no warnings (adding
  `-Werror` to `ghc-options` in your `cabal.project` is a good way of checking
  this.)
- No lint failures: Use [hlint](https://github.com/ndmitchell/hlint) with our
  custom config to validate your code, using `hlint --hint=../.hlint.yaml`.
- Consistent formatting: Use [ormolu](https://github.com/tweag/ormolu) to
  format your code. `ormolu -ei '*.hs'` will format all files with a `.hs`
  extension in the current directory.
- Consistent style: Consider the [style guide](./STYLE.md) when writing new code.

## Testing

Please see [testing-guidelines](./testing-guidelines.md) for details on how to add tests.

## Local hoogle instance

[Hoogle](https://github.com/ndmitchell/hoogle) is a Haskell API search engine. The server at
[hoogle.haskell.org](https://hoogle.haskell.org/) provides a version of Hoogle that enables
searching through all packanges available in [Stackage](https://www.stackage.org/). Following
instructions help in setting up a local hoogle server that enables searching through `graphql-engine` server code.

### Step 1: Installing hoogle

Installing `hoogle` is fairly simple with `cabal`.

```bash
$ cabal install hoogle
```

### Step 2: Generating hoogle database

A Hoogle database is a prebuilt index of a set of packages. The `hoogle.sh` script in the
top-level `scripts/` directory helps in generating the hoogle database for GraphQL Engine server
code and store it in `dist-newstyle/` directory.

    $ scripts/hoogle.sh generate

### Step 3: Running hoogle instance

Running the following `hoogle.sh` script command starts a local hoogle server with the database
generated in `Step 2`.

    $ scripts/hoogle.sh serve

Use `--port` option to specify custom port to start hoogle server.

    $ scripts/hoogle.sh serve --port 8181
