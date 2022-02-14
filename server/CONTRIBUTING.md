# Contributing

This guide explains how to set up the graphql-engine server for development on your
own machine and how to contribute.

## Pre-requisites

- [GHC](https://www.haskell.org/ghc/) 8.10.7 and [cabal-install](https://cabal.readthedocs.io/en/latest/)
  - There are various ways these can be installed, but [ghcup](https://www.haskell.org/ghcup/) is a good choice if you’re not sure.
- There are few system packages required like `libpq-dev`, `libssl-dev`, etc. The best place to get the entire list is from the packager [Dockerfile](../.buildkite/dockerfiles/ci-builders/server-builder.dockerfile)

For building console and running test suite:

- [Node.js](https://nodejs.org/en/) (v12+, it is recommended that you use `node` with version `v12.x.x` A.K.A `erbium` or version `14.x.x` A.K.A `Fermium`)
- npm >= 5.7
- python >= 3.5 with pip3 and virtualenv

Additionally, you will need a way to run a Postgres database server. The `dev.sh` script (described below) can set up a Postgres instance for you via [Docker](https://www.docker.com), but if you want to run it yourself, you’ll need:

- [PostgreSQL](https://www.postgresql.org) >= 9.5
- [postgis](https://postgis.net)

### Upgrading npm

If your npm is too old (>= 5.7 required):

    $ npm install -g npm@latest   # sudo may be required

or update your nodejs.


## Development workflow

You should fork the repo on github and then `git clone https://github.com/<your-username>/graphql-engine`.
After making your changes

### Compile

...console assets:

    $ cd console
    $ npm ci
    $ npm run server-build
    $ cd ..

...and the server:

    $ ln -s cabal.project.dev cabal.project.local
    $ cabal new-update
    $ cabal new-build graphql-engine

To set up the project configuration to coincide with the testing scripts below, thus avoiding recompilation when testing locally, rather use `cabal.project.dev-sh.local` instead of `cabal.project.dev`:

    $ ln -s cabal.project.dev-sh.local cabal.project.local

### IDE Support

You may want to use [hls](https://github.com/haskell/haskell-language-server)/[ghcide](https://github.com/haskell/ghcide) if your editor has LSP support. A sample configuration has been provided which can be used as follows:

```
ln -s sample.hie.yaml hie.yaml
```

If you have to customise any of the options for ghcide/hls, you should instead copy the sample file and make necessary changes in `hie.yaml` file. Note that `hie.yaml` is gitignored so the changes will be specific to your machine.

```
cp sample.hie.yaml hie.yaml
```

### Run and test via `dev.sh`

The `dev.sh` script in the top-level `scripts/` directory is a turnkey solution to build, run, and
test `graphql-engine` using a Docker container to run a Postgres database. **Docker is necessary to
use `dev.sh`.**

To use `dev.sh`, first launch a new postgres container with:

    $ scripts/dev.sh postgres

Then in a new terminal launch `graphql-engine` in dev mode with:

    $ scripts/dev.sh graphql-engine

The `dev.sh` will print some helpful information and logs from both services
will be printed to screen.

You can run the test suite with:

    $ scripts/dev.sh test

This should run in isolation.  The output format is described in the [pytest documentation](https://docs.pytest.org/en/latest/usage.html#detailed-summary-report).  Errors and failures are indicated by `F`s and `E`s.

Optionally, launch a new container for alternative (MSSQL) backend with:

    $ scripts/dev.sh mssql

Tests can be run against a specific backend (defaulting to Postgres) with the `backend` flag, for example:

    $ scripts/dev.sh test --integration -k TestGraphQLQueryBasicCommon --backend (bigquery|citus|mssql|postgres)

### Run and test manually

If you want, you can also run the server and test suite manually against an instance of your choosing.

#### Run

The following command can be used to build and launch a local `graphql-engine` instance:

```
cabal new-run -- exe:graphql-engine \
  --database-url='postgres://<user>:<password>@<host>:<port>/<dbname>' \
  serve --enable-console --console-assets-dir=console/static/dist
```

This will launch a server on port 8080, and it will serve the console assets if they were built with `npm run server-build` as mentioned above.

#### Test

`graphql-engine` has several test suites, among them:

  1. A small set of unit tests and integration tests written in Haskell, in `server/src-test`.

  2. A new integration test suite written in Haskell, in `server/tests-hspec`.

  3. An extensive set of end-to-end tests written in Python, in `server/tests-py`.

All sets of tests require running databases:

- some unit tests hit the database, and running the unit test suite requires passing in a postgres connection string,
- the Haskell integration test suite requires databases to run (they can be started via the docker command listed below),
- the Python integration test suite also requires databases AND the engine to be running, which can be started via either the `dev.sh` script, or manually.

##### Running py tests

The easiest way to run the Python integration test suite is by running:
```sh
scripts/dev.sh test --integration
```

For more details please check out the `README` at `server/tests-py/README`.

##### Running the Haskell test suite

There are three categories of unit tests:
- true unit tests
- Postgres unit tests (require a postgres instance)
- MSSQL unit tests (require a MSSQL instance)

The easiest way to run these tests is through `dev.sh`:

```
./scripts/dev.sh test --unit
```

If you want to limit to a specific set of tests:

```
./scripts/dev.sh test --unit --match "some pattern" mssql
```

Note that you have to use one of 'unit', 'postgres' or 'mssql' when
using '--match'. There is no way to match without specifying the subset
of tests to run.

Alternatively, you can run unit tests directly through cabal:

```
cabal new-run -- test:graphql-engine-tests unit
HASURA_GRAPHQL_DATABASE_URL='postgres://<user>:<password>@<host>:<port>/<dbname>' \
    cabal new-run -- test:graphql-engine-tests postgres
```

##### Running the Haskell integration test suite

1. To run the Haskell integration test suite, you'll first need to bring up the database containers:

```sh
docker-compose up
```

2. Once the containers are up, you can run the test suite via

```sh
cabal test tests-hspec --test-show-details=direct
```

#### Building with profiling

To build with profiling support, you need to both enable profiling via `cabal`
and set the `profiling` flag. E.g.

```
cabal build exe:graphql-engine -f profiling --enable-profiling
```

### Create Pull Request

- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md).
- If you changed the versions of any dependencies, run `cabal new-freeze` to update the freeze file.
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
