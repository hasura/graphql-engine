# Contributing

This guide explains how to set up the graphql-engine server for development on your
own machine and how to contribute.

## Pre-requisites

- [GHC](https://www.haskell.org/ghc/) 8.10.2 and [cabal-install](https://cabal.readthedocs.io/en/latest/)
  - There are various ways these can be installed, but [ghcup](https://www.haskell.org/ghcup/) is a good choice if you’re not sure.
- [Node.js](https://nodejs.org/en/) (>= v8.9)
- npm >= 5.7
- [gsutil](https://cloud.google.com/storage/docs/gsutil)
- libpq-dev
- libkrb5-dev
- openssl and libssl-dev
- python >= 3.5 with pip3 and virtualenv

The last few prerequisites can be installed on Debian or Ubuntu with:

    $ sudo apt install libpq-dev libkrb5-dev python3 python3-pip python3-venv openssl libssl-dev

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

    $ cd server
    $ ln -s cabal.project.dev cabal.project.local
    $ cabal new-update
    $ cabal new-build

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

### Run and test manually

If you want, you can also run the server and test suite manually against a Postgres instance of your choosing.

#### Run

The following command can be used to build and launch a local `graphql-engine` instance:

```
cabal new-run -- exe:graphql-engine \
  --database-url='postgres://<user>:<password>@<host>:<port>/<dbname>' \
  serve --enable-console --console-assets-dir=../console/static/dist
```

This will launch a server on port 8080, and it will serve the console assets if they were built with `npm run server-build` as mentioned above.

#### Test

`graphql-engine` has two test suites:

  1. A small set of unit tests and integration tests written in Haskell.

  2. An extensive set of end-to-end tests written in Python.

Both sets of tests require a running Postgres database.

##### Running the Haskell test suite

```
cabal new-run -- test:graphql-engine-tests \
  --database-url='postgres://<user>:<password>@<host>:<port>/<dbname>'
```

##### Running the Python test suite

1. To run the Python tests, you’ll need to install the necessary Python dependencies first. It is recommended that you do this in a self-contained Python venv, which is supported by Python 3.3+ out of the box. To create one, run:

   ```
   python3 -m venv .python-venv
   ```

   (The second argument names a directory where the venv sandbox will be created; it can be anything you like, but `.python-venv` is `.gitignore`d.)

   With the venv created, you can enter into it in your current shell session by running:

   ```
   source .python-venv/bin/activate
   ```

   (Source `.python-venv/bin/activate.fish` instead if you are using `fish` as your shell.)

2. Install the necessary Python dependencies into the sandbox:

   ```
   pip3 install -r tests-py/requirements.txt
   ```

3. Install the dependencies for the Node server used by the remote schema tests:

   ```
   (cd tests-py/remote_schemas/nodejs && npm ci)
   ```

4. Start an instance of `graphql-engine` for the test suite to use:

   ```
   env EVENT_WEBHOOK_HEADER=MyEnvValue \
       WEBHOOK_FROM_ENV=http://localhost:5592/ \
       SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN=http://127.0.0.1:5594 \
     cabal new-run -- exe:graphql-engine \
       --database-url='postgres://<user>:<password>@<host>:<port>/<dbname>' \
       serve --stringify-numeric-types
   ```

   The environment variables are needed for a couple tests, and the `--stringify-numeric-types` option is used to avoid the need to do floating-point comparisons.

5. With the server running, run the test suite:

   ```
   cd tests-py
   pytest --hge-urls http://localhost:8080 \
          --pg-urls 'postgres://<user>:<password>@<host>:<port>/<dbname>'
   ```

This will run all the tests, which can take a couple minutes (especially since some of the tests are slow). You can configure `pytest` to run only a subset of the tests; see [the `pytest` documentation](https://doc.pytest.org/en/latest/usage.html) for more details.

Some other useful points of note:

  - It is recommended to use a separate Postgres database for testing, since the tests will drop and recreate the `hdb_catalog` schema, and they may fail if certain tables already exist. (It’s also useful to be able to just drop and recreate the entire test database if it somehow gets into a bad state.)

  - You can pass the `-v` or `-vv` options to `pytest` to enable more verbose output while running the tests and in test failures. You can also pass the `-l` option to display the current values of Python local variables in test failures.

### Create Pull Request

- Make sure your commit messages meet the [guidelines](../CONTRIBUTING.md).
- If you changed the versions of any dependencies, run `cabal new-freeze` to update the freeze file.
- Create a pull request from your forked repo to the main repo.
- Every pull request will automatically build and run the tests.

## Code conventions

This helps enforce a uniform style for all committers.

- Compiler warnings are turned on, make sure your code has no warnings.
- Use [hlint](https://github.com/ndmitchell/hlint) to make sure your code has no warnings.
  You can use our custom hlint config with `$ hlint --hint=server/.hlint.yaml .`
- Use [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to format your code.
