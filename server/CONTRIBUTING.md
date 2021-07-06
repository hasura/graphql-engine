# Contributing

This guide explains how to set up the graphql-engine server for development on your
own machine and how to contribute.

## Pre-requisites

- [GHC](https://www.haskell.org/ghc/) 8.10.2 and [cabal-install](https://cabal.readthedocs.io/en/latest/)
  - There are various ways these can be installed, but [ghcup](https://www.haskell.org/ghcup/) is a good choice if you’re not sure.
- There are few system packages required like `libpq-dev`, `libssl-dev`, etc. The best place to get the entire list is from the packager [Dockerfile](https://github.com/hasura/graphql-engine/blob/master/.circleci/server-builder.dockerfile)

For building console and running test suite:

- [Node.js](https://nodejs.org/en/) (>= v8.9)
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

   Optionally, replace the `--database-url` parameter with `--metadata-database-url` to enable testing against multiple sources.

   The environment variables are needed for a couple tests, and the `--stringify-numeric-types` option is used to avoid the need to do floating-point comparisons.

5. Optionally, add alternative sources to test against:

    If you enabled testing against multiple sources with in the last step, you can add those sources as follows:
    ```
    # Add a Postgres source
    curl "$METADATA_URL" \
    --data-raw '{"type":"pg_add_source","args":{"name":"default","configuration":{"connection_info":{"database_url":"'"$POSTGRES_DB_URL"'","pool_settings":{}}}}}'

    # Add a SQL Server source
    curl "$METADATA_URL" \
    --data-raw '{"type":"mssql_add_source","args":{"name":"mssql","configuration":{"connection_info":{"connection_string":"'"$MSSQL_DB_URL"'","pool_settings":{}}}}}'

    # Optionally verify sources have been added
    curl "$METADATA_URL" --data-raw '{"type":"export_metadata","args":{}}'
    ```

6. With the server running, run the test suite:

   ```
   cd tests-py
   pytest --hge-urls http://localhost:8080 \
          --pg-urls 'postgres://<user>:<password>@<host>:<port>/<dbname>'
   ```

This will run all the tests, which can take a couple minutes (especially since some of the tests are slow). You can configure `pytest` to run only a subset of the tests; see [the `pytest` documentation](https://doc.pytest.org/en/latest/usage.html) for more details.

Some other useful points of note:

  - It is recommended to use a separate Postgres database for testing, since the tests will drop and recreate the `hdb_catalog` schema, and they may fail if certain tables already exist. (It’s also useful to be able to just drop and recreate the entire test database if it somehow gets into a bad state.)

  - You can pass the `-v` or `-vv` options to `pytest` to enable more verbose output while running the tests and in test failures. You can also pass the `-l` option to display the current values of Python local variables in test failures.

  - Tests can be run against a specific backend (defaulting to Postgres) with the `backend` flag, for example:
    ```
      pytest --hge-urls http://localhost:8080 \
             --pg-urls 'postgres://<user>:<password>@<host>:<port>/<dbname>'
             --backend mssql -k TestGraphQLQueryBasicCommon
    ```

##### Running the Python test suite on BigQuery
Running integration tests against a BigQuery data source is a little more involved due to the necessary service account requirements. Before running the test suite either [manually](https://github.com/hasura/graphql-engine/blob/master/server/CONTRIBUTING.md#run-and-test-manually) or [via `dev.sh`](https://github.com/hasura/graphql-engine/blob/master/server/CONTRIBUTING.md#run-and-test-via-devsh):
1. Ensure you have access to a [Google Cloud Console service account](https://cloud.google.com/iam/docs/creating-managing-service-accounts#creating).
2. [Create and download a new service account key](https://cloud.google.com/iam/docs/creating-managing-service-account-keys).
3. [Activate the service account](https://cloud.google.com/sdk/gcloud/reference/auth/activate-service-account), if it is not already activated.
4. Verify the service account is accessible via the [BigQuery API](https://cloud.google.com/bigquery/docs/reference/rest):
     1. Update the environment variables in `scripts/verify-bigquery-creds.sh` with the credentials for your service account.
     2. Run `source scripts/verify-bigquery-creds.sh`.
     3. If the query succeeds, the service account is setup correctly to run tests against BigQuery locally.
5. Create a new `hasura` data source, and run the contents of [this `schema_setup_bigquery.sql` file](https://github.com/hasura/graphql-engine/blob/master/server/tests-py/queries/graphql_query/bigquery/schema_setup_bigquery.sql) against it.
6. Finally, run the BigQuery test suite with `HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE` and `HASURA_BIGQUERY_PROJECT_ID` environment variables set. For example:
  ```
  export HASURA_BIGQUERY_PROJECT_ID=# the project ID of the service account from step 1
  export HASURA_BIGQUERY_SERVICE_ACCOUNT_FILE=# the filepath to the downloaded service account key from step 2
  scripts/dev.sh test --integration --backend bigquery -k TestGraphQLQueryBasicBigquery
  ```

##### Guide on writing python tests

1. Check whether the test you intend to write already exists in the test suite, so that there will be no
   duplicate tests or the existing test will just need to be modified.

2. All the tests use setup and teardown, the setup step is used to initialize the graphql-engine
   and the database in a certain state after which the tests should be run. After the tests are run,
   the state needs to be cleared, which should be done in the teardown step. The setup and teardown
   is localised for every python test class.

   See `TestCreateAndDelete` in [test_events.py](tests-py/test_events.py)
   for reference.

3. The setup and teardown can be configured to run before and after every test in a test class
   or run before and after running all the tests in a class. Depending on the use case, there
   are different fixtures like `per_class_tests_db_state`,`per_method_tests_db_state` defined in the [conftest.py](tests-py/conftest.py) file.

4. Sometimes, it's required to run the graphql-engine with in a different configuration only
   for a particular set of tests. In this case, these tests should be run only when the graphql-engine
   is run with the said configuration and should be skipped in other graphql-engine configurations. This
   can be done by accepting a new command-line flag from the `pytest` command and depending on the value or
   presence of the flag, the tests should be run accordingly. After adding this kind of a test, a new section
   needs to be added in the [test-server.sh](../.circleci/test-server.sh). This new section's name should also
   be added in the `server-test-names.txt` file, otherwise the test will not be run in the CI.

   For example,

   The tests in the [test_remote_schema_permissions.py](tests-py/test_remote_schema_permissions.py)
   are only to be run when the remote schema permissions are enabled in the graphql-engine and when
   it's not set, these tests should be skipped. Now, to run these tests we parse a command line option
   from pytest called (`--enable-remote-schema-permissions`) and the presence of this flag means that
   we need to run these tests. When the tests are run with this command line option, it's assumed that
   the server has enabled remote schema permissions.

##### Adding test support for a new backend
The current workflow for supporting a new backend in integration tests is as follows:

1. Add functions to launch and cleanup a server for the new backend. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-876c076817b4e593cf797bdfa378ac3a24b6dc76c6f6408dd2f27da903bb331dR520-R523).
2. Connect to the database you've just launched. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-876c076817b4e593cf797bdfa378ac3a24b6dc76c6f6408dd2f27da903bb331dR554-R557).
3. Add setup and teardown files:
    1. `setup_<backend>`: for `v1/query` or metadata queries such as `<backend>_track_table`. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-97ba2b889f4ed620e8bd044f819b1f94f95bfc695a69804519e38a00119337d9).
    2. `schema_setup_<backend>`: for `v2/query` queries such as `<backend>_run_sql`. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-b34081ef8e1c34492fcf0cf72a8c1d64bcb66944f2ab2efb9ac0812cd7a003c7).
    3. `teardown_<backend>` and `cleardb_<backend>`
    4. important: filename suffixes should be the same as the value that’s being passed to `—backend`; that's how the files are looked up.
4. Specify a `backend` parameter for [the `per_backend_tests` fixture](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-1034b560ce9984643a4aa4edab1d612aa512f1c3c28bbc93364700620681c962R420), parameterised by backend. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-40b7c6ad5362e70cafd29a3ac5d0a5387bd75befad92532ea4aaba99421ba3c8R12-R13).
5. Optional: Run the existing (Postgres) test suite against the new backend to identify and group common and backend-specific tests into their own classes.

Tests against alternative backends aren't yet run/supported in CI, so please test locally.

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
