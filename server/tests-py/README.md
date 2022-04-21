# Python Integration Test Suite

This document describes the Python integration test suite. Please consult
the `server/CONTRIBUTING` document for general information on the overall
test setup and other testing suites.

This document describes running and writing tests, as well as some
information on how to update test dependencies.

## Running tests

Tests can be run using the `dev.sh` script or directly using `pytest`.

Please note that running the `BigQuery` tests requires a few manual steps.


### Running tests via dev.sh

The easiest way to run the test suite is to do:

```sh
scripts/dev.sh test --integration
```

NOTE: this only runs the tests for Postgres. If you want to run tests
for a different backend, use:

```sh
scripts/dev.sh test --integration --backend mssql
```

Available options are documented in `scripts/parse-pytest-backend`:

- postgres (default)
- bigquery (see section below)
- citus
- mssql
- mysql

#### Filtering tests

You can filter tests by using `-k <name>`. Note that `<name>` is case-
insensitive.

```sh
scripts/dev.sh test --integration --backend mssql -k MSSQL
```

Note that you can also use expressions here, for example:

```sh
scripts/dev.sh test --integration --backend mssql -k "MSSQL and not Permission"
```

See [pytest docs](https://docs.pytest.org/en/6.2.x/usage.html#specifying-tests-selecting-tests)
for more details.

#### Failures

If you want to stop after the first test failure you can pass `-x`:

```sh
scripts/dev.sh test --integration --backend mssql -k MSSQL -x
```

#### Verbosity

You can increase or decrease the log verbosity by adding `-v` or `-q`
to the command.

### Running tests directly

WARNING: running tests manually will force skipping of some tests. `dev.sh`
deals with setting up some environment variables which decide how and if
some of the tests are executed.

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
       WEBHOOK_FROM_ENV=http://localhost:5592 \
       SCHEDULED_TRIGGERS_WEBHOOK_DOMAIN=http://127.0.0.1:5594 \
     cabal new-run -- exe:graphql-engine \
       --database-url='postgres://<user>:<password>@<host>:<port>/<dbname>' \
       serve --stringify-numeric-types
   ```

   Optionally, replace the `--database-url` parameter with `--metadata-database-url` to enable testing against multiple sources.

   The environment variables are needed for a couple of tests, and the `--stringify-numeric-types` option is used to avoid the need to do floating-point comparisons.

5. Optionally, add more sources to test against:

    If the tests include more sources (e.g., by using `-k MSSQL or MySQL`), then you can use the following commands to add sources to your running graphql instance:
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

For more details, please consult `pytest --help`.

### Running BigQuery tests

Running integration tests against a BigQuery data source is a little more involved due to the necessary service account requirements:
```
HASURA_BIGQUERY_PROJECT_ID=# the project ID of the service account
HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL=# eg. "<<SERVICE_ACCOUNT_NAME>>@<<PROJECT_NAME>>.iam.gserviceaccount.com"
HASURA_BIGQUERY_SERVICE_KEY=# the service account key
```

Before running the test suite either manually or via `dev.sh`:
1. Ensure you have access to a [Google Cloud Console service account](https://cloud.google.com/iam/docs/creating-managing-service-accounts#creating). Store the project ID and account email in `HASURA_BIGQUERY_PROJECT_ID` and (optional) `HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL` variables.
2. [Create and download a new service account key](https://cloud.google.com/iam/docs/creating-managing-service-account-keys). Store the contents of file in a `HASURA_BIGQUERY_SERVICE_KEY` variable. 
    ```bash
    export HASURA_BIGQUERY_SERVICE_KEY=$(cat /path/to/service/account)
    ```
3. [Login and activate the service account](https://cloud.google.com/sdk/gcloud/reference/auth/activate-service-account), if it is not already activated.
4. Verify the service account is accessible via the [BigQuery API](https://cloud.google.com/bigquery/docs/reference/rest):
    1. Run the following command:
    ```bash
    source scripts/verify-bigquery-creds.sh $HASURA_BIGQUERY_PROJECT_ID $HASURA_BIGQUERY_SERVICE_KEY $HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL
    ```
    If the query succeeds, the service account is setup correctly to run tests against BigQuery locally.
5. Finally, run the BigQuery test suite with `HASURA_BIGQUERY_SERVICE_KEY` and `HASURA_BIGQUERY_PROJECT_ID` environment variables set. For example:
  ```
  scripts/dev.sh test --integration --backend bigquery -k TestGraphQLQueryBasicBigquery
  ```

## Tests structure

- Tests are grouped as test classes in test modules (names starting with `test_`)

- The configuration files (if needed) for the tests in a class are usually kept in one folder.
  - The folder name is usually either the `dir` variable or the `dir()` function


-  Some tests (like in `test_graphql_queries.py`) requires a setup and teardown per class.
   - Here we are extending the `DefaultTestSelectQueries` class.
   - This class defines a fixture which will run the configurations in `setup.yaml` and `teardown.yaml` once per class
   - Extending test class should define a function name `dir()`, which returns the configuration folder


- For mutation tests (like in `test_graphql_mutations.py`)
   - We need a `schema_setup` and `schema_teardown` per class
   - And `values_setup` and `values_teardown` per test
   - Doing schema setup and teardown per test is expensive.
   - We are extending the `DefaultTestMutations` class for this.
   - This class defines a fixture which will run the configuration in `setup.yaml` and `teardown.yaml` once per class.
   - Another fixture defined in this class runs the configuration in `values_setup.yaml` and `values_teardown.yaml` once per class.

## Writing python tests

1. Check whether the test you intend to write already exists in the test suite, so that there will be no
   duplicate tests or the existing test will just need to be modified.

2. All the tests use setup and teardown, the setup step is used to initialize the graphql-engine
   and the database in a certain state after which the tests should be run. After the tests are run,
   the state needs to be cleared, which should be done in the teardown step. The setup and teardown
   is localised for every python test class.

   See `TestCreateAndDelete` in [test_events.py](test_events.py)
   for reference.

3. The setup and teardown can be configured to run before and after every test in a test class
   or run before and after running all the tests in a class. Depending on the use case, there
   are different fixtures like `per_class_tests_db_state`,`per_method_tests_db_state` defined in the [conftest.py](conftest.py) file.

4. Sometimes, it's required to run the graphql-engine with in a different configuration only
   for a particular set of tests. In this case, these tests should be run only when the graphql-engine
   is run with the said configuration and should be skipped in other graphql-engine configurations. This
   can be done by accepting a new command-line flag from the `pytest` command and depending on the value or
   presence of the flag, the tests should be run accordingly. After adding this kind of a test, a new section
   needs to be added in the [test-server.sh](../../oss-.circleci/test-server.sh). This new section's name should also
   be added in the `server-test-names.txt` file, otherwise the test will not be run in the CI.

   For example,

   The tests in the [test_remote_schema_permissions.py](test_remote_schema_permissions.py)
   are only to be run when the remote schema permissions are enabled in the graphql-engine and when
   it's not set, these tests should be skipped. Now, to run these tests we parse a command line option
   from pytest called (`--enable-remote-schema-permissions`) and the presence of this flag means that
   we need to run these tests. When the tests are run with this command line option, it's assumed that
   the server has enabled remote schema permissions.

### Adding test support for a new backend
The current workflow for supporting a new backend in integration tests is as follows:

1. Add functions to launch and cleanup a server for the new backend. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-876c076817b4e593cf797bdfa378ac3a24b6dc76c6f6408dd2f27da903bb331dR520-R523).
2. Connect to the database you've just launched. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-876c076817b4e593cf797bdfa378ac3a24b6dc76c6f6408dd2f27da903bb331dR554-R557).
3. Add setup and teardown files:
    1. `setup_<backend>`: for `v1/query` or metadata queries such as `<backend>_track_table`. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-97ba2b889f4ed620e8bd044f819b1f94f95bfc695a69804519e38a00119337d9).
    2. `schema_setup_<backend>`: for `v2/query` queries such as `<backend>_run_sql`. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-b34081ef8e1c34492fcf0cf72a8c1d64bcb66944f2ab2efb9ac0812cd7a003c7).
    3. `teardown_<backend>` and `cleardb_<backend>`
    4. **Important:** filename suffixes _**should be the same**_ as the value that’s being passed to `—backend`; that's how the files are looked up.
4. Specify a `backend` parameter for [the `per_backend_tests` fixture](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-1034b560ce9984643a4aa4edab1d612aa512f1c3c28bbc93364700620681c962R420), parameterised by backend. [Example](https://github.com/hasura/graphql-engine/commit/64d52f5fa333f337ef76ada4e0b6abd49353c457/scripts/dev.sh#diff-40b7c6ad5362e70cafd29a3ac5d0a5387bd75befad92532ea4aaba99421ba3c8R12-R13).

Note: When teardown is not disabled (via `skip_teardown`(*) , in which case, this phase is skipped entirely), `teardown.yaml` always runs before `schema_teardown.yaml`, even if the tests fail. See `setup_and_teardown` in `server/tests-py/conftest.py` for the full source code/logic.

(*): See `setup_and_teardown_v1q` and `setup_and_teardown_v2q` in `conftest.py` for more details.

This means, for example, that if `teardown.yaml` untracks a table, and `schema_teardown.yaml` runs raw SQL to drop the table, both would succeed (assuming the table is tracked/exists).

**Test suite naming convention**
The current convention is to indicate the backend(s) tests can be run against in the class name. For example:
   * `TestGraphQLQueryBasicMySQL` for tests that can only be run on MySQL
   * `TestGraphQLQueryBasicCommon` for tests that can be run against more than one backend
   * If a test class doesn't have a suffix specifying the backend, nor does its name end in `Common`, then it is likely a test written pre-v2.0 that
     can only be run on Postgres

This naming convention enables easier test filtering with [pytest command line flags](https://docs.pytest.org/en/6.2.x/usage.html#specifying-tests-selecting-tests).

The backend-specific and common test suites are disjoint; for example, run `pytest --integration -k "Common or MySQL" --backend mysql` to run all MySQL tests.

Note that `--backend` does not interact with the selection of tests. You will generally have to combine `--backend` with `-k`.

## Updating Python requirements

The packages/requirements are documented in two files:
- `server/tests-py/requirements-top-level.txt`
- `server/tests-py/requirements.txt`

The `server/tests-py/requirements-top-level.txt` file is the main file. It
contains the direct dependencies along with version requirements we know
we should be careful about.

The `server/tests-py/requirements.txt` file is the _lock_ file. It holds
version numbers for all direct and transitive dependencies. This file
can be re-generated by:
1. alter `server/tests-py/requirements-top-level.txt`
2. remove `server/tests-py/requirements.txt`
3. run `dev.sh test --integration`
4. update `DEVSH_VERSION` in `scripts/dev.sh` to force reinstall
    these dependencies

Steps 3 can be done manually:
```sh
pip3 install -r requirements-top-level.txt
pip3 freeze > requirements.txt
```
