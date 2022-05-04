# tests-hspec

Graphql-engine integration tests written in Haskell using the [hspec](https://hspec.github.io) testing framework.

For motivation, rationale, and more, see the [test suite rfc](../../rfcs/hspec-test-suite.md).

**Table of Contents**

- [tests-hspec](#tests-hspec)
  - [Required setup for BigQuery tests](#required-setup-for-bigquery-tests)
  - [Running the test suite](#running-the-test-suite)
  - [Enabling logging](#enabling-logging)
  - [Test suite structure](#test-suite-structure)
    - [Harness](#harness)
    - [Test](#test)
  - [Adding a new test](#adding-a-new-test)
    - [Specifying contexts](#specifying-contexts)
      - [Make local testEnvironment action](#make-local-testenvironment-action)
      - [Setup action](#setup-action)
      - [Teardown action](#teardown-action)
    - [Writing tests](#writing-tests)
  - [Debugging](#debugging)
  - [Style guide](#style-guide)
    - [Stick to Simple Haskell](#stick-to-simple-haskell)
    - [Write small, atomic, autonomous specs](#write-small-atomic-autonomous-specs)
    - [Use the `Harness.*` hierarchy for common functions](#use-the-harness-hierarchy-for-common-functions)
  - [Troubleshooting](#troubleshooting)
    - [`Database 'hasura' already exists. Choose a different database name.`](#database-hasura-already-exists-choose-a-different-database-name)

## Required setup for BigQuery tests

Running integration tests against a BigQuery data source is a more involved due to the necessary service account requirements:
```
HASURA_BIGQUERY_PROJECT_ID=# the project ID of the service account
HASURA_BIGQUERY_SERVICE_KEY=# the service account key
# optional variable used to verify the account setup in step 4 below
HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL=# eg. "<<SERVICE_ACCOUNT_NAME>>@<<PROJECT_NAME>>.iam.gserviceaccount.com"
```

Before running the test suite:
1. Ensure you have access to a [Google Cloud Console service account](https://cloud.google.com/iam/docs/creating-managing-service-accounts#creating). Store the project ID and account email in `HASURA_BIGQUERY_PROJECT_ID` variable.
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
5. Finally, run the BigQuery tests once the `HASURA_BIGQUERY_SERVICE_KEY` and `HASURA_BIGQUERY_PROJECT_ID` environment variables set. For example:
  ```
  cabal run tests-hspec -- -m "BigQuery"
  ```

*Note to Hasura team: a service account is already setup for internal use, please check the wiki for further details.*

## Running the test suite

1. To run the Haskell integration test suite, we'll first need to start the backends:

   ```sh
   docker-compose up
   ```

   This will start up Postgres, SQL Server, Citus and MariaDB.


    > __Note__: on ARM64 architecture we'll need additional steps in order to test mssql properly.
    >
    > ### Preparation
    >
    > 1. Switch the docker image in `docker-compose/sqlserver/Dockerfile` to `azure-sql-edge`:
    >
    > ```diff
    > - FROM mcr.microsoft.com/mssql/server:2019-latest@sha256:a098c9ff6fbb8e1c9608ad7511fa42dba8d22e0d50b48302761717840ccc26af
    > + FROM mcr.microsoft.com/azure-sql-edge
    > ```
    >
    > 2. Install `sqlcmd` locally. On MacOS, this can be done with brew: `brew install mssql-tools`.
    >
    > ### Start the backends
    >
    > 1. Run `docker-compose up`
    > 2. Initialize the SQL Server database
    >
    >    ```sh
    >    (cd docker-compose/sqlserver && bash run-init.sh 65003)
    >    ```

2. Once the containers are up, you can run the test suite via

   ```sh
   cabal run tests-hspec
   ```

    You can also further refine which tests to run using the `-m` flag:

   ```sh
   cabal run tests-hspec -- -m "SQLServer"
   ```

    For additional information, consult the help section:

   ```sh
   cabal run tests-hspec -- --help
   ```

3. The local databases persist even after shutting down docker-compose.
   If this is undesirable, delete the databases using the following command:

   ```sh
   docker-compose down --volumes
   ```

## Enabling logging

In order to enable logging, you have to manually edit the `engineLogLevel` term
in [Harness/Constants.hs](Harness/Constants.hs).

This pairs well with running a single test via the `-m` flag (see the section
above).

## Test suite structure

### Harness

Modules under the [Harness/](Harness/) namespace provide the infrastructure
and supporting code for writing and running the tests.
It includes quasiquoters, interacting with backends, interfacing with HTTP,
constants, and so on.

Supporting code should be added under the `Harness.*` namespace instead of
added ad-hoc in test specs, to improve readability and reuse.

### Test

Modules under the [Test/](Test/) namespace define integration test specifications for various
features and backends.

## Adding a new test

The module [Test.HelloWorldSpec](Test/HelloWorldSpec.hs) contains a starting point
which can be built upon.

To create a new test:

1. Create a new module based on `Test.HelloWorldSpec`
2. Specify each relevant context on which the tests will run in `spec`
3. Specify the tests and expectations in `tests`

When creating a new test, make sure that:

1. The module name is suffixed by the word `Spec`
2. The module exports the entry point `spec :: SpecWith TestEnvironment`
3. The module is listed in the cabal file under `other-modules`

(1) and (2) are required for `hspec-discover` to find and run the test.

### Specifying contexts

We often want to run the same tests several times with slightly different
configuration. Most commonly, we want to assert that a given behaviour works
consistently across different backends.

[Harness.Test.Context](Harness/Test/Context.hs) defines two functions for
running test trees in terms of a list of `Context a`s.

Each `Context a` requires:
- a unique `name`, of type `ContextName`
- a `mkLocalTestEnvironment` action, of type `TestEnvironment -> IO a`
- a `setup` action, of type `(TestEnvironment, a) -> IO ()`
- a `teardown` action, of type `(TestEnvironment, a) -> IO ()`
- an `customOptions` parameter, which will be threaded through the
tests themselves to modify behavior for a particular `Context`

Of these two functions, whether one wishes to use `Harness.Test.Context.run` or
`Harness.Test.Context.runWithLocalTestEnvironment` will depend on if their test can be
written in terms of information provided by the global `TestEnvironment` type or if it
depends on some additional "local" state.

More often than not, test authors should use `Harness.Test.Context.run`, which
is written in terms of `Context ()`. This uses `()` for the local test which
does not carry any "useful" state information, and is therefore omitted from
the body of the tests themselves.

In the rare cases where some local state is necessary (either for the test
itself, or as an argument to the `teardown` action for some `Context`), test
authors should use `Harness.Test.Context.runWithLocalTestEnvironment`. This function
takes a type parameter for its local testEnvironment, which will be provided to both
the `teardown` action specified in `Context` as well as the body of tests
themselves.

#### Make local testEnvironment action

This refers to the function `mkLocalTestEnvironment` defined for `Context`:

```hs
mkLocalTestEnvironment :: TestEnvironment -> IO a
```

Its return value, `IO a`, matches the `a` of `Context a`: it is the
additional local state that is required throughout the tests, in
addition to the global `TestEnvironment`. Some tests, such as tests
which check remote relationships, need to keep some state which is
local to the context, but most tests do not need additional state, and
define `mkLocalTestEnvironment` to be
`Harness.Test.Context.noLocalTestEnvironment`.

This local state will be pass to the `setup` function and the `teardown` function.
The `teardown` function is responsible to destroy the local state as well, if needed.

#### Setup action

A setup action is a function of type `(TestEnvironment, a) -> IO ()` which is responsible for
creating the environment for the test. It needs to:

1. Clear and reconfigure the metadata
2. Setup tables and insert values
3. Track tables, add relationships, permissions

etc.

These actions can be created by running POST requests against graphql-engine
using `Harness.GraphqlEngine.post_`, or by running SQL requests against the
backend using `Backend.<backend>.run_`.

#### Teardown action

The teardown action is another function of type `(TestEnvironment, a) -> IO ()` which is
responsible for removing the environment created by the test or setup, so that
other tests can have a "clean slate" with no artifacts. The `(TestEnvironment, a)`
parameter is constructed from the `a` parameter of the `Context a`: it is the
local state that is passed throughout the tests.

This action is responsible for freeing acquired resources, and reverting all
local modifications: dropping newly created tables, deleting custom functions,
removing the changes made to the metadata, and so on.

These actions can be created by running POST requests against graphql-engine
using `Harness.GraphqlEngine.post_`, or by running SQL requests against the
backend using `Backend.<backend>.run_`.

### Writing tests

Test should be written (or reachable from) `tests :: SpecWith TestEnvironment`, or `tests
:: SpecWith (TestEnvironment, Foo)` for tests that use an additional local state.

A typical test will look similar to this:

```hs
  it "Where id=1" \testEnvironment ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
query {
  hasura_author(where: {id: {_eq: 1}}) {
    name
    id
  }
}
|]
      )
      [yaml|
data:
  hasura_author:
  - name: Author 1
    id: 1
|]
```

- `it` specifies the name of the test
- `shouldReturnYaml` creates an [`Expectation`](https://hspec.github.io/expectations.html)
  which does the following:
  - Runs a POST request against graphql-engine which can be specified using the `graphql` quasi-quoter.
  - Compares the response to an expected result which can be specified using the `yaml` quasi-quoter.

__Note__: these quasi-quoter can also perform string interpolation. See the relevant modules
under the [Harness.Quoter](Harness/Quoter) namespace.

## Debugging

There are times when you would want to debug a test failure by playing
around with the Hasura's Graphql engine or by inspecting the
database. The default behavior of the test suite is to drop all the
data and the tables onces the test suite finishes. To prevent that,
you can modify your test module to prevent teardown. Example:

``` diff
spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.MySQL,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = Mysql.setup schema,
-         teardown = Mysql.teardown schema,
+         teardown = const $ pure (),
          customOptions = Nothing
        }]
```

Now re-run the particular test case again so that the local database
is setup. You will still have access to that data once the test suite
finishes running. Now based on what you want to, you can either run
the Hasura's Graphql engine to debug this further or directly inspect
the database using [any of it's clients](https://en.wikipedia.org/wiki/Comparison_of_database_administration_tools).

## Style guide

### Stick to [Simple Haskell](https://www.simplehaskell.org/)
This test suite should remain accessible to contributors who are new to Haskell and/or the GraphQL engine codebase. Consider the [power-to-weight](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto/#power-to-weight-ratio) ratio of features, language extensions or abstractions before you introduce them. For example, try to fully leverage Haskell '98 or 2010 features before making use of more advanced ones.

### Write small, atomic, autonomous specs
Small: Keep specs short and succinct wherever possible. Consider reorganising modules that grow much longer than ~200-300 lines of code.

*For example: The [`TestGraphQLQueryBasic*` pytest class](../tests-py/test_graphql_queries.py#L251) was ported to the hspec suite as separate `BasicFields`, `LimitOffset`, `Where`, `Ordering`, `Directives` and `Views` specs.*

Atomic: Each spec should test only one feature against the backends (or contexts) that support it. Each module should contain only the context setup and teardown, and the tests themselves. The database schema, test data, and feature expectations should be easy to reason about without navigating to different module.

*For example: [`BasicFieldsSpec.hs`](Test/BasicFieldsSpec.hs)*

Autonomous: Each test should run independently of other tests, and not be dependent on the results of a previous test. Shared test state, where unavoidable, should be made explicit.

*For example: [Remote relationship tests](Test/RemoteRelationship/) explicitly require shared state.*

### Use the `Harness.*` hierarchy for common functions
Avoid functions or types in tests, other than calls to the `Harness.*` API.

Any supporting code should be in the `Harness.*` hierarchy and apply broadly to the test suites overall.

## Troubleshooting

### `Database 'hasura' already exists. Choose a different database name.`
This typically indicates persistent DB state between test runs. Try `docker-compose down --volumes` to delete the DBs and restart the containers.
