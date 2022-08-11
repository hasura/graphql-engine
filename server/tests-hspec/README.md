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
    - [Using GHCI](#using-ghci)
  - [Style guide](#style-guide)
    - [Stick to Simple Haskell](#stick-to-simple-haskell)
    - [Write small, atomic, autonomous specs](#write-small-atomic-autonomous-specs)
    - [Use the `Harness.*` hierarchy for common functions](#use-the-harness-hierarchy-for-common-functions)
  - [Troubleshooting](#troubleshooting)
    - [`Database 'hasura' already exists. Choose a different database name.`](#database-hasura-already-exists-choose-a-different-database-name)
    - [General `DataConnector` failures](#general-dataconnector-failures)
    - [`SQLServer` failures on Apple M1 chips](#sqlserver-failures-on-apple-m1-chips)

## Required setup for BigQuery tests

Running integration tests against a BigQuery data source is a more involved due to the necessary service account requirements:

```
HASURA_BIGQUERY_PROJECT_ID=# the project ID of the service account
HASURA_BIGQUERY_SERVICE_KEY=# the service account key
# optional variable used to verify the account setup in step 4 below
HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL=# eg. "<<SERVICE_ACCOUNT_NAME>>@<<PROJECT_NAME>>.iam.gserviceaccount.com"
```

Before running the test suite:

1.  Ensure you have access to a [Google Cloud Console service account](https://cloud.google.com/iam/docs/creating-managing-service-accounts#creating). Store the project ID and account email in `HASURA_BIGQUERY_PROJECT_ID` variable.

2.  [Create and download a new service account key](https://cloud.google.com/iam/docs/creating-managing-service-account-keys). Store the contents of file in a `HASURA_BIGQUERY_SERVICE_KEY` variable.

    ```bash
    export HASURA_BIGQUERY_SERVICE_KEY=$(cat /path/to/service/account)
    ```

3.  [Login and activate the service account](https://cloud.google.com/sdk/gcloud/reference/auth/activate-service-account), if it is not already activated.

4.  Verify the service account is accessible via the [BigQuery API](https://cloud.google.com/bigquery/docs/reference/rest), by running the following command:

    ```bash
    ./scripts/verify-bigquery-creds.sh
    ```

    If the query succeeds, the service account is setup correctly to run tests against BigQuery locally.

5.  If necessary, create a dataset called "hasura" in the [BigQuery workspace](https://console.cloud.google.com/bigquery).

6.  Finally, run the BigQuery tests once the `HASURA_BIGQUERY_SERVICE_KEY` and `HASURA_BIGQUERY_PROJECT_ID` environment variables set. For example:

    ```
    cabal run tests-hspec -- -m "BigQuery"
    ```

_Note to Hasura team: a service account is already setup for internal use, please check the wiki for further details._

## Running the test suite

1. To run the Haskell integration test suite, we'll first need to start the backends:

   ```sh
   docker compose up
   ```

   This will start up Postgres, SQL Server, Citus, MariaDB and the Hasura Data Connectors' reference agent.

   > **Note**: on ARM64 architecture we'll need additional steps in order to test mssql properly.
   > See [`SQLServer` failures on Apple M1 chips](#sqlserver-failures-on-apple-m1-chips)
   > for more details.

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

3. The local databases persist even after shutting down the containers.
   If this is undesirable, delete the databases using the following command:

   ```sh
   docker compose down --volumes
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

Tests are written using [`hspec`](http://hspec.github.io/) and
[`hspec-discover`](https://hackage.haskell.org/package/hspec-discover):

- Modules are declared under the `Test` namespace.
- Module names must end with `Spec` (e.g. `HelloWorldSpec`).
- Module names must contain some value `spec :: SpecWith TestEnvironment`,
  which serves as the entry point for the module.

See the documentation for `hspec` and `hspec-discover`, as well as other
modules in the `Test` namespace, for more guidance. As well as this, the module
[Test.HelloWorldSpec](Test/HelloWorldSpec.hs) contains a skeleton for writing
new tests.

### Specifying fixtures 

We often want to run the same tests several times with slightly different
configuration. Most commonly, we want to assert that a given behaviour works
consistently across different backends.

[Harness.Test.Fixture](Harness/Test/Fixture.hs) defines two functions for
running test trees in terms of a list of `Fixture a`s.

Each `Fixture a` requires:

- a unique `name`, of type `FixtureName`
- a `mkLocalTestEnvironment` action, of type `TestEnvironment -> IO a`
- a `setupTeardown` action, of type `(TestEnvironment, a) -> [SetupAction]`
- an `customOptions` parameter, which will be threaded through the
  tests themselves to modify behavior for a particular `Fixture`

Of these two functions, whether one wishes to use `Harness.Test.Fixture.run` or
`Harness.Test.Fixture.runWithLocalTestEnvironment` will depend on if their test can be
written in terms of information provided by the global `TestEnvironment` type or if it
depends on some additional "local" state.

More often than not, test authors should use `Harness.Test.Fixture.run`, which
is written in terms of `Fixture ()`. This uses `()` for the local test which
does not carry any "useful" state information, and is therefore omitted from
the body of the tests themselves.

In the rare cases where some local state is necessary, test authors should use 
`Harness.Test.Fixture.runWithLocalTestEnvironment`. This function
takes a type parameter for its local testEnvironment, which will be provided to 
the body of tests themselves.

#### Make local testEnvironment action

This refers to the function `mkLocalTestEnvironment` defined for `Fixture`:

```hs
mkLocalTestEnvironment :: TestEnvironment -> IO a
```

Its return value, `IO a`, matches the `a` of `Fixture a`: it is the
additional local state that is required throughout the tests, in
addition to the global `TestEnvironment`. Some tests, such as tests
which check remote relationships, need to keep some state which is
local to the context, but most tests do not need additional state, and
define `mkLocalTestEnvironment` to be
`Harness.Test.Fixture.noLocalTestEnvironment`.

#### Setup / teardown actions

To ensure things are cleaned up properly in the event of errors in setting up
and tearing down tests, test setup is defined in terms of `SetupAction`s.

These look like this:

```haskell
data SetupAction = forall a.
  SetupAction
    { setupAction :: IO a,
      teardownAction :: Maybe a -> IO ()
    }
```

A `SetupAction` encodes how to setup and tear down a single piece of test
system state.

The value produced by a `setupAction` is to be input into the corresponding
`teardownAction`, if the `setupAction` completed without throwing an exception.

Therefore one `SetupAction` could create the DB tables, and the matching
teardown removes them. Pairing setup / teardown in this way makes it easier to
remove everything in the right order.

##### Setup

The setup actions are responsible for creating the environment for the test. They need to:

1. Clear and reconfigure the metadata
2. Setup tables and insert values
3. Track tables, add relationships, permissions

etc.

These actions can be created by running POST requests against graphql-engine
using `Harness.GraphqlEngine.post_`, or by running SQL requests against the
backend using `Backend.<backend>.run_`.

##### Teardown 

These actions are responsible for freeing acquired resources, and reverting all
local modifications: dropping newly created tables, deleting custom functions,
removing the changes made to the metadata, and so on.

These actions can be created by running POST requests against graphql-engine
using `Harness.GraphqlEngine.post_`, or by running SQL requests against the
backend using `Backend.<backend>.run_`.

### Writing tests

Test should be written (or reachable from) `tests :: SpecWith TestEnvironment`, or `tests :: SpecWith (TestEnvironment, Foo)` for tests that use an additional local state.

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

**Note**: these quasi-quoter can also perform string interpolation. See the relevant modules
under the [Harness.Quoter](Harness/Quoter) namespace.

## Debugging

There are times when you would want to debug a test failure by playing
around with the Hasura's Graphql engine or by inspecting the
database. The default behavior of the test suite is to drop all the
data and the tables onces the test suite finishes. To prevent that,
you can modify your test module to prevent teardown. Example:

```diff
spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    [ Fixture.fixture (Fixture.Backend Fixture.SQLServer)
        { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
          setupTeardown = \testEnv ->
            [ Fixture.SetupAction 
               { Fixture.setupAction = SqlServer.setup schema testEnv,
-                Fixture.teardownAction = \_ -> SqlServer.teardown schema testEnv
+                Fixture.teardownAction = \_ -> pure () 
               }
            ]
        }]
```

Now re-run the particular test case again so that the local database
is setup. You will still have access to that data once the test suite
finishes running. Now based on what you want to, you can either run
the Hasura's Graphql engine to debug this further or directly inspect
the database using [any of it's clients](https://en.wikipedia.org/wiki/Comparison_of_database_administration_tools).

### Using GHCI

Alternatively it is also possible to manually start up the test environment in the GHCI repl.

An example session:

```
$ cabal repl graphql-engine:tests-hspec
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[ 1 of 59] Compiling Harness.Constants ( tests-hspec/Harness/Constants.hs, interpreted )
...
[59 of 59] Compiling Main             ( tests-hspec/Spec.hs, interpreted )
Ok, 59 modules loaded.
*Main> :module *Main *SpecHook *Test.SomeSpecImDeveloping
*Main *SpecHook *Test.SomeSpecImDeveloping> te <- SpecHook.setupTestEnvironment
*Main *SpecHook *Test.SomeSpecImDeveloping> te
<TestEnvironment: http://127.0.0.1:35975 >
*Main *SpecHook *Test.SomeSpecImDeveloping> -- Setup the instance according to the Fixture
*Main *SpecHook *Test.SomeSpecImDeveloping> cleanupPG <- Fixture.fixtureRepl Test.SomeSpecImDeveloping.postgresFixture te
*Main *SpecHook *Test.SomeSpecImDeveloping>
*Main *SpecHook *Test.SomeSpecImDeveloping> -- run tests or parts of tests manually here
*Main *SpecHook *Test.SomeSpecImDeveloping> Test.SomeSpecImDeveloping.someExample te
*Main *SpecHook *Test.SomeSpecImDeveloping>
*Main *SpecHook *Test.SomeSpecImDeveloping> -- run the test with the hspec runner
*Main *SpecHook *Test.SomeSpecImDeveloping> hspec (aroundAllWith (\a () ->a te) Test.SomeSpecImDeveloping>.spec)
Postgres
  ... [✔]
Citus
  ... [✔]
*Main *SpecHook *Test.SomeSpecImDeveloping>
*Main *SpecHook *Test.SomeSpecImDeveloping> -- Or perform other manual inspections, e.g. via the console or ghci.
*Main *SpecHook *Test.SomeSpecImDeveloping>
*Main *SpecHook *Test.SomeSpecImDeveloping> -- Cleanup before reloading
*Main *SpecHook *Test.SomeSpecImDeveloping> cleanupPG
*Main *SpecHook *Test.SomeSpecImDeveloping> SpecHook.teardownTestEnvironment te

*Main *SpecHook *Test.SomeSpecImDeveloping> -- Reload changes made to the test module or HGE.
*Main *SpecHook *Test.SomeSpecImDeveloping> :reload
```

Points to note:

- `SpecHook.setupTestEnvironment` starts the HGE server, and its url is revealed by `instance Show TestEnvironment`.
- `SpecHook.teardownTestEnvironment` stops it again.
  - This is a good idea to do before issuing the `:reload` command, because
    reloading loses the `te` reference but leaves the thread running!
- `Fixture.fixtureRepl` runs the setup action of a given `Fixture` and returns a
  corresponding teardown action.
  - After running this you can interact with the HGE console in the same state
    as when the tests are run.
  - Or you can run individual test `Example`s or `Spec`s.
- To successfully debug/develop a test in the GHCI repl, the test module should:
  - define its `Fixture`s as toplevel values,
  - define its `Example`s as toplevel values,
  - ... such that they can be used directly in the repl.

## Style guide

### Stick to [Simple Haskell](https://www.simplehaskell.org/)

This test suite should remain accessible to contributors who are new to Haskell and/or the GraphQL engine codebase. Consider the [power-to-weight](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto/#power-to-weight-ratio) ratio of features, language extensions or abstractions before you introduce them. For example, try to fully leverage Haskell '98 or 2010 features before making use of more advanced ones.

### Write small, atomic, autonomous specs

Small: Keep specs short and succinct wherever possible. Consider reorganising modules that grow much longer than ~200-300 lines of code.

_For example: The [`TestGraphQLQueryBasic` pytest class](../tests-py/test_graphql_queries.py#L251) was ported to the hspec suite as separate `BasicFields`, `LimitOffset`, `Where`, `Ordering`, `Directives`and `Views` specs._

Atomic: Each spec should test only one feature against the backends (or contexts) that support it. Each module should contain only the context setup and teardown, and the tests themselves. The database schema, test data, and feature expectations should be easy to reason about without navigating to different module.

_For example: [`BasicFieldsSpec.hs`](Test/BasicFieldsSpec.hs)_

Autonomous: Each test should run independently of other tests, and not be dependent on the results of a previous test. Shared test state, where unavoidable, should be made explicit.

_For example: [Remote relationship tests](Test/RemoteRelationship/) explicitly require shared state._

### Use the `Harness.*` hierarchy for common functions

Avoid functions or types in tests, other than calls to the `Harness.*` API.

Any supporting code should be in the `Harness.*` hierarchy and apply broadly to the test suites overall.

## Troubleshooting

### `Database 'hasura' already exists. Choose a different database name.` or `schema "hasura" does not exist`

This typically indicates persistent DB state between test runs. Try `docker compose down --volumes` to delete the DBs and restart the containers.

### General `DataConnector` failures

The DataConnector agent might be out of date. If you are getting a lot of test failures, first try rebuilding the containers with `docker compose build` to make sure you are using the latest version of the agent.

### `SQLServer` failures on Apple M1 chips

We have a few problems with SQLServer on M1:

1. Compiler bug in GHC 8.10.7 on M1.

   Due to compiler bugs in GHC 8.10.7 we need to use patched Haskell ODBC bindings as a workaround for M1 systems.
   Make the following change in the `cabal.project`:

   ```diff
     source-repository-package
       type: git
   -   location: https://github.com/fpco/odbc.git
   -   tag: 3d80ffdd4a2879f0debecabb56d834d2d898212b
   +   location: https://github.com/soupi/odbc.git
   +   tag: 46ada57c0d8f750280d6c554536c0fbcff02be59
   ```

2. Microsoft did not release SQL Server for M1. We need to use Azure SQL Edge instead.

   Switch the docker image in `docker-compose/sqlserver/Dockerfile` to `azure-sql-edge`:

   ```diff
   - FROM mcr.microsoft.com/mssql/server:2019-latest@sha256:a098c9ff6fbb8e1c9608ad7511fa42dba8d22e0d50b48302761717840ccc26af
   + FROM mcr.microsoft.com/azure-sql-edge
   ```

   Note: you might need to rebuild the Docker images with `docker compose build`

3. Azure SQL Edge does not ship with the `sqlcmd` utility with which we use to setup the SQL Server schema.

   1. Install it locally instead, with brew: `brew install microsoft/mssql-release/mssql-tools`.
   2. To start the test suite's backends, we need to setup the SQL Server schema using our local `sqlcmd`.
      To start the backends, run this command instead:

      ```diff
      - docker compose up
      + docker compose up & (cd docker-compose/sqlserver/ && ./run-init.sh 65003) && fg
      ```
