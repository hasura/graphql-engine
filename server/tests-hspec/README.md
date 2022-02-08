# tests-hspec

Graphql-engine integration tests written in Haskell using the [hspec](https://hspec.github.io) testing framework.

For motivation, rationale, and more, see the [test suite rfc](../../rfcs/hspec-test-suite.md).

**Table of Contents**

- [Running the test suite](#running-the-test-suite)
- [Test suite structure](#test-suite-structure)
    - [Harness](#harness)
    - [Test](#test)
- [Adding a new test](#adding-a-new-test)
    - [Specifying backends](#specifying-backends)
        - [Setup action](#setup-action)
        - [Teardown action](#teardown-action)
    - [Writing tests](#writing-tests)
    - [Style guide](#style-guide)

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

3. The local databases presist even after shutting down docker-compose.
   If this is undesirable, delete the databases using the following command:

   ```sh
   docker-compose down --volumes
   ```

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
2. Specify the relevant backends on which the tests will run in `spec`
3. Specify the tests and expectations in `tests`

When creating a new test, make sure that:

1. The module name is suffixed by the word `Spec`
2. The module exports the entry point `spec :: SpecWith State`
3. The module is listed in the cabal file under `other-modules`

(1) and (2) are required for `hspec-discover` to find and run the test.

### Specifying backends

To specify a new backend, create a value of the type `Backend` which is defined in
[Harness.Test.Feature](Harness/Test/Feature.hs). A `Backend` requires a `name`,
a `setup action` and a `teardown action`.

#### Setup action

A setup action is a function of type `State -> IO ()` which is responsible with
creating the environment for the test. It needs to:

1. Clear and reconfigure the metadata
2. Setup tables and insert values
3. Track tables, add relationships, permissions

etc.

These actions can be created by running POST requests against graphql-engine
using `Harness.GraphqlEngine.post_`, or by running SQL requests against the
backend using `Backend.<backend>.run_`.

#### Teardown action

The teardown action is another function of type `State -> IO ()` which is responsible
for removing the environment created by the test or setup, so that other tests can have
a "clean slate" with no artifacts.

For example, this test should drop tables, delete custom functions and sequences, and so on.

These actions can be created by running POST requests against graphql-engine
using `Harness.GraphqlEngine.post_`, or by running SQL requests against the
backend using `Backend.<backend>.run_`.

### Writing tests

Test should be written (or reachable from) `tests :: SpecWith State`.

A typical test will look similar to this:

```hs
  it "Where id=1" \state ->
    shouldReturnYaml
      ( GraphqlEngine.postGraphql
          state
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
under the [Harness.Quoter](Harness/Quouter) namespace.

### Style guide

- Test suite should be very easy to read for a junior or intermediate Haskell developer.
  Be mindful of advanced feature use and abstractions!
- Prefer self-contained specs, even if there's some query duplication.
- Avoid functions or types in tests, other than calls to the `Harness.*` API.
  Any supporting code should be in the `Harness.*` hierarchy and apply broadly to the test suites
  overall.
- Consider reorganising tests if modules get much longer than 1/2 pagescrolls (~200-300 LOC?).
