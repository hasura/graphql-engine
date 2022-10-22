# Golden API Tests

A set of [hspec](https://hspec.github.io) tests against `graphql-engine` that
send a request, and check the actual response against some expected model.
For motivation, rationale, and more, see the [test suite
RFC](../../../rfcs/hspec-test-suite.md).

## Required setup

Most of the required setup concerns (and is documented in the README for)
[../test-harness/README.md](the test harness), so please follow that link for
more information. In short, assuming you have a BigQuery account set up (again,
see [../test-harness/README.md](the test harness README) for instructions), set
the following environment variables:

```bash
$ export HASURA_BIGQUERY_PROJECT_ID=??? # The project ID
$ export HASURA_BIGQUERY_SERVICE_KEY=??? # The service account key
```

After that, BigQuery will be ready to test. For everything else, run
`docker-compose up` in the root of `graphql-engine`.

_Note to Hasura team: a service account is already setup for internal use,
please check the wiki for further details._

## Running the test suite

To run all the tests, execute the following command:

```bash
$ cabal run api-tests
```

To run only tests whose name contains a certain string, use the `-m` or
`--match=` flag:

```sh
$ cabal run api-tests -- -m "SQLServer" # SQLServer tests only
$ cabal run api-tests -- --match="Views" # All tests concerning views
```

The opposite flag is `-s` or `--skip=`, which will ignore tests containing the
given string:

```sh
$ cabal run api-tests -- -s "BigQuery" # Skip BigQuery tests
$ cabal run api-tests -- --skip="Mutations" # Skip tests around mutations
```

For additional information, consult the help section:

```bash
cabal run api-tests -- --help
```

The local databases persist even after shutting down the containers. If this is
undesirable, delete the databases using the following command:

```sh
docker compose down --volumes
```

### Enabling logging

See the logging section of [../test-harness/README.md#Logging](the test harness
README) for more information.

## Test Structure

The [feature matrix](https://hasura.io/docs/latest/databases/index/#schema)
defines the shape of this test suite. If we are writing a test for aggregation
queries, that test should live in `Test/Queries/AggregationSpec.hs`. If that
module becomes unwieldy, it should live in a module under the
`Test/Queries/Aggregation` directory.

Sometimes, tests are backend-specific. Particularly in the case of Postgres,
there are features we support that aren't available on other backends. In other
cases (such as BigQuery's handling of stringified numbers), there are
backend-specific behaviours we wish to verify. In these cases, these tests
should live under backend directories such as `Test/Postgres` or
`Test/BigQuery`. Note that a feature matrix test currently only running on one
backend should _still_ be in the feature matrix structure.

When tests are written to verify that a particular bug has been fixed, these
tests should be placed in the `Test/Regression` directory. They should contain
both a descriptive name _and_ the issue number that they address.

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

Test should be written (or reachable from) `tests :: SpecWith TestEnvironment`,
or `tests :: SpecWith (TestEnvironment, Foo)` for tests that use an additional
local state.

A typical test will look similar to this:

```hs
  it "Where id=1" \testEnvironment -> do
    let actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                hasura_author(where: {id: {_eq: 1}}) {
                  name
                  id
                }
              }
            |]

        expected :: Value
        expected =
          [yaml|
            data:
              hasura_author:
              - name: Author 1
                id: 1
          |]

    actual `shouldReturnYaml` expected
```

**Note**: these quasi-quoter can also perform string interpolation. See the
[../test-harness/README.md](the test harness README) for more information.

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

Now re-run the particular test case again so that the local database is setup.
You will still have access to that data once the test suite finishes running.
Now based on what you want to, you can either run the Hasura's Graphql engine
to debug this further or directly inspect the database using [any of its
clients](https://en.wikipedia.org/wiki/Comparison_of_database_administration_tools).

### Using GHCI

Alternatively it is also possible to manually start up the test environment in
the GHCI repl.

An example session:

```
$ cabal repl api-tests
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
[ 1 of 59] Compiling Harness.Constants ( lib/api-tests/Harness/Constants.hs, interpreted )
...
[59 of 59] Compiling Main             ( lib/api-tests/Spec.hs, interpreted )
Ok, 59 modules loaded.
> :module *Main *SpecHook *Test.SomeSpecImDeveloping
> te <- SpecHook.setupTestEnvironment
> te
<TestEnvironment: http://127.0.0.1:35975 >
> -- Setup the instance according to the Fixture
> cleanupPG <- Fixture.fixtureRepl Test.SomeSpecImDeveloping.postgresFixture te
>
> -- run tests or parts of tests manually here
> Test.SomeSpecImDeveloping.someExample te
>
> -- run the test with the hspec runner
> hspec (aroundAllWith (\a () ->a te) Test.SomeSpecImDeveloping>.spec)
Postgres
  ... [✔]
Citus
  ... [✔]
>
> -- Or perform other manual inspections, e.g. via the console or ghci.
>
> -- Cleanup before reloading
> cleanupPG
> SpecHook.teardownTestEnvironment te

> -- Reload changes made to the test module or HGE.
> :reload
```

Points to note:

- `SpecHook.setupTestEnvironment` starts the HGE server, and its url is
  revealed by `instance Show TestEnvironment`.
- `SpecHook.teardownTestEnvironment` stops it again.
  - This is a good idea to do before issuing the `:reload` command, because
    reloading loses the `te` reference but leaves the thread running!
- `Fixture.fixtureRepl` runs the setup action of a given `Fixture` and returns
  a corresponding teardown action.
  - After running this you can interact with the HGE console in the same state
    as when the tests are run.
  - Or you can run individual test `Example`s or `Spec`s.
- To successfully debug/develop a test in the GHCI repl, the test module
  should:
  - define its `Fixture`s as toplevel values,
  - define its `Example`s as toplevel values,
  - ... such that they can be used directly in the repl.

## Style guide

### Stick to [Simple Haskell](https://www.simplehaskell.org/)

This test suite should remain accessible to contributors who are new to Haskell
and/or the GraphQL engine codebase. Consider the
[power-to-weight](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto/#power-to-weight-ratio)
ratio of features, language extensions or abstractions before you introduce
them. For example, try to fully leverage Haskell '98 or 2010 features before
making use of more advanced ones.

### Write small, atomic, autonomous specs

Small: Keep specs short and succinct wherever possible. Consider reorganising
modules that grow much longer than ~200-300 lines of code.

_For example: The [`TestGraphQLQueryBasic` pytest
class](../tests-py/test_graphql_queries.py#L251) was ported to the hspec suite
as separate `BasicFields`, `LimitOffset`, `Where`, `Ordering`, `Directives`and
`Views` specs._

Atomic: Each spec should test only one feature against the backends (or
contexts) that support it. Each module should contain only the context setup
and teardown, and the tests themselves. The database schema, test data, and
feature expectations should be easy to reason about without navigating to
different module.

_For example: [`BasicFieldsSpec.hs`](Test/BasicFieldsSpec.hs)_

Autonomous: Each test should run independently of other tests, and not be
dependent on the results of a previous test. Shared test state, where
unavoidable, should be made explicit.

_For example: [Remote relationship tests](Test/RemoteRelationship/) explicitly
require shared state._

### Use the `Harness.*` hierarchy for common functions

Avoid functions or types in tests, other than calls to the `Harness.*` API.

Any supporting code should be in the `Harness.*` hierarchy and apply broadly to
the test suites overall.

## Troubleshooting

### `Database 'hasura' already exists. Choose a different database name.` or `schema "hasura" does not exist`

This typically indicates persistent DB state between test runs. Try `docker
compose down --volumes` to delete the DBs and restart the containers.

### General `DataConnector` failures

The DataConnector agent might be out of date. If you are getting a lot of test
failures, first try rebuilding the containers with `docker compose build` to
make sure you are using the latest version of the agent.

### Microsoft SQL Server failures on Apple aarch64 chips

This applies to all Apple hardware that uses aarch64 chips, e.g. the MacBook M1
or M2.

We have a few problems with Microsoft SQL Server on Apple aarch64:

1.  Due to compiler bugs in GHC 8.10.7, we need to use patched Haskell ODBC
    bindings as a workaround for aarch64 systems.

    Add the following to `cabal.project.local`:

    ```
    source-repository-package
      type: git
      location: https://github.com/soupi/odbc.git
      tag: a6acf6b4eca31022babbf8045f31a0f7f26c5923
    ```

2.  Microsoft has not yet released SQL Server for aarch64. We need to use Azure
    SQL Edge instead.

    You don't need to do anything if you're using the `make` commands; they
    will provide the correct image automatically.

    If you run `docker compose` directly, make sure to set the environment
    variable yourself:

    ```sh
    export MSSQL_IMAGE='mcr.microsoft.com/azure-sql-edge'
    ```

    You can add this to your _.envrc.local_ file if you like.

3.  Azure SQL Edge for aarch64 does not ship with the `sqlcmd` utility with
    which we use to setup the SQL Server schema.

    If you need it, you can instead use the `mssql-tools` Docker image, for
    example:

    ```
    docker run --rm -it --platform=linux/amd64 --net=host mcr.microsoft.com/mssql-tools \
      /opt/mssql-tools/bin/sqlcmd -S localhost,65003 -U SA -P <password>
    ```

    To make this easier, you might want to define an alias:

    ```
    alias sqlcmd='docker run --rm -it --platform=linux/amd64 --net=host mcr.microsoft.com/mssql-tools /opt/mssql-tools/bin/sqlcmd'
    ```

    You can also install them directly with `brew install microsoft/mssql-release/mssql-tools`.
