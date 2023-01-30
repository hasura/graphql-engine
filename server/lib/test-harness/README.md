# Hspec API Test Harness

The test harness provides convenience functions and infrastructure for writing
tests at the API level of `graphql-engine`. Specifically, it focuses on setup
and teardown for fixtures across our supported backends, and writing tests
against the GraphQL API.

## Setup

The test harness supports running tests using an external GraphQL-Engine
process, as opposed to a server instance running in a thread via library
linking. For this purpose, tests that use this need to know the path to the
`graphql-engine` executable via the `GRAPHQL_ENGINE` environment variable.

To point a test suite using the harness to the `graphql-engine` executable
produced in-tree by cabal, use e.g.:

```bash
$ export GRAPHQL_ENGINE=$(cabal list-bin exe:graphql-engine)
```

(Note that at the time of this writing, almost all of the test suite still
tests the Graphql Engine via library linking rather than via external process.
However, we expect the external process approach to become gradually more
prominent because it is more flexible and true to integration testing.)


The harness depends on the values in `src/Harness/Constants.hs` being correct.
Running `docker-compose up` in the root of `graphql-engine` should take care of
this for everything except BigQuery. Because BigQuery doesn't have a way of
installing locally for testing, we have to connect to an actual BigQuery
account. To test against BigQuery, the following environment variables must be
set:

- `HASURA_BIGQUERY_SERVICE_KEY`: the service account key.
- `HASURA_BIGQUERY_PROJECT_ID`: the ID of a project on the service account.

Once the account has been verified, the service account email variable can be
omitted from subsequent test runs. For the first run, however:

1. Ensure you have access to a Google Cloud Console service account. Store the
   project ID and account email in `HASURA_BIGQUERY_PROJECT_ID` variable.

   ```bash
   $ export HASURA_BIGQUERY_PROJECT_ID=hasura-project-identifier
   ```

   https://cloud.google.com/iam/docs/creating-managing-service-accounts#creating

2. Create and download a new service account key. Store the contents of file in
   a `HASURA_BIGQUERY_SERVICE_KEY` variable.

   ```bash
   $ export HASURA_BIGQUERY_SERVICE_KEY=$(cat /path/to/service/account)
   ```

   https://cloud.google.com/iam/docs/creating-managing-service-account-keys

3. Login and activate the service account if it is not already activated.

   https://cloud.google.com/sdk/gcloud/reference/auth/activate-service-account

4. Verify the service account is accessible via the BigQuery API by setting the
   `HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL` with the service account email and
   running the verification script.

   ```bash
   $ export HASURA_BIGQUERY_SERVICE_ACCOUNT_EMAIL=hasura@email.com
   $ ./scripts/verify-bigquery-creds.sh
   ```

   If the query succeeds, the service account is setup correctly to run tests
   against BigQuery locally!

_Note to Hasura team: a service account is already setup for internal use,
please check the wiki for further details._

## Logging

Currently, in order to enable logging, you have to edit the `engineLogLevel`
term in [Harness/Constants.hs](Harness/Constants.hs) manually and specify the
level of logging you want to display.

```diff
engineLogLevel :: Maybe L.LogLevel
- engineLogLevel = Nothing
+ engineLogLevel = Just L.LevelDebug -- Enable all logs
```

## Library Structure

### `Harness.Backend`

Utilies for interacting with all our specific backends. This includes functions
for checking liveness (is the database/backend running?), as well as setting up
fixtures (creating tables, inserting data, tracking them in Hasura, setting
permissions, and so on).

### `Harness.Quoter`

QuasiQuoters for writing non-Haskell languages such as YAML and GraphQL.

### `Harness.Test`

Specific functions for writing Hspec suites. This directory is probably best
understood starting with `Harness.Test.Fixture`, which describes the method for
running tests against one or more backends.

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
system state. The value produced by a `setupAction` is to be input into the
corresponding `teardownAction`, if the `setupAction` completed without throwing
an exception.

Therefore one `SetupAction` could create the DB tables, and the matching
teardown removes them. Pairing setup / teardown in this way makes it easier to
remove everything in the right order.

##### Setup

The setup actions are responsible for creating the environment for the test.
They need to, for example:

1. Clear and reconfigure the metadata
2. Setup tables and insert values
3. Track tables, add relationships, permissions

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
