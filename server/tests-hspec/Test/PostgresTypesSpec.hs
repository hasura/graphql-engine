{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tests around Postgres-specific database types
module Test.PostgresTypesSpec (spec) where

import Harness.Backend.Postgres as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Sql
import Harness.Quoter.Yaml
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = postgresSetup,
          teardown = postgresTeardown,
          customOptions = Nothing
        }
    ]
    tests

--------------------------------------------------------------------------------
-- Postgres

postgresSetup :: (TestEnvironment, ()) -> IO ()
postgresSetup (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment Postgres.defaultSourceMetadata

  -- Setup tables
  Postgres.run_
    [sql|
create table timestamp_test (
  id   serial primary key,
  time timestamptz
);
|]

  -- Track the tables
  GraphqlEngine.post_
    testEnvironment
    "/v1/metadata"
    [yaml|
type: postgres_track_table
args:
  source: postgres
  table:
    schema: hasura
    name: timestamp_test
|]

postgresTeardown :: (TestEnvironment, ()) -> IO ()
postgresTeardown _ = do
  Postgres.run_
    [sql|
DROP TABLE hasura.timestamp_test;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "doesn't mess up timestamps on insertion" $ \testEnvironment ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnvironment
          [graphql|
mutation {
  insert_hasura_timestamp_test(objects:[{
    time: "0001-01-01T00:00:57Z"
  }]) {
    returning {
      time
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_timestamp_test:
    returning:
      - time: "0001-01-01T00:00:57+00:00"
|]
