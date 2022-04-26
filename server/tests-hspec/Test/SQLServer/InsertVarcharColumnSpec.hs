{-# LANGUAGE QuasiQuotes #-}

-- | Test inserting non-ASCII characters in @'varchar' column type
module Test.SQLServer.InsertVarcharColumnSpec (spec) where

import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Sql (sql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, it)
import Prelude

-- ** Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    [ Context.Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = Context.noLocalTestEnvironment,
          setup = mssqlSetup,
          teardown = mssqlTeardown,
          customOptions = Nothing
        }
    ]
    tests

-- ** Setup and teardown

mssqlSetup :: (TestEnvironment, ()) -> IO ()
mssqlSetup (testEnv, ()) = do
  -- Clear metadata and configure the source
  GraphqlEngine.setSource testEnv Sqlserver.defaultSourceMetadata

  -- Setup DB schema
  Sqlserver.run_ setupSQL

  -- Track tables
  GraphqlEngine.postMetadata_
    testEnv
    [yaml|
type: bulk
args:
- type: mssql_track_table
  args:
    source: mssql
    table:
      schema: hasura
      name: test
- type: mssql_track_table
  args:
    source: mssql
    table:
      schema: hasura
      name: test_bin
|]

setupSQL :: String
setupSQL =
  [sql|
CREATE TABLE test (
  id INT PRIMARY KEY,
  varchar_column varchar(MAX)
);
CREATE TABLE test_bin (
  id INT PRIMARY KEY,
  varchar_column varchar(MAX) collate SQL_Latin1_General_CP437_BIN
);
|]

mssqlTeardown :: (TestEnvironment, ()) -> IO ()
mssqlTeardown (testEnv, ()) = do
  -- Untrack table
  GraphqlEngine.postMetadata_
    testEnv
    [yaml|
type: bulk
args:
- type: mssql_untrack_table
  args:
    source: mssql
    table:
      schema: hasura
      name: test
- type: mssql_untrack_table
  args:
    source: mssql
    table:
      schema: hasura
      name: test_bin
|]

  -- Teardown DB schema
  Sqlserver.run_ teardownSQL

  -- Clear metadata
  GraphqlEngine.clearMetadata testEnv

teardownSQL :: String
teardownSQL =
  [sql|
DROP TABLE test;
DROP TABLE test_bin;
|]

-- * Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests opts = do
  it "Insert into varchar column with non ASCII value" $ \testEnv ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
mutation {
  insert_hasura_test(
    objects: [{id: 1, varchar_column: "££££"}]
  ) {
    affected_rows
    returning{
      id
      varchar_column
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_test:
    returning:
    - id: 1
      varchar_column: "££££"
    affected_rows: 1

|]

  it "Insert into collated varchar column with non ASCII value" $ \testEnv ->
    shouldReturnYaml
      opts
      ( GraphqlEngine.postGraphql
          testEnv
          [graphql|
mutation {
  insert_hasura_test_bin(
    objects: [{id: 1, varchar_column: "££££"}]
  ) {
    affected_rows
    returning{
      id
      varchar_column
    }
  }
}
|]
      )
      [yaml|
data:
  insert_hasura_test_bin:
    returning:
    - id: 1
      varchar_column: "££££"
    affected_rows: 1

|]
