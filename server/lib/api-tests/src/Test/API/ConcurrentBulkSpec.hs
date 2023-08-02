{-# LANGUAGE QuasiQuotes #-}

-- | Test that @concurrent_bulk@ works as expected.
module Test.API.ConcurrentBulkSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine
import Harness.Quoter.Yaml
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), getBackendTypeConfig)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it, shouldContain)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    (tests postgresRunSqlQuery)
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    (tests mssqlRunSqlQuery)

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "example")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData = []
      }
  ]

tests :: (TestEnvironment -> String -> IO Value) -> SpecWith TestEnvironment
tests query =
  describe "'concurrent_bulk'" do
    it "returns the same results as regular 'bulk'" \testEnv -> do
      let actual = query testEnv "concurrent_bulk"
      expected <- query testEnv "bulk"
      show expected `shouldContain` "TuplesOk"
      shouldReturnYaml testEnv actual expected

    it "fails when a query is not read-only" \testEnv -> do
      let expected =
            [yaml|
              code: unexpected
              error: Only read-only queries are allowed in a concurrent_bulk
              path: $
            |]
      shouldReturnYaml testEnv (runSqlDrop testEnv) expected

postgresRunSqlQuery :: TestEnvironment -> String -> IO Value
postgresRunSqlQuery testEnvironment bulkType = do
  let backendTypeMetadata = fromMaybe (error "Expected a backend type but got nothing") $ getBackendTypeConfig testEnvironment
      sourceName = BackendType.backendSourceName backendTypeMetadata
      backendPrefix = BackendType.backendTypeString backendTypeMetadata
  postV2Query 200 testEnvironment
    $ [interpolateYaml|
      type: #{bulkType}
      args:
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT column_name, table_name, is_generated, is_identity, identity_generation
            FROM information_schema.columns where table_schema = 'public';
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT is_identity
            FROM information_schema.columns where table_schema = 'public';
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT column_name, table_name
            FROM information_schema.columns where table_schema = 'public';
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT column_name, table_name, identity_generation
            FROM information_schema.columns where table_schema = 'public';
    |]

runSqlDrop :: TestEnvironment -> IO Value
runSqlDrop testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Expected a backend type but got nothing") $ getBackendTypeConfig testEnvironment
      sourceName = BackendType.backendSourceName backendTypeMetadata
      backendPrefix = BackendType.backendTypeString backendTypeMetadata
  postV2Query
    500
    testEnvironment
    [interpolateYaml|
      type: concurrent_bulk
      args:
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: false
          source: #{sourceName}
          sql: |
            drop table example;
    |]

mssqlRunSqlQuery :: TestEnvironment -> String -> IO Value
mssqlRunSqlQuery testEnvironment bulkType = do
  let backendTypeMetadata = fromMaybe (error "Expected a backend type but got nothing") $ getBackendTypeConfig testEnvironment
      sourceName = BackendType.backendSourceName backendTypeMetadata
      backendPrefix = BackendType.backendTypeString backendTypeMetadata
  postV2Query 200 testEnvironment
    $ [interpolateYaml|
      type: #{bulkType}
      args:
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT s.name AS schema_name
            FROM sys.schemas s
            WHERE
              s.name NOT IN (
                'guest', 'INFORMATION_SCHEMA', 'sys',
                'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
              )
            ORDER BY s.name;
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT s.name AS schema_name
            FROM sys.schemas s
            WHERE
              s.name NOT IN (
                'db_owner', 'db_securityadmin', 'db_accessadmin',
                'db_backupoperator', 'db_ddladmin', 'db_datawriter',
                'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
              )
            ORDER BY s.name;
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT s.name AS schema_name
            FROM sys.schemas s
            WHERE
              s.name NOT IN (
                'guest', 'INFORMATION_SCHEMA', 'sys'
              )
            ORDER BY s.name;
      - type: #{backendPrefix}_run_sql
        args:
          cascade: false
          read_only: true
          source: #{sourceName}
          sql: |
            SELECT s.name AS schema_name
            FROM sys.schemas s
            WHERE
              s.name NOT IN (
                'guest', 'INFORMATION_SCHEMA', 'sys',
                'db_owner', 'db_securityadmin', 'db_accessadmin',
                'db_backupoperator', 'db_ddladmin', 'db_datawriter',
                'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
              )
            ORDER BY s.name;
    |]
