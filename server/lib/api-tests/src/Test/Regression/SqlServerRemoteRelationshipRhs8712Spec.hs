{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests that SqlServer is capable of returning many rows when invoked as the
-- rhs of a remote object relationship.
--
-- 'FOR JSON' selects at the top-level split up large results across multiple result
-- set rows, and the function that invokes the query needs to be aware of that.
module Test.Regression.SqlServerRemoteRelationshipRhs8712Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..))
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction as SetupAction
import Harness.Test.TestResource (Managed)
import Harness.TestEnvironment
  ( GlobalTestEnvironment,
    Server,
    TestEnvironment,
    focusFixtureLeft,
    focusFixtureRight,
  )
import Harness.Yaml (shouldAtLeastBe)
import Hasura.Prelude
import Test.Hspec hiding (context)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    contexts = NE.singleton $ Fixture.combineFixtures [] lhsPostgres rhsSQLServer

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | LHS context.
--
-- Each LHS context is responsible for setting up the remote relationship, and
-- for tearing it down. Each lhs context is given the JSON representation for
-- the table name on the RHS.
type LHSFixture = Value -> Fixture.Fixture (Maybe Server)

lhsPostgres :: LHSFixture
lhsPostgres tableName =
  (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = lhsPostgresMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsPostgresSetup tableName testEnv)
        ]
    }

--------------------------------------------------------------------------------

-- | RHS context
--
-- Each RHS context is responsible for setting up the target table, and for
-- returning the JSON representation of said table.
type RHSFixture = (Value, Fixture.Fixture ())

rhsSQLServer :: RHSFixture
rhsSQLServer =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
          { Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsSQLServerSetup testEnv)
              ]
          }
   in (table, context)

--------------------------------------------------------------------------------
-- Schema

-- | LHS
track :: Schema.Table
track =
  (Schema.table "track")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.columnNull "album_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt i, Schema.VStr "sometrack", Schema.VInt i] | i <- [0 .. 19]
        ]
    }

-- | RHS
album :: Schema.Table
album =
  (Schema.table "album")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.columnNull "artist_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt i, Schema.VStr (Text.concat $ replicate 10 "somealbum"), Schema.VInt 1]
          | i <- [0 .. 19 :: Int]
        ]
    }

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceName = "source"
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  Postgres.createTable testEnvironment track
  Postgres.insertTable testEnvironment track
  Schema.trackTable sourceName track testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: pg_create_remote_relationship
        args:
          source: *sourceName
          table:
            schema: *schemaName
            name: track
          name: album
          definition:
            to_source:
              source: target
              table: *rhsTableName
              relationship_type: object
              field_mapping:
                album_id: id
    |]

--------------------------------------------------------------------------------
-- RHS SQLServer

rhsSQLServerSetup :: (TestEnvironment, ()) -> IO ()
rhsSQLServerSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceName = "target"
      sourceConfig = SQLServer.defaultSourceConfiguration testEnvironment

  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mssql_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  SQLServer.createTable testEnvironment album
  SQLServer.insertTable testEnvironment album
  Schema.trackTable sourceName album testEnvironment

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Maybe Server)
tests = describe "object-relationship" $ do
  executionTests

-- | Basic queries using *-to-DB joins
executionTests :: SpecWith (TestEnvironment, Maybe Server)
executionTests = describe "execution" $ do
  -- fetches the relationship data
  it "related-data" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let query =
          [graphql|
          query {
            track: #{lhsSchema}_track {
              id
              title
              album {
                id
                title
              }
            }
          }
          |]
        expected =
          [yaml|
          data:
            track: []
          |]

    actual <- (GraphqlEngine.postGraphql testEnvironment query)

    actual `shouldAtLeastBe` expected
