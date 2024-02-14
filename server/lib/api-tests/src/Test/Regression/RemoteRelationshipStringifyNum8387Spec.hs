{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- Regression test for https://github.com/hasura/graphql-engine/issues/8387
module Test.Regression.RemoteRelationshipStringifyNum8387Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (SelectPermissionDetails (..))
import Harness.Permissions qualified as Permissions
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.Schema (Table (..))
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.FixtureName (FixtureName (..))
import Harness.Test.SetupAction qualified as SetupAction
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment (..), getBackendTypeConfig, stopServer)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)
import Test.Schema.RemoteRelationships.MetadataAPI.Common qualified as Common

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.hgeWithEnv [("HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES", "true")]
    $ Fixture.runWithLocalTestEnvironment contexts testsWithFeatureOn

  Fixture.hgeWithEnv [("HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES", "false")]
    $ Fixture.runWithLocalTestEnvironment contexts testsWithFeatureOff
  where
    lhsFixtures = [lhsPostgres, lhsRemoteServer]
    rhsFixtures = [rhsPostgres]
    contexts = NE.fromList $ Fixture.combineFixtures [] <$> lhsFixtures <*> rhsFixtures

--------------------------------------------------------------------------------

-- | Left-hand-side (LHS) fixtures
lhsPostgres :: Fixture.LHSFixture
lhsPostgres tableName =
  (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsPostgresSetup tableName testEnv)
        ]
    }

lhsRemoteServer :: Fixture.LHSFixture
lhsRemoteServer tableName =
  (Fixture.fixture $ Fixture.RemoteGraphQLServer)
    { Fixture.mkLocalTestEnvironment = Common.lhsRemoteServerMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ Fixture.SetupAction
            { Fixture.setupAction = lhsRemoteServerSetup tableName testEnv,
              Fixture.teardownAction = \_ -> lhsRemoteServerTeardown testEnv
            }
        ]
    }

--------------------------------------------------------------------------------

-- | Right-hand-side (RHS) fixtures
rhsPostgres :: Fixture.RHSFixture
rhsPostgres =
  let table =
        [interpolateYaml|
      schema: hasura
      name: #{ rhsTableName_ }
    |]
      context =
        (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
          { Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsPostgresSetup testEnv)
              ],
            Fixture.customOptions =
              Just
                $ Fixture.defaultOptions
                  { Fixture.stringifyNumbers = True
                  }
          }
   in (table, context)

--------------------------------------------------------------------------------
-- Schema

-- | LHS
track :: Schema.Table
track =
  (Schema.table lhsTableName_)
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.columnNull "album_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "track1_album1", Schema.VInt 1]
        ]
    }

-- | RHS
album :: Schema.Table
album =
  (Schema.table rhsTableName_)
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.columnNull "artist_id" Schema.TInt,
          Schema.column "play_count" bigIntType,
          Schema.column "version" floatType
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "album1", Schema.VInt 1, mkBigIntValue "1000000000000", mkFloatValue "1.075"]
        ]
    }

floatType :: Schema.ScalarType
floatType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "NUMERIC"
      }

mkFloatValue :: Text -> Schema.ScalarValue
mkFloatValue int =
  Schema.VCustomValue
    $ Schema.defaultBackendScalarValue
      { Schema.bsvPostgres = Just (Schema.Unquoted int)
      }

bigIntType :: Schema.ScalarType
bigIntType =
  Schema.TCustomType
    $ Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "BIGINT"
      }

mkBigIntValue :: Text -> Schema.ScalarValue
mkBigIntValue int =
  Schema.VCustomValue
    $ Schema.defaultBackendScalarValue
      { Schema.bsvPostgres = Just (Schema.Unquoted int)
      }

--------------------------------------------------------------------------------
-- LHS

lhsSourceName_ :: Text
lhsSourceName_ = "source"

lhsTableName_ :: Text
lhsTableName_ = "track"

lhsRole1 :: Permissions.Permission
lhsRole1 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionSource = Just lhsSourceName_,
        selectPermissionRole = "role1",
        selectPermissionTable = lhsTableName_,
        selectPermissionColumns = (["id", "title", "album_id"] :: [Text])
      }

lhsRole2 :: Permissions.Permission
lhsRole2 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionSource = Just lhsSourceName_,
        selectPermissionRole = "role2",
        selectPermissionTable = lhsTableName_,
        selectPermissionColumns = (["id", "title", "album_id"] :: [Text])
      }

createRemoteRelationship :: Value -> TestEnvironment -> IO ()
createRemoteRelationship rhsTableName testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
      schemaName = Schema.getSchemaName testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [interpolateYaml|
      type: #{ backendType }_create_remote_relationship
      args:
        source: #{ lhsSourceName_ }
        table:
          schema: #{ schemaName }
          name: #{ lhsTableName_ }
        name: #{ rhsTableName_ }
        definition:
          to_source:
            source: #{ rhsSourceName_ }
            table: #{ rhsTableName }
            relationship_type: object
            field_mapping:
              album_id: id
    |]

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (testEnvironment, _) = do
  let testEnvironmentPostgres = testEnvironment {fixtureName = Backend Postgres.backendTypeMetadata}
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment

  -- Add remote source
  Schema.addSource lhsSourceName_ sourceConfig testEnvironmentPostgres

  -- Setup tables only
  Postgres.createTable testEnvironment track
  Postgres.insertTable testEnvironment track
  Schema.trackTable (Text.unpack lhsSourceName_) track testEnvironmentPostgres

  -- Setup metadata
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironmentPostgres lhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironmentPostgres lhsRole2

  createRemoteRelationship rhsTableName testEnvironmentPostgres

--------------------------------------------------------------------------------
-- LHS Remote Server

lhsRemoteServerSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerSetup tableName (testEnvironment, maybeRemoteServer) = case maybeRemoteServer of
  Nothing -> error "XToDBObjectRelationshipSpec: remote server local testEnvironment did not succesfully create a server"
  Just remoteServer -> do
    let remoteSchemaEndpoint = GraphqlEngine.serverUrl remoteServer ++ "/graphql"
    GraphqlEngine.postMetadata_
      testEnvironment
      [interpolateYaml|
        type: bulk
        args:
        - type: add_remote_schema
          args:
            name: #{ lhsSourceName_ }
            definition:
              url: #{ remoteSchemaEndpoint }
        - type: create_remote_schema_remote_relationship
          args:
            remote_schema: #{ lhsSourceName_ }
            type_name: hasura_track
            name: #{ rhsTableName_ }
            definition:
              to_source:
                source: target
                table: #{ tableName }
                relationship_type: object
                field_mapping:
                  album_id: id
      |]

lhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

--------------------------------------------------------------------------------
-- RHS

rhsSourceName_ :: Text
rhsSourceName_ = "target"

rhsTableName_ :: Text
rhsTableName_ = "album"

rhsRole1 :: Permissions.Permission
rhsRole1 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionSource = Just rhsSourceName_,
        selectPermissionRole = "role1",
        selectPermissionTable = rhsTableName_,
        selectPermissionColumns = (["title", "artist_id", "play_count", "version"] :: [Text]),
        selectPermissionRows =
          [yaml|
          artist_id:
            _eq: x-hasura-artist-id
        |]
      }

rhsRole2 :: Permissions.Permission
rhsRole2 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionSource = Just rhsSourceName_,
        selectPermissionRole = "role2",
        selectPermissionTable = rhsTableName_,
        selectPermissionColumns = (["title", "artist_id", "play_count", "version"] :: [Text]),
        selectPermissionRows =
          [yaml|
          artist_id:
            _eq: x-hasura-artist-id
        |],
        selectPermissionAllowAggregations = True
      }

--------------------------------------------------------------------------------
-- RHS Postgres

rhsPostgresSetup :: (TestEnvironment, ()) -> IO ()
rhsPostgresSetup (testEnvironment, _) = do
  let testEnvironmentPostgres = testEnvironment {fixtureName = Backend Postgres.backendTypeMetadata}
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment

  -- Add remote source
  Schema.addSource rhsSourceName_ sourceConfig testEnvironmentPostgres

  -- setup tables only
  Postgres.createTable testEnvironment album
  Postgres.insertTable testEnvironment album
  Schema.trackTable (Text.unpack rhsSourceName_) album testEnvironmentPostgres

  -- setup metadata
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironmentPostgres rhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironmentPostgres rhsRole2

--------------------------------------------------------------------------------
-- Tests

-- | Basic queries using *-to-DB joins
testsWithFeatureOn :: SpecWith (TestEnvironment, Maybe Server)
testsWithFeatureOn = describe "object-relationship (stringified numeric types)" $ do
  -- fetches the relationship data
  it "related-data" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
              query {
                track: hasura_track {
                  album {
                    title
                    play_count
                    version
                  }
                }
              }
            |]
        expectedResponse =
          [interpolateYaml|
              data:
                track:
                - album:
                    title: "album1"
                    play_count: "1000000000000"
                    version: "1.075"
            |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

-- | Expected behaviour when HASURA_GRAPHQL_STRINGIFY_NUMERIC_TYPES is false
testsWithFeatureOff :: SpecWith (TestEnvironment, Maybe Server)
testsWithFeatureOff = describe "object-relationship (no stringified numeric types)" $ do
  -- fetches the relationship data
  it "related-data" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
              query {
                track: hasura_track {
                  album {
                    title
                    play_count
                    version
                  }
                }
              }
            |]
        expectedResponse =
          [interpolateYaml|
              data:
                track:
                - album:
                    title: "album1"
                    play_count: 1.0e12
                    version: 1.075
            |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
