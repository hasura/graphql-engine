{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- Regression test for https://github.com/hasura/graphql-engine/issues/8387
module Test.Regression.RemoteRelationshipStringifyNum8387Spec (spec) where

import Data.Aeson (Value)
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types qualified as Morpheus
import Data.Typeable (Typeable)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..))
import Harness.Test.Schema qualified as Schema
import Harness.Test.TestResource (Managed)
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment, backendTypeConfig, stopServer)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    lhsFixtures = [lhsPostgresStringifyNums, lhsRemoteServerStringifyNums]
    rhsFixtures = [rhsPostgresStringifyNums]
    contexts = NE.fromList $ Fixture.combineFixtures <$> lhsFixtures <*> rhsFixtures

--------------------------------------------------------------------------------

-- | Left-hand-side (LHS) fixtures
lhsPostgresStringifyNums :: Fixture.LHSFixture
lhsPostgresStringifyNums tableName =
  (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = lhsPostgresMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ Fixture.SetupAction
            { Fixture.setupAction = lhsPostgresSetup tableName testEnv,
              Fixture.teardownAction = \_ -> lhsPostgresTeardown testEnv
            }
        ],
      Fixture.customOptions =
        Just $
          Fixture.defaultOptions
            { Fixture.stringifyNumbers = True
            }
    }

lhsRemoteServerStringifyNums :: Fixture.LHSFixture
lhsRemoteServerStringifyNums tableName =
  (Fixture.fixture $ Fixture.RemoteGraphQLServer)
    { Fixture.mkLocalTestEnvironment = lhsRemoteServerMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ Fixture.SetupAction
            { Fixture.setupAction = lhsRemoteServerSetup tableName testEnv,
              Fixture.teardownAction = \_ -> lhsRemoteServerTeardown testEnv
            }
        ],
      Fixture.customOptions =
        Just $
          Fixture.defaultOptions
            { Fixture.stringifyNumbers = True
            }
    }

--------------------------------------------------------------------------------

-- | Right-hand-side (RHS) fixtures
rhsPostgresStringifyNums :: Fixture.RHSFixture
rhsPostgresStringifyNums =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
          { Fixture.setupTeardown = \testEnv ->
              [ Fixture.SetupAction
                  { Fixture.setupAction = rhsPostgresSetup testEnv,
                    Fixture.teardownAction = \_ -> rhsPostgresTeardown testEnv
                  }
              ],
            Fixture.customOptions =
              Just $
                Fixture.defaultOptions
                  { Fixture.stringifyNumbers = True
                  }
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
          Schema.columnNull "album_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VInt 1]
        ]
    }

-- | RHS
album :: Schema.Table
album =
  (Schema.table "album")
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
  Schema.TCustomType $
    Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "NUMERIC"
      }

mkFloatValue :: Text -> Schema.ScalarValue
mkFloatValue int =
  Schema.VCustomValue $
    Schema.defaultBackendScalarValue
      { Schema.bsvPostgres = Just (Schema.Unquoted int)
      }

bigIntType :: Schema.ScalarType
bigIntType =
  Schema.TCustomType $
    Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just "BIGINT"
      }

mkBigIntValue :: Text -> Schema.ScalarValue
mkBigIntValue int =
  Schema.VCustomValue $
    Schema.defaultBackendScalarValue
      { Schema.bsvPostgres = Just (Schema.Unquoted int)
      }

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (testEnvironment, _) = do
  let sourceName = "source"
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
  Schema.trackTable sourceName track (testEnvironment {backendTypeConfig = Just (Postgres.backendTypeMetadata)})
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: pg_create_select_permission
        args:
          source: *sourceName
          role: role1
          table:
            schema: *schemaName
            name: track
          permission:
            columns: '*'
            filter: {}
      - type: pg_create_select_permission
        args:
          source: *sourceName
          role: role2
          table:
            schema: *schemaName
            name: track
          permission:
            columns: '*'
            filter: {}
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

lhsPostgresTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresTeardown (_testEnvironment, _) =
  pure ()

--------------------------------------------------------------------------------
-- LHS Remote Server

-- TODO AS: factor out

-- | To circumvent Morpheus' default behaviour, which is to capitalize type
-- names and field names for Haskell records to be consistent with their
-- corresponding GraphQL equivalents, we define most of the schema manually with
-- the following options.
hasuraTypeOptions :: Morpheus.GQLTypeOptions
hasuraTypeOptions =
  Morpheus.defaultTypeOptions
    { -- transformation to apply to constructors, for enums; we simply map to
      -- lower case:
      --   Asc -> asc
      Morpheus.constructorTagModifier = map toLower,
      -- transformation to apply to field names; we drop all characters up to and
      -- including the first underscore:
      --   hta_where -> where
      Morpheus.fieldLabelModifier = tail . dropWhile (/= '_'),
      -- transformation to apply to type names; we split the name on uppercase
      -- letters, intercalate with underscore, and map everything to lowercase:
      --   HasuraTrack -> hasura_track
      Morpheus.typeNameModifier = \_ ->
        map toLower
          . intercalate "_"
          . split (dropBlanks $ keepDelimsL $ whenElt isUpper)
    }

data Query m = Query
  { hasura_track :: HasuraTrackArgs -> m [HasuraTrack m]
  }
  deriving (Generic)

instance Typeable m => Morpheus.GQLType (Query m)

data HasuraTrackArgs = HasuraTrackArgs
  { ta_where :: Maybe HasuraTrackBoolExp,
    ta_order_by :: Maybe [HasuraTrackOrderBy],
    ta_limit :: Maybe Int
  }
  deriving (Generic)

instance Morpheus.GQLType HasuraTrackArgs where
  typeOptions _ _ = hasuraTypeOptions

data HasuraTrack m = HasuraTrack
  { t_id :: m (Maybe Int),
    t_album_id :: m (Maybe Int)
  }
  deriving (Generic)

instance Typeable m => Morpheus.GQLType (HasuraTrack m) where
  typeOptions _ _ = hasuraTypeOptions

data HasuraTrackOrderBy = HasuraTrackOrderBy
  { tob_id :: Maybe OrderType,
    tob_album_id :: Maybe OrderType
  }
  deriving (Generic)

instance Morpheus.GQLType HasuraTrackOrderBy where
  typeOptions _ _ = hasuraTypeOptions

data HasuraTrackBoolExp = HasuraTrackBoolExp
  { tbe__and :: Maybe [HasuraTrackBoolExp],
    tbe__or :: Maybe [HasuraTrackBoolExp],
    tbe__not :: Maybe HasuraTrackBoolExp,
    tbe_id :: Maybe IntCompExp,
    tbe_album_id :: Maybe IntCompExp
  }
  deriving (Generic)

instance Morpheus.GQLType HasuraTrackBoolExp where
  typeOptions _ _ = hasuraTypeOptions

data OrderType = Asc | Desc
  deriving (Show, Generic)

instance Morpheus.GQLType OrderType where
  typeOptions _ _ = hasuraTypeOptions

[gqlDocument|

input IntCompExp {
  _eq: Int
}

input StringCompExp {
  _eq: String
}

|]

lhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsRemoteServerMkLocalTestEnvironment _ =
  Just <$> RemoteServer.run (RemoteServer.generateQueryInterpreter (Query {hasura_track}))
  where
    -- Implements the @hasura_track@ field of the @Query@ type.
    hasura_track (HasuraTrackArgs {..}) = do
      let filterFunction = case ta_where of
            Nothing -> const True
            Just whereArg -> flip matchTrack whereArg
          orderByFunction = case ta_order_by of
            Nothing -> \_ _ -> EQ
            Just orderByArg -> orderTrack orderByArg
          limitFunction = maybe Hasura.Prelude.id take ta_limit
      pure $
        tracks
          & filter filterFunction
          & sortBy orderByFunction
          & limitFunction
          & map mkTrack
    -- Returns True iif the given track matches the given boolean expression.
    matchTrack trackInfo@(trackId, maybeAlbumId) (HasuraTrackBoolExp {..}) =
      and
        [ all (all (matchTrack trackInfo)) tbe__and,
          all (any (matchTrack trackInfo)) tbe__or,
          not (any (matchTrack trackInfo) tbe__not),
          all (matchInt trackId) tbe_id,
          all (matchMaybeInt maybeAlbumId) tbe_album_id
        ]
    matchInt intField IntCompExp {..} = Just intField == _eq
    matchMaybeInt maybeIntField IntCompExp {..} = maybeIntField == _eq
    -- Returns an ordering between the two given tracks.
    orderTrack
      orderByList
      (trackId1, trackAlbumId1)
      (trackId2, trackAlbumId2) =
        flip foldMap orderByList \HasuraTrackOrderBy {..} ->
          if
              | Just idOrder <- tob_id -> case idOrder of
                  Asc -> compare trackId1 trackId2
                  Desc -> compare trackId2 trackId1
              | Just albumIdOrder <- tob_album_id ->
                  compareWithNullLast albumIdOrder trackAlbumId1 trackAlbumId2
              | otherwise ->
                  error "empty order_by object"
    compareWithNullLast Desc x1 x2 = compareWithNullLast Asc x2 x1
    compareWithNullLast Asc Nothing Nothing = EQ
    compareWithNullLast Asc (Just _) Nothing = LT
    compareWithNullLast Asc Nothing (Just _) = GT
    compareWithNullLast Asc (Just x1) (Just x2) = compare x1 x2
    tracks = [(1, Just 1)]
    mkTrack (trackId, albumId) =
      HasuraTrack
        { t_id = pure $ Just trackId,
          t_album_id = pure albumId
        }

lhsRemoteServerSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerSetup tableName (testEnvironment, maybeRemoteServer) = case maybeRemoteServer of
  Nothing -> error "XToDBObjectRelationshipSpec: remote server local testEnvironment did not succesfully create a server"
  Just remoteServer -> do
    let remoteSchemaEndpoint = GraphqlEngine.serverUrl remoteServer ++ "/graphql"
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: bulk
args:
- type: add_remote_schema
  args:
    name: source
    definition:
      url: *remoteSchemaEndpoint
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: source
    type_name: hasura_track
    name: album
    definition:
      to_source:
        source: target
        table: *tableName
        relationship_type: object
        field_mapping:
          album_id: id
      |]

lhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

--------------------------------------------------------------------------------
-- RHS Postgres

rhsPostgresSetup :: (TestEnvironment, ()) -> IO ()
rhsPostgresSetup (testEnvironment, _) = do
  let sourceName = "target"
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
  Postgres.createTable testEnvironment album
  Postgres.insertTable testEnvironment album
  Schema.trackTable sourceName album (testEnvironment {backendTypeConfig = Just (Postgres.backendTypeMetadata)})

  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: pg_create_select_permission
  args:
    source: *sourceName
    role: role1
    table:
      schema: *schemaName
      name: album
    permission:
      columns:
        - title
        - artist_id
        - play_count
        - version
      filter:
        artist_id:
          _eq: x-hasura-artist-id
- type: pg_create_select_permission
  args:
    source: *sourceName
    role: role2
    table:
      schema: *schemaName
      name: album
    permission:
      columns: [id, title, artist_id, play_count, version]
      filter:
        artist_id:
          _eq: x-hasura-artist-id
      limit: 1
      allow_aggregations: true
  |]

rhsPostgresTeardown :: (TestEnvironment, ()) -> IO ()
rhsPostgresTeardown (_testEnvironment, _) =
  pure ()

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith (TestEnvironment, Maybe Server)
tests opts = describe "object-relationship" $ do
  executionTests opts

-- | Basic queries using *-to-DB joins
executionTests :: Fixture.Options -> SpecWith (TestEnvironment, Maybe Server)
executionTests opts = describe "execution" $ do
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
          [yaml|
            data:
              track:
              - album:
                  title: "album1"
                  play_count: "1000000000000"
                  version: "1.075"
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
