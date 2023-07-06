{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Tests for object remote relationships to databases. Remote relationships
-- are relationships that are not local to a given source or remote schema, and
-- are handled by the engine itself. Object relationsips are 1-to-1
-- relationships.
--
-- All tests use the same GraphQL syntax, and the only difference is in the
-- setup: we do a cartesian product of all kinds of sources we support on the
-- left-hand side and all databases we support on the right-hand side.
module Test.Schema.RemoteRelationships.XToDBObjectRelationshipSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson qualified as J
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types qualified as Morpheus
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Quoter.Yaml.InterpolateYaml (interpolateYaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Schema (Table (..))
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction as SetupAction
import Harness.Test.TestResource (Managed)
import Harness.TestEnvironment
  ( GlobalTestEnvironment,
    Server,
    TestEnvironment (uniqueTestId),
    focusFixtureLeft,
    focusFixtureRight,
    stopServer,
  )
import Harness.Yaml (shouldReturnYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (HasCallStack, SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    lhsFixtures = [lhsPostgres, lhsCockroach, lhsSQLServer, lhsCitus, lhsSqlite, lhsRemoteServer]
    rhsFixtures = [rhsPostgres, rhsCockroach, rhsSQLServer, rhsCitus, rhsSqlite]
    contexts = NE.fromList $ Fixture.combineFixtures [setupSqliteAgentAction] <$> lhsFixtures <*> rhsFixtures
    setupSqliteAgentAction testEnv = SetupAction.noTeardown (Sqlite.setupSqliteAgent testEnv)

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

lhsCitus :: LHSFixture
lhsCitus tableName =
  (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = lhsCitusMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsCitusSetup tableName testEnv)
        ]
    }

lhsCockroach :: LHSFixture
lhsCockroach tableName =
  (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = lhsCockroachMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsCockroachSetup tableName testEnv)
        ],
      Fixture.customOptions = Nothing
    }

lhsSQLServer :: LHSFixture
lhsSQLServer tableName =
  (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = lhsSQLServerMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsSQLServerSetup tableName testEnv)
        ]
    }

lhsSqlite :: LHSFixture
lhsSqlite tableName =
  (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction (lhsSqliteSetup tableName testEnv) sqliteTeardown
        ]
    }

lhsRemoteServer :: LHSFixture
lhsRemoteServer tableName =
  (Fixture.fixture $ Fixture.RemoteGraphQLServer)
    { Fixture.mkLocalTestEnvironment = lhsRemoteServerMkLocalTestEnvironment,
      Fixture.setupTeardown = \testEnv ->
        [ Fixture.SetupAction
            { Fixture.setupAction = lhsRemoteServerSetup tableName testEnv,
              Fixture.teardownAction = \_ -> lhsRemoteServerTeardown testEnv
            }
        ]
    }

--------------------------------------------------------------------------------

-- | RHS context
--
-- Each RHS context is responsible for setting up the target table, and for
-- returning the JSON representation of said table.
type RHSFixture = (Value, Fixture.Fixture ())

rhsPostgres :: RHSFixture
rhsPostgres =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
          { Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsPostgresSetup testEnv)
              ]
          }
   in (table, context)

rhsCitus :: RHSFixture
rhsCitus =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
          { Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsCitusSetup testEnv)
              ]
          }
   in (table, context)

rhsCockroach :: RHSFixture
rhsCockroach =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
          { Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsCockroachSetup testEnv)
              ],
            Fixture.customOptions = Nothing
          }
   in (table, context)

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

rhsSqlite :: RHSFixture
rhsSqlite =
  let sqliteRhsTableName = J.toJSON ["main" :: Text, "album"]
      context =
        (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
          { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
            Fixture.setupTeardown = \testEnv ->
              [ SetupAction (rhsSqliteSetup testEnv) sqliteTeardown
              ]
          }
   in (sqliteRhsTableName, context)

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
        [ [Schema.VInt 1, Schema.VStr "track1_album1", Schema.VInt 1],
          [Schema.VInt 2, Schema.VStr "track2_album1", Schema.VInt 1],
          [Schema.VInt 3, Schema.VStr "track3_album1", Schema.VInt 1],
          [Schema.VInt 4, Schema.VStr "track1_album2", Schema.VInt 2],
          [Schema.VInt 5, Schema.VStr "track2_album2", Schema.VInt 2],
          [Schema.VInt 6, Schema.VStr "track1_album3", Schema.VInt 3],
          [Schema.VInt 7, Schema.VStr "track2_album3", Schema.VInt 3],
          [Schema.VInt 8, Schema.VStr "track_no_album", Schema.VNull]
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
        [ [Schema.VInt 1, Schema.VStr "album1_artist1", Schema.VInt 1],
          [Schema.VInt 2, Schema.VStr "album2_artist1", Schema.VInt 1],
          [Schema.VInt 3, Schema.VStr "album3_artist2", Schema.VInt 2]
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

--------------------------------------------------------------------------------
-- LHS Citus

lhsCitusMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsCitusMkLocalTestEnvironment _ = pure Nothing

lhsCitusSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsCitusSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceName = "source"
      sourceConfig = Citus.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: citus_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  Citus.createTable testEnvironment track
  Citus.insertTable testEnvironment track
  Schema.trackTable sourceName track testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: citus_create_select_permission
        args:
          source: *sourceName
          role: role1
          table:
            schema: *schemaName
            name: track
          permission:
            columns: '*'
            filter: {}
      - type: citus_create_select_permission
        args:
          source: *sourceName
          role: role2
          table:
            schema: *schemaName
            name: track
          permission:
            columns: '*'
            filter: {}
      - type: citus_create_remote_relationship
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
-- LHS Cockroach

lhsCockroachMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsCockroachMkLocalTestEnvironment _ = pure Nothing

lhsCockroachSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsCockroachSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceName = "source"
      sourceConfig = Cockroach.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment
  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: cockroach_add_source
      args:
        name: *sourceName
        configuration: *sourceConfig
    |]
  -- setup tables only
  Cockroach.createTable testEnvironment track
  Cockroach.insertTable testEnvironment track
  Schema.trackTable sourceName track testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: cockroach_create_select_permission
        args:
          source: *sourceName
          role: role1
          table:
            schema: *schemaName
            name: track
          permission:
            columns: '*'
            filter: {}
      - type: cockroach_create_select_permission
        args:
          source: *sourceName
          role: role2
          table:
            schema: *schemaName
            name: track
          permission:
            columns: '*'
            filter: {}
      - type: cockroach_create_remote_relationship
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
-- LHS SQLServer

lhsSQLServerMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsSQLServerMkLocalTestEnvironment _ = pure Nothing

lhsSQLServerSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceName = "source"
      sourceConfig = SQLServer.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mssql_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  SQLServer.createTable testEnvironment track
  SQLServer.insertTable testEnvironment track
  Schema.trackTable sourceName track testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: mssql_create_select_permission
  args:
    source: *sourceName
    role: role1
    table:
      schema: *schemaName
      name: track
    permission:
      columns: '*'
      filter: {}
- type: mssql_create_select_permission
  args:
    source: *sourceName
    role: role2
    table:
      schema: *schemaName
      name: track
    permission:
      columns: '*'
      filter: {}
- type: mssql_create_remote_relationship
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
-- LHS SQLite

lhsSqliteSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO API.DatasetCloneName
lhsSqliteSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
  let cloneName = API.DatasetCloneName $ tshow (uniqueTestId testEnvironment) <> "-lhs"
  let lhsSourceName_ = "source"
  let sourceName = Text.unpack lhsSourceName_
  let sqliteLhsTableName = J.toJSON ["main" :: Text, "track"]

  (API.Config sourceConfig) <- Sqlite.createEmptyDatasetCloneSourceConfig cloneName

  -- Add remote source
  Schema.addSource lhsSourceName_ (J.Object sourceConfig) testEnvironment

  -- Setup tables
  Sqlite.createTable sourceName testEnvironment track
  Sqlite.insertTable sourceName testEnvironment track
  Sqlite.trackTable sourceName testEnvironment track

  -- Setup permissions and remote relationship
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: sqlite_create_select_permission
        args:
          source: *sourceName
          role: role1
          table: *sqliteLhsTableName
          permission:
            columns: '*'
            filter: {}
      - type: sqlite_create_select_permission
        args:
          source: *sourceName
          role: role2
          table: *sqliteLhsTableName
          permission:
            columns: '*'
            filter: {}
      - type: sqlite_create_remote_relationship
        args:
          source: *sourceName
          table: *sqliteLhsTableName
          name: album
          definition:
            to_source:
              source: target
              table: *rhsTableName
              relationship_type: object
              field_mapping:
                album_id: id
    |]

  pure cloneName

sqliteTeardown :: Maybe API.DatasetCloneName -> IO ()
sqliteTeardown cloneName = do
  traverse_ Sqlite.deleteDatasetClone cloneName

--------------------------------------------------------------------------------
-- LHS Remote Server

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

instance (Typeable m) => Morpheus.GQLType (Query m)

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
    t_title :: m (Maybe Text),
    t_album_id :: m (Maybe Int)
  }
  deriving (Generic)

instance (Typeable m) => Morpheus.GQLType (HasuraTrack m) where
  typeOptions _ _ = hasuraTypeOptions

data HasuraTrackOrderBy = HasuraTrackOrderBy
  { tob_id :: Maybe OrderType,
    tob_title :: Maybe OrderType,
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
    tbe_title :: Maybe StringCompExp,
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
      pure
        $ tracks
        & filter filterFunction
        & sortBy orderByFunction
        & limitFunction
        & map mkTrack
    -- Returns True iif the given track matches the given boolean expression.
    matchTrack trackInfo@(trackId, trackTitle, maybeAlbumId) (HasuraTrackBoolExp {..}) =
      and
        [ all (all (matchTrack trackInfo)) tbe__and,
          all (any (matchTrack trackInfo)) tbe__or,
          not (any (matchTrack trackInfo) tbe__not),
          all (matchInt trackId) tbe_id,
          all (matchString trackTitle) tbe_title,
          all (matchMaybeInt maybeAlbumId) tbe_album_id
        ]
    matchInt intField IntCompExp {..} = Just intField == _eq
    matchString stringField StringCompExp {..} = Just stringField == _eq
    matchMaybeInt maybeIntField IntCompExp {..} = maybeIntField == _eq
    -- Returns an ordering between the two given tracks.
    orderTrack
      orderByList
      (trackId1, trackTitle1, trackAlbumId1)
      (trackId2, trackTitle2, trackAlbumId2) =
        flip foldMap orderByList \HasuraTrackOrderBy {..} ->
          if
            | Just idOrder <- tob_id -> case idOrder of
                Asc -> compare trackId1 trackId2
                Desc -> compare trackId2 trackId1
            | Just titleOrder <- tob_title -> case titleOrder of
                Asc -> compare trackTitle1 trackTitle2
                Desc -> compare trackTitle2 trackTitle1
            | Just albumIdOrder <- tob_album_id ->
                compareWithNullLast albumIdOrder trackAlbumId1 trackAlbumId2
            | otherwise ->
                error "empty track_order object"
    compareWithNullLast Desc x1 x2 = compareWithNullLast Asc x2 x1
    compareWithNullLast Asc Nothing Nothing = EQ
    compareWithNullLast Asc (Just _) Nothing = LT
    compareWithNullLast Asc Nothing (Just _) = GT
    compareWithNullLast Asc (Just x1) (Just x2) = compare x1 x2
    tracks =
      [ (1, "track1_album1", Just 1),
        (2, "track2_album1", Just 1),
        (3, "track3_album1", Just 1),
        (4, "track1_album2", Just 2),
        (5, "track2_album2", Just 2),
        (6, "track1_album3", Just 3),
        (7, "track2_album3", Just 3),
        (8, "track_no_album", Nothing)
      ]
    mkTrack (trackId, title, albumId) =
      HasuraTrack
        { t_id = pure $ Just trackId,
          t_title = pure $ Just title,
          t_album_id = pure albumId
        }

lhsRemoteServerSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
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
rhsPostgresSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceName = "target"
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
  Schema.trackTable sourceName album testEnvironment

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
      columns: [id, title, artist_id]
      filter:
        artist_id:
          _eq: x-hasura-artist-id
      limit: 1
      allow_aggregations: true
  |]

--------------------------------------------------------------------------------
-- RHS Citus

rhsCitusSetup :: (TestEnvironment, ()) -> IO ()
rhsCitusSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceName = "target"
      sourceConfig = Citus.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: citus_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  Citus.createTable testEnvironment album
  Citus.insertTable testEnvironment album
  Schema.trackTable sourceName album testEnvironment

  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: citus_create_select_permission
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
      filter:
        artist_id:
          _eq: x-hasura-artist-id
- type: citus_create_select_permission
  args:
    source: *sourceName
    role: role2
    table:
      schema: *schemaName
      name: album
    permission:
      columns: [id, title, artist_id]
      filter:
        artist_id:
          _eq: x-hasura-artist-id
      limit: 1
      allow_aggregations: true
  |]

--------------------------------------------------------------------------------
-- RHS Cockroach

rhsCockroachSetup :: (TestEnvironment, ()) -> IO ()
rhsCockroachSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceName = "target"
      sourceConfig = Cockroach.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: cockroach_add_source
      args:
        name: *sourceName
        configuration: *sourceConfig
    |]
  -- setup tables only
  Cockroach.createTable testEnvironment album
  Cockroach.insertTable testEnvironment album
  Schema.trackTable sourceName album testEnvironment

  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: cockroach_create_select_permission
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
            filter:
              artist_id:
                _eq: x-hasura-artist-id
      - type: cockroach_create_select_permission
        args:
          source: *sourceName
          role: role2
          table:
            schema: *schemaName
            name: album
          permission:
            columns: [id, title, artist_id]
            filter:
              artist_id:
                _eq: x-hasura-artist-id
            limit: 1
            allow_aggregations: true
    |]

--------------------------------------------------------------------------------
-- RHS SQLServer

rhsSQLServerSetup :: (TestEnvironment, ()) -> IO ()
rhsSQLServerSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceName = "target"
      sourceConfig = SQLServer.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: mssql_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  SQLServer.createTable testEnvironment album
  SQLServer.insertTable testEnvironment album
  Schema.trackTable sourceName album testEnvironment

  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: mssql_create_select_permission
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
      filter:
        artist_id:
          _eq: x-hasura-artist-id
- type: mssql_create_select_permission
  args:
    source: *sourceName
    role: role2
    table:
      schema: *schemaName
      name: album
    permission:
      columns: [id, title, artist_id]
      filter:
        artist_id:
          _eq: x-hasura-artist-id
      limit: 1
      allow_aggregations: true
  |]

--------------------------------------------------------------------------------
-- RHS SQLite

rhsSqliteSetup :: (TestEnvironment, ()) -> IO API.DatasetCloneName
rhsSqliteSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
  let cloneName = API.DatasetCloneName $ tshow (uniqueTestId testEnvironment) <> "-rhs"
  let rhsSourceName_ = "target"
  let sourceName = Text.unpack rhsSourceName_
  let sqliteRhsTableName = J.toJSON ["main" :: Text, "album"]

  (API.Config sourceConfig) <- Sqlite.createEmptyDatasetCloneSourceConfig cloneName

  -- Add remote source
  Schema.addSource rhsSourceName_ (J.Object sourceConfig) testEnvironment

  -- Setup tables
  Sqlite.createTable sourceName testEnvironment album
  Sqlite.insertTable sourceName testEnvironment album
  Sqlite.trackTable sourceName testEnvironment album

  -- Setup permissions
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: sqlite_create_select_permission
        args:
          source: *sourceName
          role: role1
          table: *sqliteRhsTableName
          permission:
            columns:
              - title
              - artist_id
            filter:
              artist_id:
                _eq: x-hasura-artist-id
      - type: sqlite_create_select_permission
        args:
          source: *sourceName
          role: role2
          table: *sqliteRhsTableName
          permission:
            columns: [id, title, artist_id]
            filter:
              artist_id:
                _eq: x-hasura-artist-id
            limit: 1
            allow_aggregations: true
    |]

  pure cloneName

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Maybe Server)
tests = describe "object-relationship" $ do
  schemaTests
  executionTests
  permissionTests

-- | Basic queries using *-to-DB joins
executionTests :: SpecWith (TestEnvironment, Maybe Server)
executionTests = describe "execution" $ do
  -- fetches the relationship data
  it "related-data" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let query =
          [graphql|
          query {
            track: #{lhsSchema}_track(where: {title: {_eq: "track1_album1"}}) {
              title
              album {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "track1_album1"
               album:
                 title: album1_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when any of the join columns are null, the relationship should be null
  it "related-data-null" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let query =
          [graphql|
          query {
            track: #{lhsSchema}_track(where: {title: {_eq: "track_no_album"}}) {
              title
              album {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "track_no_album"
               album: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when the lhs response has both null and non-null values for join columns
  it "related-data-non-null-and-null" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let query =
          [graphql|
          query {
            track: #{lhsSchema}_track(
              where: {
                _or: [
                  {title: {_eq: "track1_album1"}},
                  {title: {_eq: "track_no_album"}}
                ]
              },
              order_by: [{id: asc}]
            ) {
              title
              album {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "track1_album1"
               album:
                 title: album1_artist1
             - title: "track_no_album"
               album: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

-- | Spec that describe an object relationship's data in the presence of
-- permisisons.
permissionTests :: SpecWith (TestEnvironment, Maybe Server)
permissionTests = describe "permission" $ do
  let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]

  -- only the allowed rows on the target table are queryable
  it "only-allowed-rows" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let query =
          [graphql|
          query {
            track: #{lhsSchema}_track(
              order_by: [{id: asc}]
            ) {
              title
              album {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "track1_album1"
               album:
                 title: album1_artist1
             - title: "track2_album1"
               album:
                 title: album1_artist1
             - title: "track3_album1"
               album:
                 title: album1_artist1
             - title: "track1_album2"
               album:
                 title: album2_artist1
             - title: "track2_album2"
               album:
                 title: album2_artist1
             - title: "track1_album3"
               album: null
             - title: "track2_album3"
               album: null
             - title: "track_no_album"
               album: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- we use an introspection query to check column permissions:
  -- 1. the type 'hasura_album' has only 'artist_id' and 'title', the allowed columns
  -- 2. the album field in 'hasura_track' type is of type 'hasura_album'
  it "only-allowed-columns" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let rhsSchema = Schema.getSchemaName $ focusFixtureRight testEnvironment
    let query =
          [graphql|
          query {
            album_fields: __type(name: "#{rhsSchema}_album") {
              fields {
                name
              }
            }
            track: #{lhsSchema}_track(where: {title: {_eq: "track1_album1"}}) {
              title
              album {
                __typename
              }
            }
          }
          |]
        expectedResponse =
          [interpolateYaml|
          data:
            album_fields:
              fields:
              - name: artist_id
              - name: title
            track:
            - title: track1_album1
              album:
                __typename: #{rhsSchema}_album
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

schemaTests :: SpecWith (TestEnvironment, Maybe Server)
schemaTests =
  -- we use an introspection query to check:
  -- 1. a field 'album' is added to the track table
  -- 1. track's where clause does not have 'album' field
  -- 2. track's order_by clause does nat have 'album' field
  it "graphql-schema" $ \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let query =
          [graphql|
          query {
            track_fields: __type(name: "#{lhsSchema}_track") {
              fields {
                name
              }
            }
            track_where_exp_fields: __type(name: "#{lhsSchema}_track_bool_exp") {
              inputFields {
                name
              }
            }
            track_order_by_exp_fields: __type(name: "#{lhsSchema}_track_order_by") {
              inputFields {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track_fields:
              fields:
              - name: album
              - name: album_id
              - name: id
              - name: title
            track_where_exp_fields:
              inputFields:
              - name: _and
              - name: _not
              - name: _or
              - name: album_id
              - name: id
              - name: title
            track_order_by_exp_fields:
              inputFields:
              - name: album_id
              - name: id
              - name: title
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
