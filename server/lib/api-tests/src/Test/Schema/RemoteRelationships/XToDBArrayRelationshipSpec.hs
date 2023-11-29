{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Tests for array remote relationships to databases. Remote relationships are
-- relationships that are not local to a given source or remote schema, and are
-- handled by the engine itself. Array relationsips are 1-to-many relationships.
--
-- All tests use the same GraphQL syntax, and the only difference is in the
-- setup: we do a cartesian product of all kinds of sources we support on the
-- left-hand side and all databases we support on the right-hand side.
module Test.Schema.RemoteRelationships.XToDBArrayRelationshipSpec (spec) where

import Control.Lens (findOf, has, only, (^?!))
import Data.Aeson (Value (..))
import Data.Aeson qualified as J
import Data.Aeson.Lens (key, values, _String)
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Maybe qualified as Unsafe (fromJust)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types qualified as Morpheus
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Permissions (SelectPermissionDetails (..))
import Harness.Permissions qualified as Permissions
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml, yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Schema (Table (..))
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture (LHSFixture, RHSFixture, SetupAction (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction qualified as SetupAction
import Harness.Test.TestResource (Managed)
import Harness.TestEnvironment
  ( GlobalTestEnvironment,
    Server,
    TestEnvironment (..),
    focusFixtureLeft,
    focusFixtureRight,
    getBackendTypeConfig,
    stopServer,
  )
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (HasCallStack, SpecWith, describe, it)

-------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment fixtures tests
  where
    lhsFixtures = [lhsPostgres, lhsCockroach, lhsSQLServer, lhsCitus, lhsSqlite, lhsRemoteServer]
    rhsFixtures = [rhsPostgres, rhsCockroach, rhsSQLServer, rhsCitus, rhsSqlite]
    fixtures = NE.fromList $ Fixture.combineFixtures [setupSqliteAgentAction] <$> lhsFixtures <*> rhsFixtures
    setupSqliteAgentAction testEnv = SetupAction.noTeardown (Sqlite.setupSqliteAgent testEnv)

--------------------------------------------------------------------------------
-- Fixtures

-- | Left-hand-side (LHS) fixtures
lhsPostgres :: LHSFixture
lhsPostgres tableName =
  (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsPostgresSetup tableName testEnv)
        ]
    }

lhsCitus :: LHSFixture
lhsCitus tableName =
  (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsCitusSetup tableName testEnv)
        ]
    }

lhsCockroach :: LHSFixture
lhsCockroach tableName =
  (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsCockroachSetup tableName testEnv)
        ],
      Fixture.customOptions =
        Just
          Fixture.defaultOptions
            { Fixture.skipTests = Just "NDAT-177"
            }
    }

lhsSQLServer :: LHSFixture
lhsSQLServer tableName =
  (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction.noTeardown (lhsSQLServerSetup tableName testEnv)
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

lhsSqlite :: LHSFixture
lhsSqlite tableName =
  (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
    { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
      Fixture.setupTeardown = \testEnv ->
        [ SetupAction (lhsSqliteSetup tableName testEnv) sqliteTeardown
        ]
    }

-- | Right-hand-side (RHS) fixtures
rhsPostgres :: RHSFixture
rhsPostgres =
  let fixture =
        (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
          { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
            Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsPostgresSetup testEnv)
              ]
          }
   in (rhsTable, fixture)

rhsCockroach :: RHSFixture
rhsCockroach =
  let fixture =
        (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
          { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
            Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsCockroachSetup testEnv)
              ],
            Fixture.customOptions =
              Just
                Fixture.defaultOptions
                  { Fixture.skipTests = Just "NDAT-177"
                  }
          }
   in (rhsTable, fixture)

rhsCitus :: RHSFixture
rhsCitus =
  let fixture =
        (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
          { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
            Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsCitusSetup testEnv)
              ]
          }
   in (rhsTable, fixture)

rhsSQLServer :: RHSFixture
rhsSQLServer =
  let fixture =
        (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
          { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
            Fixture.setupTeardown = \testEnv ->
              [ SetupAction.noTeardown (rhsSQLServerSetup testEnv)
              ]
          }
   in (rhsTable, fixture)

rhsSqlite :: RHSFixture
rhsSqlite =
  let sqliteRhsTableName = J.toJSON ["main", rhsTableName_]
      fixture =
        (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
          { Fixture.mkLocalTestEnvironment = Fixture.noLocalTestEnvironment,
            Fixture.setupTeardown = \testEnv ->
              [ SetupAction (rhsSqliteSetup testEnv) sqliteTeardown
              ]
          }
   in (sqliteRhsTableName, fixture)

--------------------------------------------------------------------------------
-- Schema

-- | LHS
artist :: Schema.Table
artist =
  (Schema.table lhsTableName_)
    { tableColumns =
        [ Schema.columnNull "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "artist1"],
          [Schema.VInt 2, Schema.VStr "artist2"],
          [Schema.VInt 3, Schema.VStr "artist3_no_albums"],
          [Schema.VNull, Schema.VStr "artist4_no_id"]
        ]
    }

lhsSourceName_ :: Text
lhsSourceName_ = "source"

lhsTableName_ :: Text
lhsTableName_ = "artist"

lhsRole1 :: Permissions.Permission
lhsRole1 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionRole = "role1",
        selectPermissionTable = lhsTableName_,
        selectPermissionColumns = (["id", "name"] :: [Text]),
        selectPermissionSource = Just lhsSourceName_
      }

lhsRole2 :: Permissions.Permission
lhsRole2 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionRole = "role2",
        selectPermissionTable = lhsTableName_,
        selectPermissionColumns = (["id", "name"] :: [Text]),
        selectPermissionSource = Just lhsSourceName_
      }

createRemoteRelationship :: (HasCallStack) => Value -> Value -> TestEnvironment -> IO ()
createRemoteRelationship lhsTableName rhsTableName testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
      backendType = BackendType.backendTypeString backendTypeMetadata
  GraphqlEngine.postMetadata_
    testEnvironment
    [interpolateYaml|
        type: #{ backendType }_create_remote_relationship
        args:
          source: #{ lhsSourceName_ }
          table: #{ lhsTableName }
          name: albums
          definition:
            to_source:
              source: #{ rhsSourceName_ }
              table: #{ rhsTableName }
              relationship_type: array
              field_mapping:
                id: artist_id
    |]

-- | RHS
album :: Schema.Table
album =
  (Schema.table rhsTableName_)
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.columnNull "artist_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "album1_artist1", Schema.VInt 1],
          [Schema.VInt 2, Schema.VStr "album2_artist1", Schema.VInt 1],
          [Schema.VInt 3, Schema.VStr "album3_artist1", Schema.VInt 1],
          [Schema.VInt 4, Schema.VStr "album4_artist2", Schema.VInt 2]
        ]
    }

rhsSourceName_ :: Text
rhsSourceName_ = "target"

rhsTableName_ :: Text
rhsTableName_ = "album"

rhsRole1 :: Permissions.Permission
rhsRole1 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionRole = "role1",
        selectPermissionTable = rhsTableName_,
        selectPermissionColumns = (["title", "artist_id"] :: [Text]),
        selectPermissionRows =
          [yaml|
        artist_id:
          _eq: x-hasura-artist-id
      |],
        selectPermissionSource = Just rhsSourceName_
      }

rhsRole2 :: Permissions.Permission
rhsRole2 =
  Permissions.SelectPermission
    Permissions.selectPermission
      { selectPermissionRole = "role2",
        selectPermissionTable = rhsTableName_,
        selectPermissionColumns = (["id", "title", "artist_id"] :: [Text]),
        selectPermissionAllowAggregations = True,
        selectPermissionLimit = J.Number 2,
        selectPermissionRows =
          [yaml|
        artist_id:
          _eq: x-hasura-artist-id
      |],
        selectPermissionSource = Just rhsSourceName_
      }

mkLhsTable :: Schema.SchemaName -> J.Value
mkLhsTable (Schema.SchemaName schemaName) =
  [yaml|
    schema: *schemaName
    name: *lhsTableName_
  |]

rhsTable :: J.Value
rhsTable =
  [yaml|
    schema: hasura
    name: *rhsTableName_
  |]

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  Schema.addSource lhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  Postgres.createTable testEnvironment artist
  Postgres.insertTable testEnvironment artist
  Schema.trackTable (Text.unpack lhsSourceName_) artist testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole2

  createRemoteRelationship (mkLhsTable schemaName) rhsTableName testEnvironment

--------------------------------------------------------------------------------
-- LHS Cockroach

lhsCockroachSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsCockroachSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceConfig = Cockroach.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  -- TODO(SOLOMON): Remove BackendTypeConfig param from all of these functions
  Schema.addSource lhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  Cockroach.createTable testEnvironment artist
  Cockroach.insertTable testEnvironment artist
  Schema.trackTable (Text.unpack lhsSourceName_) artist testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole2

  createRemoteRelationship (mkLhsTable schemaName) rhsTableName testEnvironment

--------------------------------------------------------------------------------
-- LHS Citus

lhsCitusSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsCitusSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceConfig = Citus.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  Schema.addSource lhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  Citus.createTable testEnvironment artist
  Citus.insertTable testEnvironment artist
  Schema.trackTable (Text.unpack lhsSourceName_) artist testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole2

  createRemoteRelationship (mkLhsTable schemaName) rhsTableName testEnvironment

--------------------------------------------------------------------------------
-- LHS SQLServer

lhsSQLServerSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
      sourceConfig = SQLServer.defaultSourceConfiguration testEnvironment
      schemaName = Schema.getSchemaName testEnvironment

  -- Add remote source
  Schema.addSource lhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  SQLServer.createTable testEnvironment artist
  SQLServer.insertTable testEnvironment artist
  Schema.trackTable (Text.unpack lhsSourceName_) artist testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole2

  createRemoteRelationship (mkLhsTable schemaName) rhsTableName testEnvironment

--------------------------------------------------------------------------------
-- LHS SQLite

lhsSqliteSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO API.DatasetCloneName
lhsSqliteSetup rhsTableName (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureLeft wholeTestEnvironment
  let cloneName = API.DatasetCloneName $ tshow (uniqueTestId testEnvironment) <> "-lhs"
  let sourceName = Text.unpack lhsSourceName_

  (API.Config sourceConfig) <- Sqlite.createEmptyDatasetCloneSourceConfig cloneName

  -- Add remote source
  Schema.addSource lhsSourceName_ (J.Object sourceConfig) testEnvironment

  -- Setup tables
  Sqlite.createTable sourceName testEnvironment artist
  Sqlite.insertTable sourceName testEnvironment artist
  Sqlite.trackTable sourceName testEnvironment artist

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment lhsRole2

  let sqliteLhsTableName = J.toJSON ["main", lhsTableName_]
  createRemoteRelationship sqliteLhsTableName rhsTableName testEnvironment

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
  { hasura_artist :: HasuraArtistArgs -> m [HasuraArtist m]
  }
  deriving (Generic)

instance (Typeable m) => Morpheus.GQLType (Query m)

data HasuraArtistArgs = HasuraArtistArgs
  { aa_where :: Maybe HasuraArtistBoolExp,
    aa_order_by :: Maybe [HasuraArtistOrderBy],
    aa_limit :: Maybe Int
  }
  deriving (Generic)

instance Morpheus.GQLType HasuraArtistArgs where
  typeOptions _ _ = hasuraTypeOptions

data HasuraArtist m = HasuraArtist
  { a_id :: m (Maybe Int),
    a_name :: m (Maybe Text)
  }
  deriving (Generic)

instance (Typeable m) => Morpheus.GQLType (HasuraArtist m) where
  typeOptions _ _ = hasuraTypeOptions

data HasuraArtistOrderBy = HasuraArtistOrderBy
  { aob_id :: Maybe OrderType,
    aob_name :: Maybe OrderType
  }
  deriving (Generic)

instance Morpheus.GQLType HasuraArtistOrderBy where
  typeOptions _ _ = hasuraTypeOptions

data HasuraArtistBoolExp = HasuraArtistBoolExp
  { abe__and :: Maybe [HasuraArtistBoolExp],
    abe__or :: Maybe [HasuraArtistBoolExp],
    abe__not :: Maybe HasuraArtistBoolExp,
    abe_id :: Maybe IntCompExp,
    abe_name :: Maybe StringCompExp
  }
  deriving (Generic)

instance Morpheus.GQLType HasuraArtistBoolExp where
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
  Just <$> RemoteServer.run (RemoteServer.generateQueryInterpreter (Query {hasura_artist}))
  where
    -- Implements the @hasura_artist@ field of the @Query@ type.
    hasura_artist (HasuraArtistArgs {..}) = do
      let filterFunction = case aa_where of
            Nothing -> const True
            Just whereArg -> flip matchArtist whereArg
          orderByFunction = case aa_order_by of
            Nothing -> \_ _ -> EQ
            Just orderByArg -> orderArtist orderByArg
          limitFunction = maybe id take aa_limit
      pure
        $ artists
        & filter filterFunction
        & sortBy orderByFunction
        & limitFunction
        & map mkArtist
    -- Returns True iif the given artist matches the given boolean expression.
    matchArtist artistInfo@(artistId, artistName) (HasuraArtistBoolExp {..}) =
      and
        [ all (all (matchArtist artistInfo)) abe__and,
          all (any (matchArtist artistInfo)) abe__or,
          not (any (matchArtist artistInfo) abe__not),
          all (matchMaybeInt artistId) abe_id,
          all (matchString artistName) abe_name
        ]
    matchString stringField StringCompExp {..} = Just stringField == _eq
    matchMaybeInt maybeIntField IntCompExp {..} = maybeIntField == _eq
    -- Returns an ordering between the two given artists.
    orderArtist
      orderByList
      (artistId1, artistName1)
      (artistId2, artistName2) =
        flip foldMap orderByList \HasuraArtistOrderBy {..} ->
          if
            | Just idOrder <- aob_id ->
                compareWithNullLast idOrder artistId1 artistId2
            | Just nameOrder <- aob_name -> case nameOrder of
                Asc -> compare artistName1 artistName2
                Desc -> compare artistName2 artistName1
            | otherwise ->
                error "empty artist_order object"
    compareWithNullLast Desc x1 x2 = compareWithNullLast Asc x2 x1
    compareWithNullLast Asc Nothing Nothing = EQ
    compareWithNullLast Asc (Just _) Nothing = LT
    compareWithNullLast Asc Nothing (Just _) = GT
    compareWithNullLast Asc (Just x1) (Just x2) = compare x1 x2
    artists =
      [ (Just 1, "artist1"),
        (Just 2, "artist2"),
        (Just 3, "artist3_no_albums"),
        (Nothing, "artist4_no_id")
      ]
    mkArtist (artistId, artistName) =
      HasuraArtist
        { a_id = pure artistId,
          a_name = pure $ Just artistName
        }

lhsRemoteServerSetup :: (HasCallStack) => Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerSetup tableName (testEnvironment, maybeRemoteServer) = case maybeRemoteServer of
  Nothing -> error "XToDBArrayRelationshipSpec: remote server local testEnvironment did not succesfully create a server"
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
    type_name: hasura_artist
    name: albums
    definition:
      to_source:
        source: target
        table: *tableName
        relationship_type: array
        field_mapping:
          id: artist_id
      |]

lhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

--------------------------------------------------------------------------------
-- RHS Postgres

rhsPostgresSetup :: (TestEnvironment, ()) -> IO ()
rhsPostgresSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment

  -- Add remote source
  Schema.addSource rhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  Postgres.createTable testEnvironment album
  Postgres.insertTable testEnvironment album
  Schema.trackTable (Text.unpack rhsSourceName_) album testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole2

--------------------------------------------------------------------------------
-- RHS Cockroach

rhsCockroachSetup :: (TestEnvironment, ()) -> IO ()
rhsCockroachSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceConfig = Cockroach.defaultSourceConfiguration testEnvironment

  -- Add remote source
  Schema.addSource rhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  Cockroach.createTable testEnvironment album
  Cockroach.insertTable testEnvironment album
  Schema.trackTable (Text.unpack rhsSourceName_) album testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole2

--------------------------------------------------------------------------------
-- RHS Citus

rhsCitusSetup :: (TestEnvironment, ()) -> IO ()
rhsCitusSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceConfig = Citus.defaultSourceConfiguration testEnvironment

  -- Add remote source
  Schema.addSource rhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  Citus.createTable testEnvironment album
  Citus.insertTable testEnvironment album
  Schema.trackTable (Text.unpack rhsSourceName_) album testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole2

--------------------------------------------------------------------------------
-- RHS SQLServer

rhsSQLServerSetup :: (TestEnvironment, ()) -> IO ()
rhsSQLServerSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceConfig = SQLServer.defaultSourceConfiguration testEnvironment

  -- Add remote source
  Schema.addSource rhsSourceName_ sourceConfig testEnvironment

  -- Setup tables
  SQLServer.createTable testEnvironment album
  SQLServer.insertTable testEnvironment album
  Schema.trackTable (Text.unpack rhsSourceName_) album testEnvironment

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole2

--------------------------------------------------------------------------------
-- RHS SQLite

rhsSqliteSetup :: (TestEnvironment, ()) -> IO API.DatasetCloneName
rhsSqliteSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
  let cloneName = API.DatasetCloneName $ tshow (uniqueTestId testEnvironment) <> "-rhs"
  let sourceName = Text.unpack rhsSourceName_

  (API.Config sourceConfig) <- Sqlite.createEmptyDatasetCloneSourceConfig cloneName

  -- Add remote source
  Schema.addSource rhsSourceName_ (J.Object sourceConfig) testEnvironment

  -- Setup tables
  Sqlite.createTable sourceName testEnvironment album
  Sqlite.insertTable sourceName testEnvironment album
  Sqlite.trackTable sourceName testEnvironment album

  -- Setup permissions
  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole1

  GraphqlEngine.postMetadata_ testEnvironment do
    Permissions.createPermissionMetadata testEnvironment rhsRole2

  pure cloneName

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Maybe Server)
tests = describe "array-relationship" do
  schemaTests
  executionTests
  permissionTests

schemaTests :: SpecWith (TestEnvironment, Maybe Server)
schemaTests =
  -- we introspect the schema and validate it
  it "graphql-schema" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let rhsSchema = Schema.getSchemaName $ focusFixtureRight testEnvironment

    let query =
          [graphql|
          fragment type_info on __Type {
            name
            kind
            ofType {
              name
              kind
              ofType {
                name
                kind
                ofType {
                  name
                }
              }
            }
          }
          query {
            artist_fields: __type(name: "#{lhsSchema}_artist") {
              fields {
                name
                type {
                  ...type_info
                }
                args {
                  name
                  type {
                    ...type_info
                  }
                }
              }
            }
          }
          |]
    introspectionResult <- GraphqlEngine.postGraphql testEnvironment query
    let focusArtistFields =
          key "data"
            . key "artist_fields"
            . key "fields"
            . values
        albumsField =
          Unsafe.fromJust
            $ findOf
              focusArtistFields
              (has $ key "name" . _String . only "albums")
              introspectionResult
        albumsAggregateField =
          Unsafe.fromJust
            $ findOf
              focusArtistFields
              (has $ key "name" . _String . only "albums_aggregate")
              introspectionResult

    -- check the return type of albums field
    shouldBeYaml
      (albumsField ^?! key "type")
      [interpolateYaml|
      kind: NON_NULL
      name: null
      ofType:
        kind: LIST
        name: null
        ofType:
          kind: NON_NULL
          name: null
          ofType:
            name: #{rhsSchema}_album
      |]

    -- check the return type of albums_aggregate field
    shouldBeYaml
      (albumsAggregateField ^?! key "type")
      [interpolateYaml|
      name: null
      kind: NON_NULL
      ofType:
        name: #{rhsSchema}_album_aggregate
        kind: OBJECT
        ofType: null
      |]

-- | Basic queries using DB-to-DB joins
executionTests :: SpecWith (TestEnvironment, Maybe Server)
executionTests = describe "execution" do
  -- fetches the relationship data
  it "related-data" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(where: {name: {_eq: "artist1"}}) {
              name
              albums {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums:
               - title: album1_artist1
               - title: album2_artist1
               - title: album3_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when there are no matching rows, the relationship response should be []
  it "related-data-empty-array" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(where: {name: {_eq: "artist3_no_albums"}}) {
              name
              albums {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist3_no_albums
               albums: []
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when any of the join columns are null, the relationship should be null
  it "related-data-null" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(where: {name: {_eq: "artist4_no_id"}}) {
              name
              albums {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist4_no_id
               albums: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when the lhs response has both null and non-null values for join columns
  it "related-data-non-null-and-null" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              where: {
                _or: [
                  {name: {_eq: "artist1"}},
                  {name: {_eq: "artist4_no_id"}}
                ]
              },
              order_by: [{name: asc}]
            ) {
              name
              albums {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums:
               - title: album1_artist1
               - title: album2_artist1
               - title: album3_artist1
             - name: artist4_no_id
               albums: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

-- TODO:
-- 1. where
-- 1. limit with order_by
-- 1. offset
-- 1. _aggregate

-- | tests that describe an array relationship's data in the presence of permisisons
permissionTests :: SpecWith (TestEnvironment, Maybe Server)
permissionTests = describe "permission" do
  -- only the allowed rows on the target table are queryable
  it "only-allowed-rows" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              order_by: [{name: asc}]
            ) {
              name
              albums {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums:
               - title: album1_artist1
               - title: album2_artist1
               - title: album3_artist1
             - name: artist2
               albums: []
             - name: artist3_no_albums
               albums: []
             - name: artist4_no_id
               albums: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- we use an introspection query to check column permissions:
  -- 1. the type 'hasura_album' has only 'artist_id' and 'title', the allowed columns
  -- 2. the albums field in 'hasura_artist' type is of type 'hasura_album'
  it "only-allowed-columns" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment
    let rhsSchema = Schema.getSchemaName $ focusFixtureRight testEnvironment

    let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            album_fields: __type(name: "#{rhsSchema}_album") {
              fields {
                name
              }
            }
            artist: #{lhsSchema}_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums(limit: 1) {
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
            artist:
            - name: artist1
              albums:
              - __typename: #{rhsSchema}_album
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- _aggregate field should not be generated when 'allow_aggregations' isn't set to 'true'
  it "aggregations-not-allowed" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let userHeaders = [("x-hasura-role", "role1")]
        query =
          [graphql|
          query {
            artist_fields: __type(name: "#{lhsSchema}_artist") {
              fields {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist_fields:
              fields:
              - name: albums
              - name: id
              - name: name
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- _aggregate field should only be allowed when 'allow_aggregations' is set to 'true'
  it "aggregations-allowed" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

    let userHeaders = [("x-hasura-role", "role2")]
        query =
          [graphql|
          query {
            artist_fields: __type(name: "#{lhsSchema}_artist") {
              fields {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist_fields:
              fields:
              - name: albums
              - name: albums_aggregate
              - name: id
              - name: name
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- permission limit should kick in when no query limit is specified
  it "no-query-limit" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

        query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums (order_by: {id: asc}){
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums:
               - title: album1_artist1
               - title: album2_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- query limit should be applied when query limit <= permission limit
  it "user-limit-less-than-permission-limit" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

        query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums (order_by: {id: asc} limit: 1){
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums:
                 - title: album1_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- permission limit should be applied when query limit > permission limit
  it "user-limit-greater-than-permission-limit" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

        query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums (order_by: {id: asc} limit: 4){
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums:
               - title: album1_artist1
               - title: album2_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- permission limit should only apply on 'nodes' but not on 'aggregate'
  it "aggregations" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

        query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums_aggregate (order_by: {id: asc}){
                aggregate {
                  count
                }
                nodes {
                  title
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums_aggregate:
                 aggregate:
                   count: 3
                 nodes:
                 - title: album1_artist1
                 - title: album2_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- query limit applies to both 'aggregate' and 'nodes'
  it "aggregations-query-limit" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
    let lhsSchema = Schema.getSchemaName $ focusFixtureLeft testEnvironment

        query =
          [graphql|
          query {
            artist: #{lhsSchema}_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums_aggregate (limit: 2 order_by: {id: asc}){
                aggregate {
                  count
                }
                nodes {
                  title
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
             - name: artist1
               albums_aggregate:
                 aggregate:
                   count: 2
                 nodes:
                 - title: album1_artist1
                 - title: album2_artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse
