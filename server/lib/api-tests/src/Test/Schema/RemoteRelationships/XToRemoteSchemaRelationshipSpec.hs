{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Tests for remote relationships to remote schemas. Remote relationships are
-- relationships that are not local to a given source or remote schema, and are
-- handled by the engine itself.
--
-- All tests use the same GraphQL syntax, and the only difference is in the
-- setup: for each left-hand side source we support we do a custom setup and run
-- the tests.
module Test.Schema.RemoteRelationships.XToRemoteSchemaRelationshipSpec (spec) where

import Data.Aeson qualified as J
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
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
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.SetupAction (SetupAction (..))
import Harness.Test.SetupAction qualified as SetupAction
import Harness.Test.TestResource (Managed)
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment (..), stopServer)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    contexts =
      NE.fromList
        $ map
          mkFixture
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.mkLocalTestEnvironment = lhsPostgresMkLocalTestEnvironment,
                Fixture.setupTeardown = \(testEnv, _localEnv) ->
                  [lhsPostgresSetupAction testEnv]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.mkLocalTestEnvironment = lhsCitusMkLocalTestEnvironment,
                Fixture.setupTeardown = \(testEnv, _localEnv) ->
                  [lhsCitusSetupAction testEnv]
              },
            (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.mkLocalTestEnvironment = lhsCockroachMkLocalTestEnvironment,
                Fixture.setupTeardown = \(testEnv, _localEnv) ->
                  [lhsCockroachSetupAction testEnv],
                Fixture.customOptions = Nothing
              },
            (Fixture.fixture $ Fixture.Backend SQLServer.backendTypeMetadata)
              { Fixture.mkLocalTestEnvironment = lhsSQLServerMkLocalTestEnvironment,
                Fixture.setupTeardown = \(testEnv, _localEnv) ->
                  [lhsSQLServerSetupAction testEnv]
              },
            (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
              { Fixture.mkLocalTestEnvironment = \_ -> pure Nothing,
                Fixture.setupTeardown = \(testEnv, _localEnv) ->
                  [ SetupAction (lhsSqliteSetup testEnv) sqliteTeardown
                  ]
              },
            (Fixture.fixture $ Fixture.RemoteGraphQLServer)
              { Fixture.mkLocalTestEnvironment = lhsRemoteServerMkLocalTestEnvironment,
                Fixture.setupTeardown = \(testEnv, localEnv) ->
                  [lhsRemoteServerSetupAction (testEnv, localEnv)]
              }
          ]

-- | Uses a given RHS context to create a combined context.
mkFixture :: Fixture.Fixture (Maybe Server) -> Fixture.Fixture LocalTestTestEnvironment
mkFixture lhs =
  Fixture.Fixture
    { Fixture.name = lhsName,
      Fixture.mkLocalTestEnvironment = \testEnvironment -> do
        rhsServer <- rhsRemoteServerMkLocalTestEnvironment testEnvironment
        lhsServer <- lhsMkLocalTestEnvironment testEnvironment
        pure $ LocalTestTestEnvironment lhsServer rhsServer,
      Fixture.setupTeardown = \(testEnvironment, LocalTestTestEnvironment lhsServer rhsServer) ->
        [ clearMetadataSetupAction testEnvironment,
          setupSqliteAgentAction testEnvironment,
          rhsRemoteSchemaSetupAction (testEnvironment, rhsServer)
        ]
          <> lhsSetupTeardown (testEnvironment, lhsServer),
      Fixture.customOptions = lhsOptions
    }
  where
    Fixture.Fixture
      { name = lhsName,
        mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
        setupTeardown = lhsSetupTeardown,
        customOptions = lhsOptions
      } = lhs
    setupSqliteAgentAction testEnv = SetupAction.noTeardown (Sqlite.setupSqliteAgent testEnv)

-- | Local test testEnvironment.
data LocalTestTestEnvironment = LocalTestTestEnvironment
  { _lhsServer :: Maybe Server,
    _rhsServer :: Server
  }

--------------------------------------------------------------------------------
-- Schema

-- | LHS
track :: Schema.Table
track =
  (table "track")
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

-- RHS schema is defined by the @gqlDocument@

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresSetupAction :: TestEnvironment -> Fixture.SetupAction
lhsPostgresSetupAction testEnv =
  SetupAction.noTeardown
    (lhsPostgresSetup (testEnv, Nothing))

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup (testEnvironment, _) = do
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
  Schema.trackTable sourceName track testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: pg_create_remote_relationship
  args:
    source: source
    table:
      schema: *schemaName
      name: track
    name: album
    definition:
      to_remote_schema:
        remote_schema: target
        lhs_fields: [album_id]
        remote_field:
          album:
            arguments:
              album_id: $album_id
  |]

--------------------------------------------------------------------------------
-- LHS Citus

lhsCitusSetupAction :: TestEnvironment -> Fixture.SetupAction
lhsCitusSetupAction testEnv =
  SetupAction.noTeardown
    (lhsCitusSetup (testEnv, Nothing))

lhsCitusMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsCitusMkLocalTestEnvironment _ = pure Nothing

lhsCitusSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsCitusSetup (testEnvironment, _) = do
  let sourceName = "source"
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
- type: citus_create_remote_relationship
  args:
    source: source
    table:
      schema: *schemaName
      name: track
    name: album
    definition:
      to_remote_schema:
        remote_schema: target
        lhs_fields: [album_id]
        remote_field:
          album:
            arguments:
              album_id: $album_id
  |]

--------------------------------------------------------------------------------
-- LHS Cockroach

lhsCockroachSetupAction :: TestEnvironment -> Fixture.SetupAction
lhsCockroachSetupAction testEnv =
  SetupAction.noTeardown
    (lhsCockroachSetup (testEnv, Nothing))

lhsCockroachMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsCockroachMkLocalTestEnvironment _ = pure Nothing

lhsCockroachSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsCockroachSetup (testEnvironment, _) = do
  let sourceName = "source"
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
      - type: cockroach_create_remote_relationship
        args:
          source: source
          table:
            schema: *schemaName
            name: track
          name: album
          definition:
            to_remote_schema:
              remote_schema: target
              lhs_fields: [album_id]
              remote_field:
                album:
                  arguments:
                    album_id: $album_id
    |]

--------------------------------------------------------------------------------
-- LHS SQLServer

lhsSQLServerMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsSQLServerMkLocalTestEnvironment _ = pure Nothing

lhsSQLServerSetupAction :: TestEnvironment -> Fixture.SetupAction
lhsSQLServerSetupAction testEnv =
  SetupAction.noTeardown
    (lhsSQLServerSetup (testEnv, Nothing))

lhsSQLServerSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerSetup (testEnvironment, _) = do
  let sourceName = "source"
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
- type: mssql_create_remote_relationship
  args:
    source: source
    table:
      schema: *schemaName
      name: track
    name: album
    definition:
      to_remote_schema:
        remote_schema: target
        lhs_fields: [album_id]
        remote_field:
          album:
            arguments:
              album_id: $album_id
  |]

--------------------------------------------------------------------------------
-- LHS SQLite

lhsSqliteSetup :: TestEnvironment -> IO API.DatasetCloneName
lhsSqliteSetup testEnvironment = do
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

  -- Setup permissions
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
      type: bulk
      args:
      - type: sqlite_create_remote_relationship
        args:
          source: source
          table: *sqliteLhsTableName
          name: album
          definition:
            to_remote_schema:
              remote_schema: target
              lhs_fields: [album_id]
              remote_field:
                album:
                  arguments:
                    album_id: $album_id
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
      -- transformation to apply to type names; we remove the leading "LHS" we
      -- use to differentiate those types from the RHS ones, split the name on
      -- uppercase letters, intercalate with underscore, and map everything to
      -- lowercase: LHSHasuraTrack -> hasura_track
      Morpheus.typeNameModifier = \_ ->
        map toLower
          . intercalate "_"
          . split (dropBlanks $ keepDelimsL $ whenElt isUpper)
          . drop 3
    }

data LHSQuery m = LHSQuery
  { q_hasura_track :: LHSHasuraTrackArgs -> m [LHSHasuraTrack m]
  }
  deriving (Generic)

instance (Typeable m) => Morpheus.GQLType (LHSQuery m) where
  typeOptions _ _ = hasuraTypeOptions

data LHSHasuraTrackArgs = LHSHasuraTrackArgs
  { ta_where :: Maybe LHSHasuraTrackBoolExp,
    ta_order_by :: Maybe [LHSHasuraTrackOrderBy],
    ta_limit :: Maybe Int
  }
  deriving (Generic)

instance Morpheus.GQLType LHSHasuraTrackArgs where
  typeOptions _ _ = hasuraTypeOptions

data LHSHasuraTrack m = LHSHasuraTrack
  { t_id :: m (Maybe Int),
    t_title :: m (Maybe Text),
    t_album_id :: m (Maybe Int)
  }
  deriving (Generic)

instance (Typeable m) => Morpheus.GQLType (LHSHasuraTrack m) where
  typeOptions _ _ = hasuraTypeOptions

data LHSHasuraTrackOrderBy = LHSHasuraTrackOrderBy
  { tob_id :: Maybe LHSOrderType,
    tob_title :: Maybe LHSOrderType,
    tob_album_id :: Maybe LHSOrderType
  }
  deriving (Generic)

instance Morpheus.GQLType LHSHasuraTrackOrderBy where
  typeOptions _ _ = hasuraTypeOptions

data LHSHasuraTrackBoolExp = LHSHasuraTrackBoolExp
  { tbe__and :: Maybe [LHSHasuraTrackBoolExp],
    tbe__or :: Maybe [LHSHasuraTrackBoolExp],
    tbe__not :: Maybe LHSHasuraTrackBoolExp,
    tbe_id :: Maybe IntCompExp,
    tbe_title :: Maybe StringCompExp,
    tbe_album_id :: Maybe IntCompExp
  }
  deriving (Generic)

instance Morpheus.GQLType LHSHasuraTrackBoolExp where
  typeOptions _ _ = hasuraTypeOptions

data LHSOrderType = Asc | Desc
  deriving (Show, Generic)

instance Morpheus.GQLType LHSOrderType where
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
  Just <$> RemoteServer.run (RemoteServer.generateQueryInterpreter (LHSQuery {q_hasura_track = hasura_track}))
  where
    -- Implements the @hasura_track@ field of the @Query@ type.
    hasura_track (LHSHasuraTrackArgs {..}) = do
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
    matchTrack trackInfo@(trackId, trackTitle, maybeAlbumId) (LHSHasuraTrackBoolExp {..}) =
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
        flip foldMap orderByList \LHSHasuraTrackOrderBy {..} ->
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
      LHSHasuraTrack
        { t_id = pure $ Just trackId,
          t_title = pure $ Just title,
          t_album_id = pure albumId
        }

lhsRemoteServerSetupAction :: (TestEnvironment, Maybe Server) -> Fixture.SetupAction
lhsRemoteServerSetupAction (testEnv, server) =
  Fixture.SetupAction
    (lhsRemoteServerSetup (testEnv, server))
    (const $ lhsRemoteServerTeardown (testEnv, server))

lhsRemoteServerSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerSetup (testEnvironment, maybeRemoteServer) = case maybeRemoteServer of
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
      to_remote_schema:
        remote_schema: target
        lhs_fields: [album_id]
        remote_field:
          album:
            arguments:
              album_id: $album_id
      |]

lhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

--------------------------------------------------------------------------------
-- RHS Remote Server

[gqlDocument|

type Album {
  id: Int!
  title: String!
  artist_id: Int
}

type Query {
  album(album_id: Int!): Album
}

|]

rhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> Managed Server
rhsRemoteServerMkLocalTestEnvironment _ =
  RemoteServer.run $ RemoteServer.generateQueryInterpreter (Query {album})
  where
    albums =
      [ (1, ("album1_artist1", Just 1)),
        (2, ("album2_artist1", Just 1)),
        (3, ("album3_artist2", Just 2))
      ]
    album (Arg albumId) = pure $ mkAlbum albumId <$> lookup albumId albums
    mkAlbum albumId (title, artist_id) =
      Album
        { id = pure albumId,
          title = pure title,
          artist_id = pure artist_id
        }

rhsRemoteSchemaSetupAction :: (TestEnvironment, Server) -> Fixture.SetupAction
rhsRemoteSchemaSetupAction (testEnv, server) =
  Fixture.SetupAction
    (rhsRemoteSchemaSetup (testEnv, server))
    ( const $ rhsRemoteSchemaTeardown (testEnv, server)
    )

clearMetadataSetupAction :: TestEnvironment -> Fixture.SetupAction
clearMetadataSetupAction testEnv =
  Fixture.SetupAction
    (pure ())
    (const $ GraphqlEngine.clearMetadata testEnv)

rhsRemoteSchemaSetup :: (TestEnvironment, Server) -> IO ()
rhsRemoteSchemaSetup (testEnvironment, remoteServer) = do
  let remoteSchemaEndpoint = GraphqlEngine.serverUrl remoteServer ++ "/graphql"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: add_remote_schema
args:
  name: target
  definition:
    url: *remoteSchemaEndpoint
  |]

rhsRemoteSchemaTeardown :: (TestEnvironment, Server) -> IO ()
rhsRemoteSchemaTeardown (_, server) = stopServer server

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, LocalTestTestEnvironment)
tests = describe "remote-schema-relationship" do
  schemaTests
  executionTests

-- | Basic queries using *-to-DB joins
executionTests :: SpecWith (TestEnvironment, LocalTestTestEnvironment)
executionTests = describe "execution" do
  -- fetches the relationship data
  it "related-data" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName testEnvironment
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
  it "related-data-null" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName testEnvironment
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
  it "related-data-non-null-and-null" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName testEnvironment
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

schemaTests :: SpecWith (TestEnvironment, LocalTestTestEnvironment)
schemaTests =
  -- we use an introspection query to check:
  -- 1. a field 'album' is added to the track table
  -- 1. track's where clause does not have 'album' field
  -- 2. track's order_by clause does nat have 'album' field
  it "graphql-schema" \(testEnvironment, _) -> do
    let lhsSchema = Schema.getSchemaName testEnvironment
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
