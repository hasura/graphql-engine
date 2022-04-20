{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for object remote relationships to databases. Remote relationships
-- are relationships that are not local to a given source or remote schema, and
-- are handled by the engine itself. Object relationsips are 1-to-1
-- relationships.
--
-- All tests use the same GraphQL syntax, and the only difference is in the
-- setup: we do a cartesian product of all kinds of sources we support on the
-- left-hand side and all databases we support on the right-hand side.
module Test.RemoteRelationship.XToDBObjectRelationshipSpec
  ( spec,
  )
where

import Data.Aeson (Value)
import Data.Char (isUpper, toLower)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (intercalate, sortBy)
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types qualified as Morpheus
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Test.Context (Context (..))
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (Server, TestEnvironment, stopServer)
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = Context.runWithLocalTestEnvironment contexts tests
  where
    lhsContexts = [lhsPostgres, lhsSQLServer, lhsRemoteServer]
    rhsContexts = [rhsPostgres, rhsSQLServer]
    contexts = combine <$> lhsContexts <*> rhsContexts

-- | Combines a lhs and a rhs.
--
-- The rhs is set up first, then the lhs can create the remote relationship.
--
-- Teardown is done in the opposite order.
--
-- The metadata is cleared befored each setup.
combine :: LHSContext -> RHSContext -> Context (Maybe Server)
combine lhs (tableName, rhs) =
  Context
    { name = Context.Combine lhsName rhsName,
      mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
      setup = \(testEnvironment, localTestEnvironment) -> do
        GraphqlEngine.clearMetadata testEnvironment
        rhsSetup (testEnvironment, ())
        lhsSetup (testEnvironment, localTestEnvironment),
      teardown = \testEnvironment@(globalTestEnvironment, _localTestEnvironment) -> do
        lhsTeardown testEnvironment
        rhsTeardown (globalTestEnvironment, ())
        GraphqlEngine.clearMetadata globalTestEnvironment,
      customOptions =
        Context.combineOptions lhsOptions rhsOptions
    }
  where
    Context
      { name = lhsName,
        mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
        setup = lhsSetup,
        teardown = lhsTeardown,
        customOptions = lhsOptions
      } = lhs tableName
    Context
      { name = rhsName,
        setup = rhsSetup,
        teardown = rhsTeardown,
        customOptions = rhsOptions
      } = rhs

--------------------------------------------------------------------------------

-- | LHS context.
--
-- Each LHS context is responsible for setting up the remote relationship, and
-- for tearing it down. Each lhs context is given the JSON representation for
-- the table name on the RHS.
type LHSContext = Value -> Context (Maybe Server)

lhsPostgres :: LHSContext
lhsPostgres tableName =
  Context
    { name = Context.Backend Context.Postgres,
      mkLocalTestEnvironment = lhsPostgresMkLocalTestEnvironment,
      setup = lhsPostgresSetup tableName,
      teardown = lhsPostgresTeardown,
      customOptions = Nothing
    }

lhsSQLServer :: LHSContext
lhsSQLServer tableName =
  Context
    { name = Context.Backend Context.SQLServer,
      mkLocalTestEnvironment = lhsSQLServerMkLocalTestEnvironment,
      setup = lhsSQLServerSetup tableName,
      teardown = lhsSQLServerTeardown,
      customOptions = Nothing
    }

lhsRemoteServer :: LHSContext
lhsRemoteServer tableName =
  Context
    { name = Context.RemoteGraphQLServer,
      mkLocalTestEnvironment = lhsRemoteServerMkLocalTestEnvironment,
      setup = lhsRemoteServerSetup tableName,
      teardown = lhsRemoteServerTeardown,
      customOptions = Nothing
    }

--------------------------------------------------------------------------------

-- | RHS context
--
-- Each RHS context is responsible for setting up the target table, and for
-- returning the JSON representation of said table.
type RHSContext = (Value, Context ())

rhsPostgres :: RHSContext
rhsPostgres =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        Context
          { name = Context.Backend Context.Postgres,
            mkLocalTestEnvironment = Context.noLocalTestEnvironment,
            setup = rhsPostgresSetup,
            teardown = rhsPostgresTeardown,
            customOptions = Nothing
          }
   in (table, context)

rhsSQLServer :: RHSContext
rhsSQLServer =
  let table =
        [yaml|
      schema: hasura
      name: album
    |]
      context =
        Context
          { name = Context.Backend Context.SQLServer,
            mkLocalTestEnvironment = Context.noLocalTestEnvironment,
            setup = rhsSQLServerSetup,
            teardown = rhsSQLServerTeardown,
            customOptions = Nothing
          }
   in (table, context)

--------------------------------------------------------------------------------
-- Schema

-- | LHS
track :: Schema.Table
track =
  Schema.Table
    "track"
    [ Schema.column "id" Schema.TInt,
      Schema.column "title" Schema.TStr,
      Schema.columnNull "album_id" Schema.TInt
    ]
    ["id"]
    []
    [ [Schema.VInt 1, Schema.VStr "track1_album1", Schema.VInt 1],
      [Schema.VInt 2, Schema.VStr "track2_album1", Schema.VInt 1],
      [Schema.VInt 3, Schema.VStr "track3_album1", Schema.VInt 1],
      [Schema.VInt 4, Schema.VStr "track1_album2", Schema.VInt 2],
      [Schema.VInt 5, Schema.VStr "track2_album2", Schema.VInt 2],
      [Schema.VInt 6, Schema.VStr "track1_album3", Schema.VInt 3],
      [Schema.VInt 7, Schema.VStr "track2_album3", Schema.VInt 3],
      [Schema.VInt 8, Schema.VStr "track_no_album", Schema.VNull]
    ]

-- | RHS
album :: Schema.Table
album =
  Schema.Table
    "album"
    [ Schema.column "id" Schema.TInt,
      Schema.column "title" Schema.TStr,
      Schema.columnNull "artist_id" Schema.TInt
    ]
    ["id"]
    []
    [ [Schema.VInt 1, Schema.VStr "album1_artist1", Schema.VInt 1],
      [Schema.VInt 2, Schema.VStr "album2_artist1", Schema.VInt 1],
      [Schema.VInt 3, Schema.VStr "album3_artist2", Schema.VInt 2]
    ]

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (testEnvironment, _) = do
  let sourceName = "source"
      sourceConfig = Postgres.defaultSourceConfiguration
      schemaName = Context.defaultSchema Context.Postgres
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
  Postgres.createTable track
  Postgres.insertTable track
  Schema.trackTable Context.Postgres sourceName track testEnvironment
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
lhsPostgresTeardown _ = Postgres.dropTable track

--------------------------------------------------------------------------------
-- LHS SQLServer

lhsSQLServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsSQLServerMkLocalTestEnvironment _ = pure Nothing

lhsSQLServerSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerSetup rhsTableName (testEnvironment, _) = do
  let sourceName = "source"
      sourceConfig = SQLServer.defaultSourceConfiguration
      schemaName = Context.defaultSchema Context.SQLServer
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
  SQLServer.createTable track
  SQLServer.insertTable track
  Schema.trackTable Context.SQLServer sourceName track testEnvironment
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

lhsSQLServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerTeardown _ = SQLServer.dropTable track

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
    t_title :: m (Maybe Text),
    t_album_id :: m (Maybe Int)
  }
  deriving (Generic)

instance Typeable m => Morpheus.GQLType (HasuraTrack m) where
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

lhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsRemoteServerMkLocalTestEnvironment _ = do
  server <-
    RemoteServer.run $
      RemoteServer.generateQueryInterpreter (Query {hasura_track})
  pure $ Just server
  where
    -- Implements the @hasura_track@ field of the @Query@ type.
    hasura_track (HasuraTrackArgs {..}) = do
      let filterFunction = case ta_where of
            Nothing -> const True
            Just whereArg -> flip matchTrack whereArg
          orderByFunction = case ta_order_by of
            Nothing -> \_ _ -> EQ
            Just orderByArg -> orderTrack orderByArg
          limitFunction = case ta_limit of
            Nothing -> Prelude.id
            Just limitArg -> take limitArg
      pure $
        tracks
          & filter filterFunction
          & sortBy orderByFunction
          & limitFunction
          & map mkTrack
    -- Returns True iif the given track matches the given boolean expression.
    matchTrack trackInfo@(trackId, trackTitle, maybeAlbumId) (HasuraTrackBoolExp {..}) =
      and
        [ maybe True (all (matchTrack trackInfo)) tbe__and,
          maybe True (any (matchTrack trackInfo)) tbe__or,
          maybe True (not . matchTrack trackInfo) tbe__not,
          maybe True (matchInt trackId) tbe_id,
          maybe True (matchString trackTitle) tbe_title,
          maybe True (matchMaybeInt maybeAlbumId) tbe_album_id
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
      sourceConfig = Postgres.defaultSourceConfiguration
      schemaName = Context.defaultSchema Context.Postgres
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
  Postgres.createTable album
  Postgres.insertTable album
  Schema.trackTable Context.Postgres sourceName album testEnvironment

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

rhsPostgresTeardown :: (TestEnvironment, ()) -> IO ()
rhsPostgresTeardown _ = Postgres.dropTable album

--------------------------------------------------------------------------------
-- RHS SQLServer

rhsSQLServerSetup :: (TestEnvironment, ()) -> IO ()
rhsSQLServerSetup (testEnvironment, _) = do
  let sourceName = "target"
      sourceConfig = SQLServer.defaultSourceConfiguration
      schemaName = Context.defaultSchema Context.SQLServer
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
  SQLServer.createTable album
  SQLServer.insertTable album
  Schema.trackTable Context.SQLServer sourceName album testEnvironment

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

rhsSQLServerTeardown :: (TestEnvironment, ()) -> IO ()
rhsSQLServerTeardown _ = SQLServer.dropTable album

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
tests opts = describe "object-relationship" $ do
  schemaTests opts
  executionTests opts
  permissionTests opts

-- | Basic queries using *-to-DB joins
executionTests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
executionTests opts = describe "execution" $ do
  -- fetches the relationship data
  it "related-data" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(where: {title: {_eq: "track1_album1"}}) {
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when any of the join columns are null, the relationship should be null
  it "related-data-null" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(where: {title: {_eq: "track_no_album"}}) {
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when the lhs response has both null and non-null values for join columns
  it "related-data-non-null-and-null" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

-- | Spec that describe an object relationship's data in the presence of
-- permisisons.
permissionTests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
permissionTests opts = describe "permission" $ do
  let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]

  -- only the allowed rows on the target table are queryable
  it "only-allowed-rows" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(
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
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- we use an introspection query to check column permissions:
  -- 1. the type 'hasura_album' has only 'artist_id' and 'title', the allowed columns
  -- 2. the album field in 'hasura_track' type is of type 'hasura_album'
  it "only-allowed-columns" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            album_fields: __type(name: "hasura_album") {
              fields {
                name
              }
            }
            track: hasura_track(where: {title: {_eq: "track1_album1"}}) {
              title
              album {
                __typename
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            album_fields:
              fields:
              - name: artist_id
              - name: title
            track:
            - title: track1_album1
              album:
                __typename: hasura_album
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

schemaTests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
schemaTests opts =
  -- we use an introspection query to check:
  -- 1. a field 'album' is added to the track table
  -- 1. track's where clause does not have 'album' field
  -- 2. track's order_by clause does nat have 'album' field
  it "graphql-schema" $ \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track_fields: __type(name: "hasura_track") {
              fields {
                name
              }
            }
            track_where_exp_fields: __type(name: "hasura_track_bool_exp") {
              inputFields {
                name
              }
            }
            track_order_by_exp_fields: __type(name: "hasura_track_order_by") {
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
