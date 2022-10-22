{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for remote relationships to remote schemas. Remote relationships are
-- relationships that are not local to a given source or remote schema, and are
-- handled by the engine itself.
--
-- All tests use the same GraphQL syntax, and the only difference is in the
-- setup: for each left-hand side source we support we do a custom setup and run
-- the tests.
module Test.RemoteRelationship.XToRemoteSchemaRelationshipSpec
  ( spec,
  )
where

import Data.Char (isUpper, toLower)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List (intercalate, sortBy)
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types qualified as Morpheus
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
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
    contexts = map mkContext [lhsPostgres, lhsSQLServer, lhsRemoteServer]
    lhsPostgres =
      Context
        { name = Context.Backend Context.Postgres,
          mkLocalTestEnvironment = lhsPostgresMkLocalTestEnvironment,
          setup = lhsPostgresSetup,
          teardown = lhsPostgresTeardown,
          customOptions = Nothing
        }
    lhsSQLServer =
      Context
        { name = Context.Backend Context.SQLServer,
          mkLocalTestEnvironment = lhsSQLServerMkLocalTestEnvironment,
          setup = lhsSQLServerSetup,
          teardown = lhsSQLServerTeardown,
          customOptions = Nothing
        }
    lhsRemoteServer =
      Context
        { name = Context.RemoteGraphQLServer,
          mkLocalTestEnvironment = lhsRemoteServerMkLocalTestEnvironment,
          setup = lhsRemoteServerSetup,
          teardown = lhsRemoteServerTeardown,
          customOptions = Nothing
        }

-- | Uses a given RHS context to create a combined context.
mkContext :: Context (Maybe Server) -> Context LocalTestTestEnvironment
mkContext lhs =
  Context
    { name = lhsName,
      mkLocalTestEnvironment = \testEnvironment -> do
        rhsServer <- rhsRemoteServerMkLocalTestEnvironment testEnvironment
        lhsServer <- lhsMkLocalTestEnvironment testEnvironment
        pure $ LocalTestTestEnvironment lhsServer rhsServer,
      setup = \(testEnvironment, LocalTestTestEnvironment lhsServer rhsServer) -> do
        rhsRemoteSchemaSetup (testEnvironment, rhsServer)
        lhsSetup (testEnvironment, lhsServer),
      teardown = \(testEnvironment, LocalTestTestEnvironment lhsServer rhsServer) -> do
        lhsTeardown (testEnvironment, lhsServer)
        rhsRemoteSchemaTeardown (testEnvironment, rhsServer)
        GraphqlEngine.clearMetadata testEnvironment,
      customOptions = lhsOptions
    }
  where
    Context
      { name = lhsName,
        mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
        setup = lhsSetup,
        teardown = lhsTeardown,
        customOptions = lhsOptions
      } = lhs

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

-- RHS schema is defined by the @gqlDocument@

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup (testEnvironment, _) = do
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

lhsPostgresTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresTeardown (testEnvironment, _) = do
  let sourceName = "source"
  Schema.untrackTable Context.Postgres sourceName track testEnvironment
  Postgres.dropTable track

--------------------------------------------------------------------------------
-- LHS SQLServer

lhsSQLServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsSQLServerMkLocalTestEnvironment _ = pure Nothing

lhsSQLServerSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerSetup (testEnvironment, _) = do
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

lhsSQLServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerTeardown (testEnvironment, _) = do
  let sourceName = "source"
  Schema.untrackTable Context.SQLServer sourceName track testEnvironment
  SQLServer.dropTable track

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

instance Typeable m => Morpheus.GQLType (LHSQuery m) where
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

instance Typeable m => Morpheus.GQLType (LHSHasuraTrack m) where
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

lhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsRemoteServerMkLocalTestEnvironment _ = do
  server <-
    RemoteServer.run $
      RemoteServer.generateQueryInterpreter (LHSQuery {q_hasura_track = hasura_track})
  pure $ Just server
  where
    -- Implements the @hasura_track@ field of the @Query@ type.
    hasura_track (LHSHasuraTrackArgs {..}) = do
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
    matchTrack trackInfo@(trackId, trackTitle, maybeAlbumId) (LHSHasuraTrackBoolExp {..}) =
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

rhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> IO Server
rhsRemoteServerMkLocalTestEnvironment _ =
  RemoteServer.run $
    RemoteServer.generateQueryInterpreter (Query {album})
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

tests :: Context.Options -> SpecWith (TestEnvironment, LocalTestTestEnvironment)
tests opts = describe "remote-schema-relationship" do
  schemaTests opts
  executionTests opts

-- | Basic queries using *-to-DB joins
executionTests :: Context.Options -> SpecWith (TestEnvironment, LocalTestTestEnvironment)
executionTests opts = describe "execution" do
  -- fetches the relationship data
  it "related-data" \(testEnvironment, _) -> do
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
  it "related-data-null" \(testEnvironment, _) -> do
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
  it "related-data-non-null-and-null" \(testEnvironment, _) -> do
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

schemaTests :: Context.Options -> SpecWith (TestEnvironment, LocalTestTestEnvironment)
schemaTests opts =
  -- we use an introspection query to check:
  -- 1. a field 'album' is added to the track table
  -- 1. track's where clause does not have 'album' field
  -- 2. track's order_by clause does nat have 'album' field
  it "graphql-schema" \(testEnvironment, _) -> do
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
