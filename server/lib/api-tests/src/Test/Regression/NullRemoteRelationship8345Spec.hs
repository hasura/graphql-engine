{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Regression tests for issue 8345.
module Test.Regression.NullRemoteRelationship8345Spec (spec) where

import Data.Aeson qualified as J
import Data.Char (isUpper, toLower)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types qualified as Morpheus
import Data.Typeable (Typeable)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.TestResource (Managed)
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment, focusFixtureLeft, focusFixtureRight, stopServer)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironment contexts tests
  where
    contexts = NE.fromList $ do
      (rhsName, rhsMkLocalEnv, rhsSetup, rhsTeardown, albumJoin, artistJoin) <- [rhsPostgres, rhsRemoteServer]
      (lhsName, lhsMkLocalEnv, lhsSetup, lhsTeardown) <- [lhsPostgres, lhsRemoteServer]
      pure
        $ Fixture.Fixture
          { Fixture.name = Fixture.Combine lhsName rhsName,
            Fixture.mkLocalTestEnvironment = \testEnvironment -> do
              lhsServer <- lhsMkLocalEnv testEnvironment
              rhsServer <- rhsMkLocalEnv testEnvironment
              pure $ LocalTestTestEnvironment lhsServer rhsServer,
            Fixture.setupTeardown = \(testEnvironment, LocalTestTestEnvironment lhsServer rhsServer) -> do
              pure
                $ Fixture.SetupAction
                  { Fixture.setupAction = do
                      let schemaName = Schema.getSchemaName testEnvironment
                      -- RHS must always be setup before the LHS
                      rhsSetup (testEnvironment, rhsServer)
                      lhsSetup (albumJoin schemaName) (artistJoin schemaName) (testEnvironment, lhsServer),
                    Fixture.teardownAction = const do
                      GraphqlEngine.clearMetadata testEnvironment
                      lhsTeardown (testEnvironment, lhsServer)
                      rhsTeardown (testEnvironment, rhsServer)
                  },
            Fixture.customOptions = Nothing
          }
    lhsPostgres =
      ( Fixture.Backend Postgres.backendTypeMetadata,
        lhsPostgresMkLocalTestEnvironment,
        lhsPostgresSetup,
        lhsPostgresTeardown
      )
    lhsRemoteServer =
      ( Fixture.RemoteGraphQLServer,
        lhsRemoteServerMkLocalTestEnvironment,
        lhsRemoteServerSetup,
        lhsRemoteServerTeardown
      )
    rhsPostgres =
      ( Fixture.Backend Postgres.backendTypeMetadata,
        rhsPostgresMkLocalTestEnvironment,
        rhsPostgresSetup,
        rhsPostgresTeardown,
        \schemaName ->
          [yaml|
            to_source:
              source: target
              table: {schema: *schemaName, name: album}
              relationship_type: object
              field_mapping:
                album_id: id
          |],
        \schemaName ->
          [yaml|
            to_source:
              source: target
              table: {schema: *schemaName, name: artist}
              relationship_type: object
              field_mapping:
                artist_id: id
          |]
      )
    rhsRemoteServer =
      ( Fixture.RemoteGraphQLServer,
        rhsRemoteServerMkLocalTestEnvironment,
        rhsRemoteServerSetup,
        rhsRemoteServerTeardown,
        const
          $ [yaml|
            to_remote_schema:
              remote_schema: target
              lhs_fields: [album_id]
              remote_field:
                album:
                  arguments:
                    album_id: $album_id
          |],
        const
          $ [yaml|
            to_remote_schema:
              remote_schema: target
              lhs_fields: [artist_id]
              remote_field:
                artist:
                  arguments:
                    artist_id: $artist_id
          |]
      )

-- | Local test testEnvironment.
data LocalTestTestEnvironment = LocalTestTestEnvironment
  { _lhsServer :: Maybe Server,
    _rhsServer :: Maybe Server
  }

--------------------------------------------------------------------------------
-- Schema

-- LHS
lhsTrack :: Schema.Table
lhsTrack =
  (table "track")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr,
          Schema.columnNull "album_id" Schema.TInt,
          Schema.columnNull "artist_id" Schema.TInt
        ],
      tablePrimaryKey = ["id"],
      tableData =
        [ [Schema.VInt 1, Schema.VStr "1_1_null", Schema.VInt 1, Schema.VNull],
          [Schema.VInt 2, Schema.VStr "2_null_1", Schema.VNull, Schema.VInt 1],
          [Schema.VInt 3, Schema.VStr "3_1_1", Schema.VInt 1, Schema.VInt 1],
          [Schema.VInt 4, Schema.VStr "4_null_null", Schema.VNull, Schema.VNull]
        ]
    }

-- RHS
rhsAlbum :: Schema.Table
rhsAlbum =
  (table "album")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "title" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData = [[Schema.VInt 1, Schema.VStr "album1"]]
    }

rhsArtist :: Schema.Table
rhsArtist =
  (table "artist")
    { tableColumns =
        [ Schema.column "id" Schema.TInt,
          Schema.column "name" Schema.TStr
        ],
      tablePrimaryKey = ["id"],
      tableData = [[Schema.VInt 1, Schema.VStr "artist1"]]
    }

-- Remote server RHS schema is defined by the @gqlDocument@

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: J.Value -> J.Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup albumJoin artistJoin (wholeTestEnvironment, _) = do
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
  Postgres.createTable testEnvironment lhsTrack
  Postgres.insertTable testEnvironment lhsTrack
  Schema.trackTable sourceName lhsTrack testEnvironment
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
    definition: *albumJoin
- type: pg_create_remote_relationship
  args:
    source: source
    table:
      schema: *schemaName
      name: track
    name: artist
    definition: *artistJoin
  |]

lhsPostgresTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresTeardown (_testEnvironment, _) = pure ()

--------------------------------------------------------------------------------
-- RHS Postgres

rhsPostgresMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
rhsPostgresMkLocalTestEnvironment _ = pure Nothing

rhsPostgresSetup :: (TestEnvironment, Maybe Server) -> IO ()
rhsPostgresSetup (wholeTestEnvironment, _) = do
  let testEnvironment = focusFixtureRight wholeTestEnvironment
      sourceName = "target"
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  Postgres.createTable testEnvironment rhsAlbum
  Postgres.createTable testEnvironment rhsArtist
  Postgres.insertTable testEnvironment rhsAlbum
  Postgres.insertTable testEnvironment rhsArtist
  Schema.trackTable sourceName rhsAlbum testEnvironment
  Schema.trackTable sourceName rhsArtist testEnvironment

rhsPostgresTeardown :: (TestEnvironment, Maybe Server) -> IO ()
rhsPostgresTeardown (_testEnvironment, _) =
  pure ()

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
    t_album_id :: m (Maybe Int),
    t_artist_id :: m (Maybe Int)
  }
  deriving (Generic)

instance (Typeable m) => Morpheus.GQLType (LHSHasuraTrack m) where
  typeOptions _ _ = hasuraTypeOptions

data LHSHasuraTrackOrderBy = LHSHasuraTrackOrderBy
  { tob_id :: Maybe LHSOrderType,
    tob_title :: Maybe LHSOrderType,
    tob_album_id :: Maybe LHSOrderType,
    tob_artist_id :: Maybe LHSOrderType
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
    tbe_album_id :: Maybe IntCompExp,
    tbe_artist_id :: Maybe IntCompExp
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
    matchTrack trackInfo@(trackId, trackTitle, maybeAlbumId, maybeArtistId) (LHSHasuraTrackBoolExp {..}) =
      and
        [ all (all (matchTrack trackInfo)) tbe__and,
          all (any (matchTrack trackInfo)) tbe__or,
          not (any (matchTrack trackInfo) tbe__not),
          all (matchInt trackId) tbe_id,
          all (matchString trackTitle) tbe_title,
          all (matchMaybeInt maybeAlbumId) tbe_album_id,
          all (matchMaybeInt maybeArtistId) tbe_artist_id
        ]
    matchInt intField IntCompExp {..} = Just intField == _eq
    matchString stringField StringCompExp {..} = Just stringField == _eq
    matchMaybeInt maybeIntField IntCompExp {..} = maybeIntField == _eq
    -- Returns an ordering between the two given tracks.
    orderTrack
      orderByList
      (trackId1, trackTitle1, trackAlbumId1, trackArtistId1)
      (trackId2, trackTitle2, trackAlbumId2, trackArtistId2) =
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
            | Just artistIdOrder <- tob_artist_id ->
                compareWithNullLast artistIdOrder trackArtistId1 trackArtistId2
            | otherwise ->
                error "empty track_order object"
    compareWithNullLast Desc x1 x2 = compareWithNullLast Asc x2 x1
    compareWithNullLast Asc Nothing Nothing = EQ
    compareWithNullLast Asc (Just _) Nothing = LT
    compareWithNullLast Asc Nothing (Just _) = GT
    compareWithNullLast Asc (Just x1) (Just x2) = compare x1 x2
    tracks =
      [ (1, "1_1_null", Just 1, Nothing),
        (2, "2_null_1", Nothing, Just 1),
        (3, "3_1_1", Just 1, Just 1),
        (4, "4_null_null", Nothing, Nothing)
      ]
    mkTrack (trackId, title, albumId, artistId) =
      LHSHasuraTrack
        { t_id = pure $ Just trackId,
          t_title = pure $ Just title,
          t_album_id = pure albumId,
          t_artist_id = pure artistId
        }

lhsRemoteServerSetup :: J.Value -> J.Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerSetup albumJoin artistJoin (testEnvironment, maybeRemoteServer) = case maybeRemoteServer of
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
    definition: *albumJoin
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: source
    type_name: hasura_track
    name: artist
    definition: *artistJoin
      |]

lhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

--------------------------------------------------------------------------------
-- RHS Remote Server

[gqlDocument|

type Album {
  id: Int!
  title: String!
}

type Artist {
  id: Int!
  name: String!
}

type Query {
  album(album_id: Int!): Album
  artist(artist_id: Int!): Artist
}

|]

rhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> Managed (Maybe Server)
rhsRemoteServerMkLocalTestEnvironment _ =
  Just <$> RemoteServer.run (RemoteServer.generateQueryInterpreter (Query {album, artist}))
  where
    albums = [(1, "album1")]
    artists = [(1, "artist1")]
    album (Arg albumId) = pure $ mkAlbum albumId <$> lookup albumId albums
    artist (Arg artistId) = pure $ mkArtist artistId <$> lookup artistId artists
    mkAlbum albumId title = Album (pure albumId) (pure title)
    mkArtist artistId name = Artist (pure artistId) (pure name)

rhsRemoteServerSetup :: (TestEnvironment, Maybe Server) -> IO ()
rhsRemoteServerSetup (testEnvironment, remoteServer) = for_ remoteServer \server -> do
  let remoteSchemaEndpoint = GraphqlEngine.serverUrl server ++ "/graphql"
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: add_remote_schema
args:
  name: target
  definition:
    url: *remoteSchemaEndpoint
  |]

rhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
rhsRemoteServerTeardown (_, server) = traverse_ stopServer server

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, LocalTestTestEnvironment)
tests = do
  it "joins album when artist is null" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(where: {id: {_eq: 1}}) {
              title
              album {
                title
              }
              artist {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "1_1_null"
               album:
                 title: album1
               artist: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins artist when album is null" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(where: {id: {_eq: 2}}) {
              title
              album {
                title
              }
              artist {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "2_null_1"
               album: null
               artist:
                 name: artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins both when nothing is null" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(where: {id: {_eq: 3}}) {
              title
              album {
                title
              }
              artist {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "3_1_1"
               album:
                 title: album1
               artist:
                 name: artist1
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins neither when both null" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            track: hasura_track(where: {id: {_eq: 4}}) {
              title
              album {
                title
              }
              artist {
                name
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            track:
             - title: "4_null_null"
               album: null
               artist: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
