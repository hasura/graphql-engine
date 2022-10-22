{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This file contains all the contexts for setting up remote relationships between
-- different kinds of sources.
-- Currently remote relationships are possible between:
--  1. Two Postgres Sources
--  2. (Postgres - Remote Schema), here a PG source has remote relationship with a
--     remote schema
--  3. (Remote Schema - Postgres), here a remote schema has remote relationship with a
--     PG source.
--  4. (Remote Schema - Remote Schema), here a remote schema has a remote relationship
--     with another remote schema
--
-- A Remote relationship has two entities: LHS (left hand side) and RHS (right hand
-- side). Think of them as a mathematical equation: LHS = RHS i.e a LHS entity
-- depends on RHS entity.
-- In terms of remote relationship:
--    A source present on LHS has a remote relationship with the source on RHS. That
--    means, the source on LHS depends on RHS. This is the reason why in the setup of
--    tests - we first setup the RHS and then setup the LHS. And we do the reverse in
--    teardown.
--
-- The RHS source in the below tests have the source name as "target"
-- The LHS source in the below tests have the source name as "source"
module Test.RemoteRelationship.MetadataAPI.Common
  ( dbTodbRemoteRelationshipContext,
    dbToRemoteSchemaRemoteRelationshipContext,
    remoteSchemaToDBRemoteRelationshipContext,
    remoteSchemaToremoteSchemaRemoteRelationshipContext,
    LocalTestTestEnvironment (..),
  )
where

--------------------------------------------------------------------------------
-- Debugging

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
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Test.Context (Context (..))
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (Server, TestEnvironment, stopServer)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

data LocalTestTestEnvironment = LocalTestTestEnvironment
  { _lhsServer :: Maybe Server,
    _rhsServer :: Maybe Server
  }

dbTodbRemoteRelationshipContext :: Context LocalTestTestEnvironment
dbTodbRemoteRelationshipContext =
  Context
    { name = Context.Combine (Context.Backend Context.Postgres) (Context.Backend Context.Postgres),
      mkLocalTestEnvironment = \_testEnvironment ->
        pure $ LocalTestTestEnvironment Nothing Nothing,
      setup = \(testEnvironment, _) -> do
        GraphqlEngine.clearMetadata testEnvironment
        rhsPostgresSetup testEnvironment
        lhsPostgresSetup (testEnvironment, Nothing)
        createSourceRemoteRelationship testEnvironment,
      teardown = \(testEnvironment, _) -> do
        GraphqlEngine.clearMetadata testEnvironment
        lhsPostgresTeardown
        rhsPostgresTeardown
        pure (),
      customOptions = Nothing
    }

dbToRemoteSchemaRemoteRelationshipContext :: Context LocalTestTestEnvironment
dbToRemoteSchemaRemoteRelationshipContext =
  Context
    { name = Context.Combine (Context.Backend Context.Postgres) Context.RemoteGraphQLServer,
      mkLocalTestEnvironment = \testEnvironment -> do
        rhsServer <- rhsRemoteServerMkLocalTestEnvironment testEnvironment
        pure $ LocalTestTestEnvironment Nothing rhsServer,
      setup = \(testEnvironment, LocalTestTestEnvironment _ rhsSever) -> do
        GraphqlEngine.clearMetadata testEnvironment
        rhsRemoteServerSetup (testEnvironment, rhsSever)
        lhsPostgresSetup (testEnvironment, Nothing)
        createRemoteSchemaRemoteRelationship testEnvironment,
      teardown = \(testEnvironment, LocalTestTestEnvironment _ rhsSever) -> do
        GraphqlEngine.clearMetadata testEnvironment
        lhsPostgresTeardown
        rhsRemoteServerTeardown (testEnvironment, rhsSever),
      customOptions = Nothing
    }

remoteSchemaToDBRemoteRelationshipContext :: Context LocalTestTestEnvironment
remoteSchemaToDBRemoteRelationshipContext =
  Context
    { name = Context.Combine Context.RemoteGraphQLServer (Context.Backend Context.Postgres),
      mkLocalTestEnvironment = \testEnvironment -> do
        lhsServer <- lhsRemoteServerMkLocalTestEnvironment testEnvironment
        pure $ LocalTestTestEnvironment lhsServer Nothing,
      setup = \(testEnvironment, LocalTestTestEnvironment lhsServer _) -> do
        GraphqlEngine.clearMetadata testEnvironment
        rhsPostgresSetup testEnvironment
        lhsRemoteServerSetup (testEnvironment, lhsServer)
        addRStoDBRelationship testEnvironment,
      teardown = \(testEnvironment, LocalTestTestEnvironment lhsServer _) -> do
        GraphqlEngine.clearMetadata testEnvironment
        lhsRemoteServerTeardown (testEnvironment, lhsServer)
        rhsPostgresTeardown,
      customOptions = Nothing
    }

remoteSchemaToremoteSchemaRemoteRelationshipContext :: Context LocalTestTestEnvironment
remoteSchemaToremoteSchemaRemoteRelationshipContext =
  Context
    { name = Context.Combine Context.RemoteGraphQLServer Context.RemoteGraphQLServer,
      mkLocalTestEnvironment = \testEnvironment -> do
        lhsServer <- lhsRemoteServerMkLocalTestEnvironment testEnvironment
        rhsServer <- rhsRemoteServerMkLocalTestEnvironment testEnvironment
        pure $ LocalTestTestEnvironment lhsServer rhsServer,
      setup = \(testEnvironment, LocalTestTestEnvironment lhsServer rhsServer) -> do
        GraphqlEngine.clearMetadata testEnvironment
        rhsRemoteServerSetup (testEnvironment, rhsServer)
        lhsRemoteServerSetup (testEnvironment, lhsServer)
        addRStoRSRelationship testEnvironment,
      teardown = \(testEnvironment, LocalTestTestEnvironment lhsServer rhsServer) -> do
        GraphqlEngine.clearMetadata testEnvironment
        lhsRemoteServerTeardown (testEnvironment, lhsServer)
        rhsRemoteServerTeardown (testEnvironment, rhsServer),
      customOptions = Nothing
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

-- | RHS
albumTable :: Schema.Table
albumTable =
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
-- DB to DB Postgres Remote relationship

-- | RHS Postgres Setup
rhsPostgresSetup :: TestEnvironment -> IO ()
rhsPostgresSetup testEnvironment = do
  let sourceName = "target"
      sourceConfig = Postgres.defaultSourceConfiguration
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: pg_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  Postgres.createTable albumTable
  Postgres.insertTable albumTable
  Schema.trackTable Context.Postgres sourceName albumTable testEnvironment

rhsPostgresTeardown :: IO ()
rhsPostgresTeardown = Postgres.dropTable albumTable

-- | LHS Postgres Setup
lhsPostgresSetup :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup (testEnvironment, _) = do
  let sourceName = "source"
      sourceConfig = Postgres.defaultSourceConfiguration
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

createSourceRemoteRelationship :: TestEnvironment -> IO ()
createSourceRemoteRelationship testEnvironment = do
  let schemaName = Context.defaultSchema Context.Postgres
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
    name: albums
    definition:
      to_source:
        source: target
        table:
          schema: hasura
          name: album
        relationship_type: object
        field_mapping:
          id: artist_id
  |]

lhsPostgresTeardown :: IO ()
lhsPostgresTeardown = Postgres.dropTable track

--------------------------------------------------------------------------------
-- DB to Remote Schema Remote relationship

-- Here the RHS is remote schema because a postgres source has a remote reltionship
-- with it and hence depends on it.

createRemoteSchemaRemoteRelationship :: TestEnvironment -> IO ()
createRemoteSchemaRemoteRelationship testEnvironment = do
  let schemaName = Context.defaultSchema Context.Postgres
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
-- Remote Schema to DB Remote relationship

-- | LHS Remote Server
addRStoDBRelationship :: TestEnvironment -> IO ()
addRStoDBRelationship testEnvironment =
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: source
    type_name: hasura_track
    name: album
    definition:
      to_source:
        source: target
        table:
          schema: hasura
          name: album
        relationship_type: object
        field_mapping:
          album_id: id
      |]

--------------------------------------------------------------------------------
-- Remote Schema to Remote Schema Remote relationship

addRStoRSRelationship :: TestEnvironment -> IO ()
addRStoRSRelationship testEnvironment =
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
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

--------------------------------------------------------------------------------
-- Remote server

-- | RHS Remote server
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

rhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
rhsRemoteServerMkLocalTestEnvironment _ = do
  server <-
    RemoteServer.run $
      RemoteServer.generateQueryInterpreter (Query {album})
  pure $ Just server
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

rhsRemoteServerSetup :: (TestEnvironment, Maybe Server) -> IO ()
rhsRemoteServerSetup (testEnvironment, maybeRemoteServer) = case maybeRemoteServer of
  Nothing -> error "RHS remote server local testEnvironment did not succesfully create a server"
  Just remoteServer -> do
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

rhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
rhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

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
  Nothing -> error "LHS remote server local testEnvironment did not succesfully create a server"
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
      |]

lhsRemoteServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer
