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

import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types (Arg (..))
import Data.Text (Text)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.State (Server, State)
import Harness.Test.Context (Context (..))
import Harness.Test.Context qualified as Context
import Harness.Test.Schema qualified as Schema
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec = Context.runWithLocalState contexts tests
  where
    contexts = map mkContext [lhsPostgres]
    lhsPostgres =
      Context
        { name = Context.Backend Context.Postgres,
          mkLocalState = lhsPostgresMkLocalState,
          setup = lhsPostgresSetup,
          teardown = lhsPostgresTeardown,
          customOptions = Nothing
        }

-- | Uses a given RHS context to create a combined context.
mkContext :: Context (Maybe Server) -> Context LocalTestState
mkContext lhs =
  Context
    { name = lhsName,
      mkLocalState = \state -> do
        rhsServer <- rhsRemoteServerMkLocalState state
        lhsServer <- lhsMkLocalState state
        pure $ LocalTestState lhsServer rhsServer,
      setup = \(state, LocalTestState lhsServer rhsServer) -> do
        GraphqlEngine.clearMetadata state
        rhsRemoteSchemaSetup (state, rhsServer)
        lhsSetup (state, lhsServer),
      teardown = \(state, LocalTestState lhsServer rhsServer) -> do
        lhsTeardown (state, lhsServer)
        rhsRemoteSchemaTeardown (state, rhsServer),
      customOptions = lhsOptions
    }
  where
    Context
      { name = lhsName,
        mkLocalState = lhsMkLocalState,
        setup = lhsSetup,
        teardown = lhsTeardown,
        customOptions = lhsOptions
      } = lhs

-- | Local test state.
data LocalTestState = LocalTestState
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

lhsPostgresMkLocalState :: State -> IO (Maybe Server)
lhsPostgresMkLocalState _ = pure Nothing

lhsPostgresSetup :: (State, Maybe Server) -> IO ()
lhsPostgresSetup (state, _) = do
  let sourceName = "source"
      sourceConfig = Postgres.defaultSourceConfiguration
      schemaName = Context.defaultSchema Context.Postgres
  -- Add remote source
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: pg_add_source
args:
  name: *sourceName
  configuration: *sourceConfig
|]
  -- setup tables only
  Postgres.createTable track
  Postgres.insertTable track
  Schema.trackTable Context.Postgres sourceName track state
  GraphqlEngine.postMetadata_
    state
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

lhsPostgresTeardown :: (State, Maybe Server) -> IO ()
lhsPostgresTeardown (state, _) = do
  let sourceName = "source"
  Schema.untrackTable Context.Postgres sourceName track state
  Postgres.dropTable track

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

rhsRemoteServerMkLocalState :: State -> IO Server
rhsRemoteServerMkLocalState _ =
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

rhsRemoteSchemaSetup :: (State, Server) -> IO ()
rhsRemoteSchemaSetup (state, remoteServer) = do
  let remoteSchemaEndpoint = GraphqlEngine.serverUrl remoteServer ++ "/graphql"
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: add_remote_schema
args:
  name: target
  definition:
    url: *remoteSchemaEndpoint
  |]

rhsRemoteSchemaTeardown :: (State, Server) -> IO ()
rhsRemoteSchemaTeardown (_, server) = GraphqlEngine.stopServer server

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith (State, LocalTestState)
tests opts = describe "remote-schema-relationship" $ do
  schemaTests opts
  executionTests opts

-- | Basic queries using *-to-DB joins
executionTests :: Context.Options -> SpecWith (State, LocalTestState)
executionTests opts = describe "execution" $ do
  -- fetches the relationship data
  it "related-data" $ \(state, _) -> do
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
      (GraphqlEngine.postGraphql state query)
      expectedResponse

  -- when any of the join columns are null, the relationship should be null
  it "related-data-null" $ \(state, _) -> do
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
      (GraphqlEngine.postGraphql state query)
      expectedResponse

  -- when the lhs response has both null and non-null values for join columns
  it "related-data-non-null-and-null" $ \(state, _) -> do
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
              order_by: {id: asc}
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
      (GraphqlEngine.postGraphql state query)
      expectedResponse

schemaTests :: Context.Options -> SpecWith (State, LocalTestState)
schemaTests opts =
  -- we use an introspection query to check:
  -- 1. a field 'album' is added to the track table
  -- 1. track's where clause does not have 'album' field
  -- 2. track's order_by clause does nat have 'album' field
  it "graphql-schema" $ \(state, _) -> do
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
      (GraphqlEngine.postGraphql state query)
      expectedResponse
