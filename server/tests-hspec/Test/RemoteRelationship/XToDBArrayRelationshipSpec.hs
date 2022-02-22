-- | Tests for array remote relationships to databases. Remote relationships are
-- relationships that are not local to a given source or remote schema, and are
-- handled by the engine itself. Array relationsips are 1-to-many relationships.
--
-- All tests use the same GraphQL syntax, and the only difference is in the
-- setup: we do a cartesian product of all kinds of sources we support on the
-- left-hand side and all databases we support on the right-hand side.
module Test.RemoteRelationship.XToDBArrayRelationshipSpec
  ( spec,
  )
where

import Control.Lens (findOf, has, only, (^?!))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, values, _String)
import Data.Foldable (for_)
import Data.Maybe qualified as Unsafe (fromJust)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Sql (sql)
import Harness.Quoter.Yaml (shouldBeYaml, shouldReturnYaml, yaml)
import Harness.State (Server, State)
import Harness.Test.Context (Context (..))
import Harness.Test.Context qualified as Context
import Test.Hspec (SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec = Context.runWithLocalState contexts tests
  where
    lhsContexts = [lhsPostgres]
    rhsContexts = [rhsPostgres]
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
      mkLocalState = lhsMkLocalState,
      setup = \(state, localState) -> do
        GraphqlEngine.clearMetadata state
        rhsSetup (state, ())
        lhsSetup (state, localState),
      teardown = \state@(globalState, _) -> do
        lhsTeardown state
        rhsTeardown (globalState, ()),
      customOptions =
        Context.combineOptions lhsOptions rhsOptions
    }
  where
    Context
      { name = lhsName,
        mkLocalState = lhsMkLocalState,
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
    { name = Context.Postgres,
      mkLocalState = lhsPostgresMkLocalState,
      setup = lhsPostgresSetup tableName,
      teardown = lhsPostgresTeardown,
      customOptions = Nothing
    }

{-
lhsRemoteServer :: Value -> Context
lhsRemoteServer tableName = Context "from RS" (lhsRemoteSetup tableName) lhsRemoteTeardown
-}

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
          { name = Context.Postgres,
            mkLocalState = Context.noLocalState,
            setup = rhsPostgresSetup,
            teardown = rhsPostgresTeardown,
            customOptions = Nothing
          }
   in (table, context)

{-
rhsMSSQL :: (Value, Context)
rhsMSSQL = ([yaml|{"schema":"hasura", "name":"album"}|], Context "MSSQL" rhsMSSQLSetup rhsMSSQLTeardown)
-}

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalState :: State -> IO (Maybe Server)
lhsPostgresMkLocalState _ = pure Nothing

lhsPostgresSetup :: Value -> (State, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (state, _) = do
  Postgres.run_
    [sql|
create table hasura.artist (
  id int null,
  name text not null
);
insert into hasura.artist (id, name) values
  (1, 'artist1'),
  (2, 'artist2'),
  (3, 'artist_no_albums'),
  (null, 'artist_no_id');
  |]
  let lhsTableName = [yaml|{"schema":"hasura", "name":"artist"}|]
      sourceConfig = Postgres.defaultSourceConfiguration
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bulk
args:
- type: pg_add_source
  args:
    name: source
    configuration: *sourceConfig
- type: pg_track_table
  args:
    source: source
    table: *lhsTableName
- type: pg_create_select_permission
  args:
    source: source
    role: role1
    table: *lhsTableName
    permission:
      columns: '*'
      filter: {}
- type: pg_create_select_permission
  args:
    source: source
    role: role2
    table: *lhsTableName
    permission:
      columns: '*'
      filter: {}
- type: pg_create_remote_relationship
  args:
    source: source
    table: *lhsTableName
    name: albums
    definition:
      to_source:
        source: target
        table: *rhsTableName
        relationship_type: array
        field_mapping:
          id: artist_id
  |]

lhsPostgresTeardown :: (State, Maybe Server) -> IO ()
lhsPostgresTeardown _ =
  Postgres.run_
    [sql|
DROP TABLE hasura.artist;
|]

--------------------------------------------------------------------------------
-- RHS Postgres

rhsPostgresSetup :: (State, ()) -> IO ()
rhsPostgresSetup (state, _) = do
  Postgres.run_
    [sql|
create table hasura.album (
  id serial primary key,
  title text not null,
  artist_id int null
);
insert into hasura.album (title, artist_id) values
  ('album1_artist1', 1),
  ('album2_artist1', 1),
  ('album3_artist2', 2);
  |]

  let rhsTableName = [yaml|{"schema":"hasura", "name":"album"}|]
      sourceConfig = Postgres.defaultSourceConfiguration
  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bulk
args:
- type: pg_add_source
  args:
    name: target
    configuration: *sourceConfig
- type: pg_track_table
  args:
    source: target
    table: *rhsTableName
- type: pg_create_select_permission
  args:
    source: target
    role: role1
    table: *rhsTableName
    permission:
      columns:
        - title
        - artist_id
      filter:
        artist_id:
          _eq: x-hasura-artist-id
- type: pg_create_select_permission
  args:
    source: target
    role: role2
    table: *rhsTableName
    permission:
      columns: [id, title, artist_id]
      filter:
        artist_id:
          _eq: x-hasura-artist-id
      limit: 1
      allow_aggregations: true
  |]

rhsPostgresTeardown :: (State, ()) -> IO ()
rhsPostgresTeardown _ =
  Postgres.run_
    [sql|
DROP TABLE hasura.album;
|]

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith (State, Maybe Server)
tests opts = describe "array-relationship" $ do
  schemaTests opts
  executionTests opts
  permissionTests opts

schemaTests :: Context.Options -> SpecWith (State, Maybe Server)
schemaTests _opts =
  -- we introspect the schema and validate it
  it "graphql-schema" $ \(state, _) -> do
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
            artist_fields: __type(name: "hasura_artist") {
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
        relationshipFieldArgsSchema =
          [yaml|
          - name: distinct_on
            type:
              kind: LIST
              name: null
              ofType:
                kind: NON_NULL
                name: null
                ofType:
                  kind: ENUM
                  name: hasura_album_select_column
                  ofType: null
          - name: limit
            type:
              kind: SCALAR
              name: Int
              ofType: null
          - name: offset
            type:
              kind: SCALAR
              name: Int
              ofType: null
          - name: order_by
            type:
              kind: LIST
              name: null
              ofType:
                kind: NON_NULL
                name: null
                ofType:
                  kind: INPUT_OBJECT
                  name: hasura_album_order_by
                  ofType: null
          - name: where
            type:
              kind: INPUT_OBJECT
              name: hasura_album_bool_exp
              ofType: null
          |]
    introspectionResult <- GraphqlEngine.postGraphql state query
    let focusArtistFields =
          key "data"
            . key "artist_fields"
            . key "fields"
            . values
        albumsField =
          Unsafe.fromJust $
            findOf
              focusArtistFields
              (has $ key "name" . _String . only "albums")
              introspectionResult
        albumsAggregateField =
          Unsafe.fromJust $
            findOf
              focusArtistFields
              (has $ key "name" . _String . only "albums_aggregate")
              introspectionResult

    -- the schema of args should be same for both albums and albums_aggregate field
    for_
      [ albumsField ^?! key "args",
        albumsAggregateField ^?! key "args"
      ]
      \schema ->
        schema `shouldBeYaml` relationshipFieldArgsSchema

    -- check the return type of albums field
    shouldBeYaml
      (albumsField ^?! key "type")
      [yaml|
      kind: NON_NULL
      name: null
      ofType:
        kind: LIST
        name: null
        ofType:
          kind: NON_NULL
          name: null
          ofType:
            name: hasura_album
      |]

    -- check the return type of albums_aggregate field
    shouldBeYaml
      (albumsAggregateField ^?! key "type")
      [yaml|
      name: null
      kind: NON_NULL
      ofType:
        name: hasura_album_aggregate
        kind: OBJECT
        ofType: null
      |]

-- | Basic queries using DB-to-DB joins
executionTests :: Context.Options -> SpecWith (State, Maybe Server)
executionTests opts = describe "execution" $ do
  -- fetches the relationship data
  it "related-data" $ \(state, _) -> do
    let query =
          [graphql|
          query {
            artist: hasura_artist(where: {name: {_eq: "artist1"}}) {
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
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphql state query)
      expectedResponse

  -- when there are no matching rows, the relationship response should be []
  it "related-data-empty-array" $ \(state, _) -> do
    let query =
          [graphql|
          query {
            artist: hasura_artist(where: {name: {_eq: "artist_no_albums"}}) {
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
             - name: artist_no_albums
               albums: []
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
            artist: hasura_artist(where: {name: {_eq: "artist_no_id"}}) {
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
             - name: artist_no_id
               albums: null
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
            artist: hasura_artist(
              where: {
                _or: [
                  {name: {_eq: "artist1"}},
                  {name: {_eq: "artist_no_id"}}
                ]
              },
              order_by: {id: asc}
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
             - name: artist_no_id
               albums: null
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphql state query)
      expectedResponse

-- TODO:
-- 1. where
-- 1. limit with order_by
-- 1. offset
-- 1. _aggregate

-- | tests that describe an array relationship's data in the presence of permisisons
permissionTests :: Context.Options -> SpecWith (State, Maybe Server)
permissionTests opts = describe "permission" $ do
  -- only the allowed rows on the target table are queryable
  it "only-allowed-rows" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
              order_by: {id: asc}
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
             - name: artist2
               albums: []
             - name: artist_no_albums
               albums: []
             - name: artist_no_id
               albums: null
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- we use an introspection query to check column permissions:
  -- 1. the type 'hasura_album' has only 'artist_id' and 'title', the allowed columns
  -- 2. the albums field in 'hasura_artist' type is of type 'hasura_album'
  it "only-allowed-columns" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            album_fields: __type(name: "hasura_album") {
              fields {
                name
              }
            }
            artist: hasura_artist(
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
          [yaml|
          data:
            album_fields:
              fields:
              - name: artist_id
              - name: title
            artist:
            - name: artist1
              albums:
              - __typename: hasura_album
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- _aggregate field should not be generated when 'allow_aggregations' isn't set to 'true'
  it "aggregations-not-allowed" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role1")]
        query =
          [graphql|
          query {
            artist_fields: __type(name: "hasura_artist") {
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
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- _aggregate field should only be allowed when 'allow_aggregations' is set to 'true'
  it "aggregations-allowed" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role2")]
        query =
          [graphql|
          query {
            artist_fields: __type(name: "hasura_artist") {
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
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- permission limit should kick in when no query limit is specified
  it "no-query-limit" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
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
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- query limit should be applied when query limit <= permission limit
  it "user-limit-less-than-permission-limit" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums (order_by: {id: asc} limit: 0){
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
               albums: []
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- permission limit should be applied when query limit > permission limit
  it "user-limit-greater-than-permission-limit" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
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
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- permission limit should only apply on 'nodes' but not on 'aggregate'
  it "aggregations" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
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
                   count: 2
                 nodes:
                 - title: album1_artist1
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse

  -- query limit applies to both 'aggregate' and 'nodes'
  it "aggregations-query-limit" $ \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
              where: {name: {_eq: "artist1"}}
            ) {
              name
              albums_aggregate (limit: 1 order_by: {id: asc}){
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
                   count: 1
                 nodes:
                 - title: album1_artist1
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders state userHeaders query)
      expectedResponse
