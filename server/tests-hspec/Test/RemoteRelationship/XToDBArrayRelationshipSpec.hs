{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Data.Char (isUpper, toLower)
import Data.Foldable (for_, traverse_)
import Data.Function ((&))
import Data.List (intercalate, sortBy)
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Maybe qualified as Unsafe (fromJust)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types qualified as Morpheus
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (shouldBeYaml, shouldReturnYaml, yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.State (Server, State, stopServer)
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
    lhsContexts = [lhsPostgres, lhsRemoteServer]
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
    { name = Context.Backend Context.Postgres,
      mkLocalState = lhsPostgresMkLocalState,
      setup = lhsPostgresSetup tableName,
      teardown = lhsPostgresTeardown,
      customOptions = Nothing
    }

lhsRemoteServer :: LHSContext
lhsRemoteServer tableName =
  Context
    { name = Context.RemoteGraphQLServer,
      mkLocalState = lhsRemoteServerMkLocalState,
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
-- Schema

-- | LHS
artist :: Schema.Table
artist =
  Schema.Table
    "artist"
    [ Schema.columnNull "id" Schema.TInt,
      Schema.column "name" Schema.TStr
    ]
    []
    []
    [ [Schema.VInt 1, Schema.VStr "artist1"],
      [Schema.VInt 2, Schema.VStr "artist2"],
      [Schema.VInt 3, Schema.VStr "artist_no_albums"],
      [Schema.VNull, Schema.VStr "artist_no_id"]
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

lhsPostgresMkLocalState :: State -> IO (Maybe Server)
lhsPostgresMkLocalState _ = pure Nothing

lhsPostgresSetup :: Value -> (State, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (state, _) = do
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
  Postgres.createTable artist
  Postgres.insertTable artist
  Schema.trackTable Context.Postgres sourceName artist state

  GraphqlEngine.postMetadata_
    state
    [yaml|
type: bulk
args:
- type: pg_create_select_permission
  args:
    source: *sourceName
    role: role1
    table:
      schema: *schemaName
      name: artist
    permission:
      columns: '*'
      filter: {}
- type: pg_create_select_permission
  args:
    source: *sourceName
    role: role2
    table:
      schema: *schemaName
      name: artist
    permission:
      columns: '*'
      filter: {}
- type: pg_create_remote_relationship
  args:
    source: *sourceName
    table:
      schema: *schemaName
      name: artist
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
lhsPostgresTeardown _ = Postgres.dropTable artist

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

instance Typeable m => Morpheus.GQLType (Query m)

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

instance Typeable m => Morpheus.GQLType (HasuraArtist m) where
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

lhsRemoteServerMkLocalState :: State -> IO (Maybe Server)
lhsRemoteServerMkLocalState _ = do
  server <-
    RemoteServer.run $
      RemoteServer.generateQueryInterpreter (Query {hasura_artist})
  pure $ Just server
  where
    -- Implements the @hasura_artist@ field of the @Query@ type.
    hasura_artist (HasuraArtistArgs {..}) = do
      let filterFunction = case aa_where of
            Nothing -> const True
            Just whereArg -> flip matchArtist whereArg
          orderByFunction = case aa_order_by of
            Nothing -> \_ _ -> EQ
            Just orderByArg -> orderArtist orderByArg
          limitFunction = case aa_limit of
            Nothing -> Prelude.id
            Just limitArg -> take limitArg
      pure $
        artists
          & filter filterFunction
          & sortBy orderByFunction
          & limitFunction
          & map mkArtist
    -- Returns True iif the given artist matches the given boolean expression.
    matchArtist artistInfo@(artistId, artistName) (HasuraArtistBoolExp {..}) =
      and
        [ maybe True (all (matchArtist artistInfo)) abe__and,
          maybe True (any (matchArtist artistInfo)) abe__or,
          maybe True (not . matchArtist artistInfo) abe__not,
          maybe True (matchMaybeInt artistId) abe_id,
          maybe True (matchString artistName) abe_name
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
        (Just 3, "artist_no_albums"),
        (Nothing, "artist_no_id")
      ]
    mkArtist (artistId, artistName) =
      HasuraArtist
        { a_id = pure artistId,
          a_name = pure $ Just artistName
        }

lhsRemoteServerSetup :: Value -> (State, Maybe Server) -> IO ()
lhsRemoteServerSetup tableName (state, maybeRemoteServer) = case maybeRemoteServer of
  Nothing -> error "XToDBArrayRelationshipSpec: remote server local state did not succesfully create a server"
  Just remoteServer -> do
    let remoteSchemaEndpoint = GraphqlEngine.serverUrl remoteServer ++ "/graphql"
    GraphqlEngine.postMetadata_
      state
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

lhsRemoteServerTeardown :: (State, Maybe Server) -> IO ()
lhsRemoteServerTeardown (_, maybeServer) = traverse_ stopServer maybeServer

--------------------------------------------------------------------------------
-- RHS Postgres

rhsPostgresSetup :: (State, ()) -> IO ()
rhsPostgresSetup (state, _) = do
  let sourceName = "target"
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
  Postgres.createTable album
  Postgres.insertTable album
  Schema.trackTable Context.Postgres sourceName album state

  GraphqlEngine.postMetadata_
    state
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

rhsPostgresTeardown :: (State, ()) -> IO ()
rhsPostgresTeardown _ = Postgres.dropTable album

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith (State, Maybe Server)
tests opts = describe "array-relationship" do
  schemaTests opts
  executionTests opts
  permissionTests opts

schemaTests :: Context.Options -> SpecWith (State, Maybe Server)
schemaTests _opts =
  -- we introspect the schema and validate it
  it "graphql-schema" \(state, _) -> do
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
      (`shouldBeYaml` relationshipFieldArgsSchema)

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
executionTests opts = describe "execution" do
  -- fetches the relationship data
  it "related-data" \(state, _) -> do
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
  it "related-data-empty-array" \(state, _) -> do
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
  it "related-data-null" \(state, _) -> do
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
  it "related-data-non-null-and-null" \(state, _) -> do
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
              order_by: [{id: asc}]
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
permissionTests opts = describe "permission" do
  -- only the allowed rows on the target table are queryable
  it "only-allowed-rows" \(state, _) -> do
    let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
              order_by: [{id: asc}]
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
  it "only-allowed-columns" \(state, _) -> do
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
  it "aggregations-not-allowed" \(state, _) -> do
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
  it "aggregations-allowed" \(state, _) -> do
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
  it "no-query-limit" \(state, _) -> do
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
  it "user-limit-less-than-permission-limit" \(state, _) -> do
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
  it "user-limit-greater-than-permission-limit" \(state, _) -> do
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
  it "aggregations" \(state, _) -> do
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
  it "aggregations-query-limit" \(state, _) -> do
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
