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
import Data.List.NonEmpty qualified as NE
import Data.List.Split (dropBlanks, keepDelimsL, split, whenElt)
import Data.Maybe qualified as Unsafe (fromJust)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types qualified as Morpheus
import Data.Typeable (Typeable)
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as SQLServer
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Test.Context (Context (..))
import Harness.Test.Context qualified as Context
import Harness.Test.Schema (Table (..))
import Harness.Test.Schema qualified as Schema
import Harness.Test.SchemaName
import Harness.TestEnvironment (Server, TestEnvironment, stopServer)
import Harness.Yaml (shouldBeYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = Context.runWithLocalTestEnvironment contexts tests
  where
    lhsContexts = [lhsPostgres, lhsSQLServer, lhsRemoteServer]
    rhsContexts = [rhsPostgres, rhsSQLServer]
    contexts = NE.fromList $ combine <$> lhsContexts <*> rhsContexts

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
      teardown = \testEnvironment@(globalTestEnvironment, _) -> do
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
artist :: Schema.Table
artist =
  (Schema.table "artist")
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
          [Schema.VInt 3, Schema.VStr "album3_artist1", Schema.VInt 1],
          [Schema.VInt 4, Schema.VStr "album4_artist2", Schema.VInt 2]
        ]
    }

--------------------------------------------------------------------------------
-- LHS Postgres

lhsPostgresMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsPostgresMkLocalTestEnvironment _ = pure Nothing

lhsPostgresSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresSetup rhsTableName (testEnvironment, _) = do
  let schemaName = getSchemaName testEnvironment

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
  Postgres.createTable testEnvironment artist
  Postgres.insertTable artist
  Schema.trackTable Context.Postgres sourceName artist testEnvironment

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

lhsPostgresTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsPostgresTeardown _ = Postgres.dropTable artist

--------------------------------------------------------------------------------
-- LHS SQLServer

lhsSQLServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsSQLServerMkLocalTestEnvironment _ = pure Nothing

lhsSQLServerSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerSetup rhsTableName (testEnvironment, _) = do
  let schemaName = getSchemaName testEnvironment

  let sourceName = "source"
      sourceConfig = SQLServer.defaultSourceConfiguration
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
  SQLServer.createTable artist
  SQLServer.insertTable artist
  Schema.trackTable Context.SQLServer sourceName artist testEnvironment

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
      name: artist
    permission:
      columns: '*'
      filter: {}
- type: mssql_create_select_permission
  args:
    source: *sourceName
    role: role2
    table:
      schema: *schemaName
      name: artist
    permission:
      columns: '*'
      filter: {}
- type: mssql_create_remote_relationship
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

lhsSQLServerTeardown :: (TestEnvironment, Maybe Server) -> IO ()
lhsSQLServerTeardown _ = SQLServer.dropTable artist

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

lhsRemoteServerMkLocalTestEnvironment :: TestEnvironment -> IO (Maybe Server)
lhsRemoteServerMkLocalTestEnvironment _ = do
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
          limitFunction = maybe id take aa_limit
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
        (Just 3, "artist3_no_albums"),
        (Nothing, "artist4_no_id")
      ]
    mkArtist (artistId, artistName) =
      HasuraArtist
        { a_id = pure artistId,
          a_name = pure $ Just artistName
        }

lhsRemoteServerSetup :: Value -> (TestEnvironment, Maybe Server) -> IO ()
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
rhsPostgresSetup (testEnvironment, _) = do
  let schemaName = getSchemaName testEnvironment
  let sourceName = "target"
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
  Postgres.createTable testEnvironment album
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
      limit: 2
      allow_aggregations: true
  |]

rhsPostgresTeardown :: (TestEnvironment, ()) -> IO ()
rhsPostgresTeardown _ = Postgres.dropTable album

--------------------------------------------------------------------------------
-- RHS SQLServer

rhsSQLServerSetup :: (TestEnvironment, ()) -> IO ()
rhsSQLServerSetup (testEnvironment, _) = do
  let schemaName = getSchemaName testEnvironment

  let sourceName = "target"
      sourceConfig = SQLServer.defaultSourceConfiguration
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
      limit: 2
      allow_aggregations: true
  |]

rhsSQLServerTeardown :: (TestEnvironment, ()) -> IO ()
rhsSQLServerTeardown _ = SQLServer.dropTable album

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
tests opts = describe "array-relationship" do
  schemaTests opts
  executionTests opts
  permissionTests opts

schemaTests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
schemaTests _opts =
  -- we introspect the schema and validate it
  it "graphql-schema" \(testEnvironment, _) -> do
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
    introspectionResult <- GraphqlEngine.postGraphql testEnvironment query
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
executionTests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
executionTests opts = describe "execution" do
  -- fetches the relationship data
  it "related-data" \(testEnvironment, _) -> do
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
               - title: album3_artist1
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when there are no matching rows, the relationship response should be []
  it "related-data-empty-array" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            artist: hasura_artist(where: {name: {_eq: "artist3_no_albums"}}) {
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when any of the join columns are null, the relationship should be null
  it "related-data-null" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            artist: hasura_artist(where: {name: {_eq: "artist4_no_id"}}) {
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

  -- when the lhs response has both null and non-null values for join columns
  it "related-data-non-null-and-null" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            artist: hasura_artist(
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
      opts
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

-- TODO:
-- 1. where
-- 1. limit with order_by
-- 1. offset
-- 1. _aggregate

-- | tests that describe an array relationship's data in the presence of permisisons
permissionTests :: Context.Options -> SpecWith (TestEnvironment, Maybe Server)
permissionTests opts = describe "permission" do
  -- only the allowed rows on the target table are queryable
  it "only-allowed-rows" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role1"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
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
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- we use an introspection query to check column permissions:
  -- 1. the type 'hasura_album' has only 'artist_id' and 'title', the allowed columns
  -- 2. the albums field in 'hasura_artist' type is of type 'hasura_album'
  it "only-allowed-columns" \(testEnvironment, _) -> do
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
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- _aggregate field should not be generated when 'allow_aggregations' isn't set to 'true'
  it "aggregations-not-allowed" \(testEnvironment, _) -> do
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
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- _aggregate field should only be allowed when 'allow_aggregations' is set to 'true'
  it "aggregations-allowed" \(testEnvironment, _) -> do
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
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- permission limit should kick in when no query limit is specified
  it "no-query-limit" \(testEnvironment, _) -> do
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
               - title: album2_artist1
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- query limit should be applied when query limit <= permission limit
  it "user-limit-less-than-permission-limit" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
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
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- permission limit should be applied when query limit > permission limit
  it "user-limit-greater-than-permission-limit" \(testEnvironment, _) -> do
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
               - title: album2_artist1
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- permission limit should only apply on 'nodes' but not on 'aggregate'
  it "aggregations" \(testEnvironment, _) -> do
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
                   count: 3
                 nodes:
                 - title: album1_artist1
                 - title: album2_artist1
          |]
    shouldReturnYaml
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse

  -- query limit applies to both 'aggregate' and 'nodes'
  it "aggregations-query-limit" \(testEnvironment, _) -> do
    let userHeaders = [("x-hasura-role", "role2"), ("x-hasura-artist-id", "1")]
        query =
          [graphql|
          query {
            artist: hasura_artist(
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
      opts
      (GraphqlEngine.postGraphqlWithHeaders testEnvironment userHeaders query)
      expectedResponse
