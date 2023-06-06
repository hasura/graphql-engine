{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
-- For runWithLocalTestEnvironmentSingleSetup
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Tests for remote relationships from remote schemas. Unlike the "ToX"
-- modules, this module specifically cares about the remote schema on the LHS:
--   - testing the metadata API
--   - testing how we generate queries to remote schemas for the purpose of
--     making joins against them.
module Test.Schema.RemoteRelationships.FromRemoteSchemaSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Types
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.RemoteServer qualified as RemoteServer
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture (Fixture (..), FixtureName (..))
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, Server, TestEnvironment (..), stopServer)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith GlobalTestEnvironment
spec = Fixture.runWithLocalTestEnvironmentSingleSetup (NE.fromList [context]) tests
  where
    context =
      (Fixture.fixture $ Fixture.RemoteGraphQLServer)
        { -- start only one remote server
          Fixture.mkLocalTestEnvironment = \_testEnvironment ->
            RemoteServer.run
              $ RemoteServer.generateQueryInterpreter
              $ Query
                { object = objectResolver,
                  writer = writerResolver,
                  artist = artistResolver,
                  objects = objectsResolver,
                  articles = articlesResolver
                },
          -- set that remote server as both source and target, for convenience
          -- start a RHS Postgres for Metadata tests only
          setupTeardown = \(testEnvironment, server) ->
            [ Fixture.SetupAction
                { Fixture.setupAction = GraphqlEngine.clearMetadata testEnvironment,
                  Fixture.teardownAction = \_ -> GraphqlEngine.clearMetadata testEnvironment
                },
              Fixture.SetupAction
                { Fixture.setupAction = rhsPostgresSetup testEnvironment,
                  Fixture.teardownAction = \_ -> rhsPostgresTeardown testEnvironment
                },
              Fixture.SetupAction
                { Fixture.setupAction = do
                    addRemoteSchema testEnvironment "remote" server
                    addRelationships testEnvironment,
                  Fixture.teardownAction = \_ -> stopServer server
                }
            ]
        }

-- | Add a remote schema to the engine with the given name.
addRemoteSchema :: TestEnvironment -> String -> Server -> IO ()
addRemoteSchema testEnvironment rsName remoteServer = do
  let remoteSchemaEndpoint = RemoteServer.graphqlEndpoint remoteServer
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: add_remote_schema
args:
  name: *rsName
  definition:
    url: *remoteSchemaEndpoint
    |]

-- | Create the remote relationships.
addRelationships :: TestEnvironment -> IO ()
addRelationships testEnvironment = do
  GraphqlEngine.postMetadata_
    testEnvironment
    [yaml|
type: bulk
args:
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: remote
    type_name: Writer
    name: articles
    definition:
      to_remote_schema:
        remote_schema: remote
        lhs_fields: [wIds]
        remote_field:
          articles:
            arguments:
              ids: $wIds
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: remote
    type_name: Artist
    name: articles
    definition:
      to_remote_schema:
        remote_schema: remote
        lhs_fields: [aIds]
        remote_field:
          articles:
            arguments:
              ids: $aIds
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: remote
    type_name: Article
    name: artist
    definition:
      to_remote_schema:
        remote_schema: remote
        lhs_fields: [aId]
        remote_field:
          artist:
            arguments:
              id: $aId
- type: create_remote_schema_remote_relationship
  args:
    remote_schema: remote
    type_name: Article
    name: writer
    definition:
      to_remote_schema:
        remote_schema: remote
        lhs_fields: [wId]
        remote_field:
          writer:
            arguments:
              id: $wId
    |]

--------------------------------------------------------------------------------
-- Remote schema

[gqlDocument|

type Query {
  object(id: Int!): Object
  writer(id: Int!): Writer
  artist(id: Int!): Artist
  objects(ids: [Int!]!): [Object]!
  articles(ids: [Int!]!): [Article]!
}

union Object = Writer | Artist | Article

type Writer {
  id: Int!
  name: String!
  wIds: [Int!]!
  local_articles: [Article!]!
}

type Artist {
  id: Int!
  name: String!
  aIds: [Int!]!
  local_articles: [Article!]!
  self: Artist!
}

type Article {
  id: Int!
  title: String!
  aId: Int!
  wId: Int!
}

|]

knownObjects :: (Monad m) => [(Int, Object m)]
knownObjects =
  [ (101, ObjectWriter writer1),
    (102, ObjectWriter writer2),
    (201, ObjectArtist artist1),
    (202, ObjectArtist artist2),
    (301, ObjectArticle article1),
    (302, ObjectArticle article2),
    (303, ObjectArticle article3),
    (304, ObjectArticle article4)
  ]
  where
    writer1 = Writer (pure 101) (pure "Writer1") (pure [301, 302]) (pure [article1, article2])
    writer2 = Writer (pure 102) (pure "Writer2") (pure [303, 304]) (pure [article3, article4])
    artist1 = Artist (pure 201) (pure "Artist1") (pure [301, 303]) (pure [article1, article3]) (pure artist1)
    artist2 = Artist (pure 202) (pure "Artist2") (pure [302, 304]) (pure [article2, article4]) (pure artist2)
    article1 = Article (pure 301) (pure "Article1") (pure 201) (pure 101)
    article2 = Article (pure 302) (pure "Article2") (pure 202) (pure 101)
    article3 = Article (pure 303) (pure "Article3") (pure 201) (pure 102)
    article4 = Article (pure 304) (pure "Article4") (pure 202) (pure 102)

objectResolver :: (Monad m) => Arg "id" Int -> m (Maybe (Object m))
objectResolver (Arg objectId) = pure $ lookup objectId knownObjects

writerResolver :: (Monad m) => Arg "id" Int -> m (Maybe (Writer m))
writerResolver (Arg objectId) =
  pure $ case lookup objectId knownObjects of
    Just (ObjectWriter w) -> Just w
    _ -> Nothing

artistResolver :: (Monad m) => Arg "id" Int -> m (Maybe (Artist m))
artistResolver (Arg objectId) =
  pure $ case lookup objectId knownObjects of
    Just (ObjectArtist a) -> Just a
    _ -> Nothing

objectsResolver :: (Monad m) => Arg "ids" [Int] -> m [Maybe (Object m)]
objectsResolver (Arg objectIds) = pure [lookup objectId knownObjects | objectId <- objectIds]

articlesResolver :: (Monad m) => Arg "ids" [Int] -> m [Maybe (Article m)]
articlesResolver (Arg objectIds) =
  pure
    $ objectIds
    <&> \objectId ->
      case lookup objectId knownObjects of
        Just (ObjectArticle a) -> Just a
        _ -> Nothing

--------------------------------------------------------------------------------
-- RHS Postgres (for metadata only)

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

-- | we have to create a PG database manually here because it's not a Postgres
-- Fixture test
rhsPostgresSetup :: TestEnvironment -> IO ()
rhsPostgresSetup testEnvironment = do
  Postgres.createDatabase testEnvironment
  let sourceName = "db"
      sourceConfig = Postgres.defaultSourceConfiguration testEnvironment
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
  Schema.trackTable sourceName track (testEnvironment {fixtureName = Backend Postgres.backendTypeMetadata})

rhsPostgresTeardown :: TestEnvironment -> IO ()
rhsPostgresTeardown testEnvironment = Postgres.dropDatabase testEnvironment

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith (TestEnvironment, Server)
tests = do
  -- tests metadata API
  metadataAPITests
  -- ensures setup is correct
  noJoinsTests
  simpleTests
  -- joins on neither part of the union
  joinArticleTests
  -- joins on parts of the union
  joinWriterTests
  joinArtistTests
  -- joins on deeply nested joins
  deeplyNestedJoinTests

metadataAPITests :: SpecWith (TestEnvironment, Server)
metadataAPITests = describe "metadata API" do
  it "adds a RS-RS relationship" \(testEnvironment, _) ->
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: create_remote_schema_remote_relationship
args:
  remote_schema: remote
  type_name: Writer
  name: nonsensical1
  definition:
    to_remote_schema:
      remote_schema: remote
      lhs_fields: [wIds]
      remote_field:
        articles:
          arguments:
            ids: $wIds
      |]
  it "adds a RS-RD relationship" \(testEnvironment, _) ->
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: create_remote_schema_remote_relationship
args:
  remote_schema: remote
  type_name: Writer
  name: nonsensical2
  definition:
    to_source:
      source: db
      table: {schema: hasura, name: track}
      relationship_type: object
      field_mapping:
        id: id
      |]
  it "updates a RS-RS relationship" \(testEnvironment, _) ->
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: update_remote_schema_remote_relationship
args:
  remote_schema: remote
  type_name: Writer
  name: nonsensical1
  definition:
    to_remote_schema:
      remote_schema: remote
      lhs_fields: [id]
      remote_field:
        artist:
          arguments:
            id: $id
      |]
  it "updates a RS-DB relationship" \(testEnvironment, _) ->
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: update_remote_schema_remote_relationship
args:
  remote_schema: remote
  type_name: Writer
  name: nonsensical2
  definition:
    to_source:
      source: db
      table: {schema: hasura, name: track}
      relationship_type: array
      field_mapping:
        id: id
      |]
  it "deletes a RS-RS relationship" \(testEnvironment, _) ->
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: delete_remote_schema_remote_relationship
args:
  remote_schema: remote
  type_name: Writer
  name: nonsensical1
      |]
  it "deletes a RS-DB relationship" \(testEnvironment, _) ->
    GraphqlEngine.postMetadata_
      testEnvironment
      [yaml|
type: delete_remote_schema_remote_relationship
args:
  remote_schema: remote
  type_name: Writer
  name: nonsensical2
      |]

-- | Ensure we don't insert `__hasura_internal_typename` when there are no
-- joins.
noJoinsTests :: SpecWith (TestEnvironment, Server)
noJoinsTests = describe "simple joins" do
  it "select objects without remote joins" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            obj101: object(id: 101) {
              __typename
            }
            obj201: object(id: 201) {
              __typename
            }
            obj301: object(id: 301) {
              __typename
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            obj101:
              __typename: Writer
            obj201:
              __typename: Artist
            obj301:
              __typename: Article

          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

simpleTests :: SpecWith (TestEnvironment, Server)
simpleTests = describe "simple joins" do
  it "joins writer against articles" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            writer(id: 101) {
              name
              articles {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            writer:
              name: "Writer1"
              articles:
               - title: Article1
               - title: Article2
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins no writer against articles" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            writer(id: 0) {
              name
              articles {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            writer: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins artist against articles" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            artist(id: 201) {
              name
              articles {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist:
              name: "Artist1"
              articles:
               - title: Article1
               - title: Article3
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins no artist against articles" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            artist(id: 0) {
              name
              articles {
                title
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            artist: null
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

joinArticleTests :: SpecWith (TestEnvironment, Server)
joinArticleTests = describe "join from article object" do
  it "does not join" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            object(id: 301) {
              ... on Article {
                title
              }
              ... on Artist {
                name
                articles {
                  title
                }
              }
              ... on Writer {
                name
                articles {
                  title
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            object:
              title: "Article1"
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

joinWriterTests :: SpecWith (TestEnvironment, Server)
joinWriterTests = describe "join from writer object" do
  it "joins against articles" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            object(id: 101) {
              ... on Article {
                title
              }
              ... on Artist {
                name
                articles {
                  title
                }
              }
              ... on Writer {
                name
                articles {
                  title
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            object:
              name: "Writer1"
              articles:
               - title: Article1
               - title: Article2
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

joinArtistTests :: SpecWith (TestEnvironment, Server)
joinArtistTests = describe "join from artist object" do
  it "joins against articles" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            object(id: 201) {
              ... on Article {
                title
              }
              ... on Artist {
                name
                articles {
                  title
                }
              }
              ... on Writer {
                name
                articles {
                  title
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            object:
              name: "Artist1"
              articles:
               - title: Article1
               - title: Article3
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse

deeplyNestedJoinTests :: SpecWith (TestEnvironment, Server)
deeplyNestedJoinTests = describe "join from artist object" do
  it "joins ambiguously nested articles depending on the full path" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            objects(ids: [101, 201]) {
              ... on Artist {
                local_articles { # local join
                  title
                  foo: writer { # remote join
                    bar: wIds
                    baz: articles { # remote join
                      title
                    }
                  }
                }
              }
              ... on Writer {
                local_articles { # local join
                  title
                  foo: artist { # remote join
                    bar: aIds
                    baz: articles { # remote join
                      title
                    }
                  }
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            objects:
            - local_articles:
              - title: Article1
                foo:
                  bar:
                  - 301
                  - 303
                  baz:
                  - title: Article1
                  - title: Article3
              - title: Article2
                foo:
                  bar:
                  - 302
                  - 304
                  baz:
                  - title: Article2
                  - title: Article4
            - local_articles:
              - title: Article1
                foo:
                  bar:
                  - 301
                  - 302
                  baz:
                  - title: Article1
                  - title: Article2
              - title: Article3
                foo:
                  bar:
                  - 303
                  - 304
                  baz:
                  - title: Article3
                  - title: Article4
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
  it "joins nested articles at different depths" \(testEnvironment, _) -> do
    let query =
          [graphql|
          query {
            objects(ids: [102, 202]) {
              ... on Artist {
                bar: self {
                  baz: articles {
                    title
                  }
                }
              }
              ... on Writer {
                bar: articles {
                  title
                }
              }
            }
          }
          |]
        expectedResponse =
          [yaml|
          data:
            objects:
            - bar:
              - title: Article3
              - title: Article4
            - bar:
                baz:
                - title: Article2
                - title: Article4
          |]
    shouldReturnYaml
      testEnvironment
      (GraphqlEngine.postGraphql testEnvironment query)
      expectedResponse
