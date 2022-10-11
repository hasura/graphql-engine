{-# LANGUAGE QuasiQuotes #-}

-- |
-- Testing manual, misconfigured object relationships.
-- Specifically, having manual relationships with one-to-many relationships.
-- Test case for bug reported at https://github.com/hasura/graphql-engine/issues/7936
module Test.Regression.ObjectRelationshipsLimit7936Spec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql, postMetadata_)
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnOneOfYaml, shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment,
                  setupMetadata testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "author")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr,
            Schema.column "createdAt" Schema.TUTCTime
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Author 1", Schema.parseUTCTimeOrError "2017-09-21 09:39:44"],
            [Schema.VInt 2, Schema.VStr "Author 2", Schema.parseUTCTimeOrError "2017-09-21 09:50:44"],
            [Schema.VInt 3, Schema.VStr "Author 1", Schema.parseUTCTimeOrError "2017-09-21 09:55:44"]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "author_name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableReferences = [], -- No references; we are using @manual_configuration@ to make the object relationship
        tableData =
          [ [Schema.VInt 1, Schema.VStr "Author 1"],
            [Schema.VInt 2, Schema.VStr "Author 2"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

-- | Many of these may return non-deterministic results because graphql-engine
-- need to choose one of the available rows in a misconfigured manual
-- relationship that has a one-to-many relationship instead of the expected
-- one-to-one.
--
-- Because of that, we use 'shouldReturnOneOfYaml' and list all of the possible
-- (valid) expected results.
tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBeOneOf :: IO Value -> [Value] -> IO ()
      shouldBeOneOf = shouldReturnOneOfYaml opts

      shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  it "Query by ID" \testEnvironment -> do
    let possibilities :: [Value]
        possibilities =
          [ [yaml|
              data:
                hasura_article:
                - id: 1
                  author:
                    id: 1
              |],
            [yaml|
              data:
                hasura_article:
                - id: 1
                  author:
                    id: 3
              |]
          ]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
                query {
                  hasura_article(where: {id: {_eq: 1}}) {
                    id
                    author {
                      id
                    }
                  }
                }
              |]

    actual `shouldBeOneOf` possibilities

  it "Query limit 2" \testEnvironment -> do
    let possibilities :: [Value]
        possibilities =
          [ [yaml|
              data:
                hasura_article:
                - id: 1
                  author:
                    id: 1
                - id: 2
                  author:
                    id: 2
              |],
            [yaml|
              data:
                hasura_article:
                - id: 1
                  author:
                    id: 3
                - id: 2
                  author:
                    id: 2
              |]
          ]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                hasura_article(limit: 2) {
                  id
                  author {
                    id
                  }
                }
              }
            |]

    actual `shouldBeOneOf` possibilities

  it "... where author name" \testEnvironment -> do
    let possibilities :: [Value]
        possibilities =
          [ [yaml|
              data:
                hasura_article:
                - id: 1
                  author:
                    id: 1
              |],
            [yaml|
              data:
                hasura_article:
                - id: 1
                  author:
                    id: 3
              |]
          ]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                hasura_article(where: { author: { name: { _eq: "Author 1" } } }) {
                  id
                  author {
                    id
                  }
                }
              }
            |]

    actual `shouldBeOneOf` possibilities

  it "Order by author id" \testEnvironment -> do
    let possibilities :: [Value]
        possibilities =
          [ [yaml|
            data:
              hasura_article:
              - id: 1
                author:
                  id: 1
              - id: 2
                author:
                  id: 2
            |],
            [yaml|
            data:
              hasura_article:
              - id: 2
                author:
                  id: 2
              - id: 1
                author:
                  id: 3
            |]
          ]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                hasura_article(order_by: {author: {id: asc}}) {
                  id
                  author {
                    id
                  }
                }
              }
            |]

    actual `shouldBeOneOf` possibilities

  it "Count articles" \testEnvironment -> do
    let expected :: Value
        expected =
          [yaml|
            data:
              hasura_article_aggregate:
                aggregate:
                  count: 2
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              query {
                hasura_article_aggregate {
                  aggregate {
                    count
                  }
                }
              }
            |]

    actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Metadata

setupMetadata :: TestEnvironment -> Fixture.SetupAction
setupMetadata testEnvironment = do
  let setup :: IO ()
      setup =
        postMetadata_
          testEnvironment
          [yaml|
              type: pg_create_object_relationship
              args:
                source: postgres
                table:
                  schema: hasura
                  name: article
                name: author
                using:
                  manual_configuration:
                     remote_table:
                       schema: hasura
                       name: author
                     column_mapping:
                       author_name: name
            |]

      teardown :: IO ()
      teardown =
        postMetadata_
          testEnvironment
          [yaml|
              type: pg_drop_relationship
              args:
                source: postgres
                table:
                  schema: hasura
                  name: article
                relationship: author
            |]

  Fixture.SetupAction setup \_ -> teardown
