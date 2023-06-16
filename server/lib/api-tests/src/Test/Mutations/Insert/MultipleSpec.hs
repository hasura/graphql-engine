{-# LANGUAGE QuasiQuotes #-}

-- |
-- Insert multiple objects of the same type in the same mutation
--
-- https://hasura.io/docs/latest/mutations/postgres/insert/#insert-multiple-objects-of-the-same-type-in-the-same-mutation
-- https://hasura.io/docs/latest/mutations/ms-sql-server/insert/#insert-multiple-objects-of-the-same-type-in-the-same-mutation
module Test.Mutations.Insert.MultipleSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Sqlserver.setupTablesAction schema testEnv
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
            Schema.column "name" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Justin"
            ],
            [ Schema.VInt 2,
              Schema.VStr "Beltran"
            ],
            [ Schema.VInt 3,
              Schema.VStr "Sidney"
            ],
            [ Schema.VInt 4,
              Schema.VStr "Anjela"
            ]
          ]
      },
    (table "article")
      { tableColumns =
          [ Schema.column "id" Schema.defaultSerialType,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "author_id" Schema.TInt
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.reference "author_id" "author" "id"
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Adds a list of articles" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation insert_multiple_articles {
                insert_#{schemaName}_article(
                  objects: [
                    {
                      title: "Article 1",
                      content: "Sample article content",
                      author_id: 1
                    },
                    {
                      title: "Article 2",
                      content: "Sample article content",
                      author_id: 2
                    }
                  ]
                ) {
                  returning {
                    id
                    title
                  }
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_article:
                returning:
                  - id: 1
                    title: "Article 1"
                  - id: 2
                    title: "Article 2"
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Adds an empty list of authors" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation insert_multiple_articles {
                insert_#{schemaName}_article(
                  objects: []
                ) {
                  returning {
                    id
                    title
                  }
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_article:
                returning: []
          |]

    shouldReturnYaml testEnvironment actual expected
