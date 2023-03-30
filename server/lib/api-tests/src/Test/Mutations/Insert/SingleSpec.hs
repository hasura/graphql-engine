{-# LANGUAGE QuasiQuotes #-}

-- |
-- Insert a single object.
--
-- https://hasura.io/docs/latest/mutations/postgres/insert/#insert-a-single-object
-- https://hasura.io/docs/latest/mutations/ms-sql-server/insert/#insert-a-single-object
module Test.Mutations.Insert.SingleSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
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
          [ Schema.column "id" Schema.defaultSerialType,
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
          [ Schema.column "id" Schema.TInt,
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
  it "Adds an author" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation insert_single_article {
                insert_#{schemaName}_article_one(
                  object: {
                    id: 1,
                    title: "Article 1",
                    content: "Sample article content",
                    author_id: 3
                  }
                ) {
                  id
                  title
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              insert_#{schemaName}_article_one:
                id: 1
                title: "Article 1"
          |]

    shouldReturnYaml testEnvironment actual expected
