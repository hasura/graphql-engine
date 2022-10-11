{-# LANGUAGE QuasiQuotes #-}

-- |
-- Updating an object identified by its fields.
--
-- https://hasura.io/docs/latest/mutations/postgres/update/#update-objects-based-on-their-fields
-- https://hasura.io/docs/latest/mutations/ms-sql-server/update/#update-objects-based-on-their-fields
module Test.Mutations.Update.FieldSpec where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Citus)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Citus.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnv, _) ->
                [ Cockroach.setupTablesAction schema testEnv
                ],
              Fixture.customOptions =
                Just $
                  Fixture.defaultOptions
                    { Fixture.stringifyNumbers = True
                    }
            },
          (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlserver.setupTablesAction schema testEnvironment
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
          [ Schema.column "id" Schema.TInt,
            Schema.column "title" Schema.TStr,
            Schema.column "content" Schema.TStr,
            Schema.column "author_id" Schema.TInt,
            Schema.column "rating" Schema.TInt,
            Schema.column "is_published" Schema.TBool
          ],
        tablePrimaryKey = ["id"],
        tableReferences =
          [ Schema.Reference "author_id" "author" "id"
          ],
        tableData =
          [ [ Schema.VInt 1,
              Schema.VStr "Article 1",
              Schema.VStr "Content 1",
              Schema.VInt 1,
              Schema.VInt 2,
              Schema.VBool True
            ],
            [ Schema.VInt 2,
              Schema.VStr "Article 2",
              Schema.VStr "Content 2",
              Schema.VInt 2,
              Schema.VInt 5,
              Schema.VBool True
            ],
            [ Schema.VInt 3,
              Schema.VStr "Article 3",
              Schema.VStr "Content 3",
              Schema.VInt 3,
              Schema.VInt 1,
              Schema.VBool True
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  it "Update an article by its fields" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation update_article {
                update_#{schemaName}_article(
                  where: {rating: {_lte: 2}},
                  _set: {
                    rating: 1,
                    is_published: false
                  }
                ) {
                  affected_rows
                  returning {
                    id
                    title
                    rating
                  }
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              update_#{schemaName}_article:
                  affected_rows: 2
                  returning:
                  - id: 1
                    title: "Article 1"
                    rating: 1
                  - id: 3
                    title: "Article 3"
                    rating: 1
          |]

    actual `shouldBe` expected
