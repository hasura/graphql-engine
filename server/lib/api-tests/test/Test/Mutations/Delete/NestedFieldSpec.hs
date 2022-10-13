{-# LANGUAGE QuasiQuotes #-}

-- |
-- Delete objects based on nested objects' fields.
--
-- https://hasura.io/docs/latest/mutations/postgres/delete/#delete-objects-based-on-nested-objects-fields
-- https://hasura.io/docs/latest/mutations/ms-sql-server/delete/#delete-objects-based-on-nested-objects-fields
module Test.Mutations.Delete.NestedFieldSpec where

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
                ]
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
              Schema.VStr "Corny"
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
              Schema.VInt 1,
              Schema.VBool False
            ],
            [ Schema.VInt 2,
              Schema.VStr "Article 2",
              Schema.VStr "Content 2",
              Schema.VInt 2,
              Schema.VInt 2,
              Schema.VBool False
            ],
            [ Schema.VInt 3,
              Schema.VStr "Article 3",
              Schema.VStr "Content 3",
              Schema.VInt 3,
              Schema.VInt 3,
              Schema.VBool False
            ],
            [ Schema.VInt 4,
              Schema.VStr "Article 4",
              Schema.VStr "Content 4",
              Schema.VInt 4,
              Schema.VInt 1,
              Schema.VBool False
            ],
            [ Schema.VInt 5,
              Schema.VStr "Article 5",
              Schema.VStr "Content 5",
              Schema.VInt 1,
              Schema.VInt 2,
              Schema.VBool False
            ],
            [ Schema.VInt 6,
              Schema.VStr "Article 6",
              Schema.VStr "Content 6",
              Schema.VInt 2,
              Schema.VInt 3,
              Schema.VBool False
            ],
            [ Schema.VInt 7,
              Schema.VStr "Article 7",
              Schema.VStr "Content 7",
              Schema.VInt 3,
              Schema.VInt 1,
              Schema.VBool False
            ],
            [ Schema.VInt 8,
              Schema.VStr "Article 8",
              Schema.VStr "Content 8",
              Schema.VInt 4,
              Schema.VInt 2,
              Schema.VBool False
            ],
            [ Schema.VInt 9,
              Schema.VStr "Article 9",
              Schema.VStr "Content 9",
              Schema.VInt 1,
              Schema.VInt 1,
              Schema.VBool False
            ],
            [ Schema.VInt 10,
              Schema.VStr "Article 10",
              Schema.VStr "Content 10",
              Schema.VInt 2,
              Schema.VInt 2,
              Schema.VBool False
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

  it "Delete objects based on nested objects' fields" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation delete_authors_article {
                delete_#{schemaName}_article(
                  where: {author_by_author_id_to_id: {name: {_eq: "Corny"}}}
                ) {
                  affected_rows
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              delete_#{schemaName}_article:
                affected_rows: 2
          |]

    actual `shouldBe` expected
