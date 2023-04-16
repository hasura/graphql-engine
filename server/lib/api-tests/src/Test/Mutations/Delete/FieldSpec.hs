{-# LANGUAGE QuasiQuotes #-}

-- |
-- Deleting objects based on their fields.
--
-- https://hasura.io/docs/latest/mutations/postgres/delete/#delete-objects-based-on-their-fields
-- https://hasura.io/docs/latest/mutations/ms-sql-server/delete/#delete-objects-based-on-their-fields
module Test.Mutations.Delete.FieldSpec (spec) where

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
          [ Schema.reference "author_id" "author" "id"
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

tests :: SpecWith TestEnvironment
tests = do
  it "Delete objects based on their fields" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation delete_low_rated_articles {
                delete_#{schemaName}_article(where: {rating: {_lt: 3}}) {
                  affected_rows
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              delete_#{schemaName}_article:
                affected_rows: 8
          |]

    shouldReturnYaml testEnvironment actual expected
