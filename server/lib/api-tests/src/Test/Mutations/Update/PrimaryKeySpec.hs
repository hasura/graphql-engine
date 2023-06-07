{-# LANGUAGE QuasiQuotes #-}

-- |
-- Updating an object identified by primary key.
--
-- https://hasura.io/docs/latest/mutations/postgres/update/#update-an-object-by-its-primary-key
-- https://hasura.io/docs/latest/mutations/ms-sql-server/update/#update-an-object-by-its-primary-key
module Test.Mutations.Update.PrimaryKeySpec (spec) where

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
              Schema.VBool False
            ]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  it "Update an article by its primary key" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation update_an_article {
                update_#{schemaName}_article_by_pk (
                  pk_columns: { id: 1 }
                  _set: { is_published: true }
                ) {
                  id
                  is_published
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              update_#{schemaName}_article_by_pk:
                id: 1
                is_published: true
          |]

    shouldReturnYaml testEnvironment actual expected

  it "Does nothing for nonexistent primary keys" \testEnvironment -> do
    let schemaName :: Schema.SchemaName
        schemaName = Schema.getSchemaName testEnvironment

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
              mutation update_an_article {
                update_#{schemaName}_article_by_pk (
                  pk_columns: { id: 2 }
                  _set: { is_published: true }
                ) {
                  id
                  is_published
                }
              }
            |]

        expected :: Value
        expected =
          [interpolateYaml|
            data:
              update_#{schemaName}_article_by_pk: null
          |]

    shouldReturnYaml testEnvironment actual expected
