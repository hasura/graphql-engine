{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests around inserting non-ASCII characters in @VARCHAR@ column type.
module Test.Databases.SQLServer.InsertVarcharColumnSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Schema (Table (..), table)
import Harness.Schema qualified as Schema
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
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
  [ (table "test")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "test" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "varchar(MAX)"
                  }
          ],
        tablePrimaryKey = ["id"]
      },
    (table "test_bin")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "test" do
              Schema.TCustomType
                Schema.defaultBackendScalarType
                  { Schema.bstMssql = Just "varchar(MAX) collate SQL_Latin1_General_CP437_BIN"
                  }
          ],
        tablePrimaryKey = ["id"]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Inserting into varchar columns" do
    it "Insert into varchar column with non ASCII value" $ \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_test:
                  returning:
                  - id: 1
                    test: "££££"
                  affected_rows: 1

            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_test(objects:
                    [ { id: 1, test: "££££" }
                    ]
                  ) {
                    affected_rows
                    returning {
                      id
                      test
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected

    it "Insert into collated varchar column with non ASCII value" \testEnvironment -> do
      let expected :: Value
          expected =
            [yaml|
              data:
                insert_hasura_test_bin:
                  returning:
                  - id: 1
                    test: "££££"
                  affected_rows: 1
            |]

          actual :: IO Value
          actual =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_hasura_test_bin(objects:
                    [ { id: 1, test: "££££" }
                    ]
                  ) {
                    affected_rows
                    returning {
                      id
                      test
                    }
                  }
                }
              |]

      shouldReturnYaml testEnvironment actual expected
