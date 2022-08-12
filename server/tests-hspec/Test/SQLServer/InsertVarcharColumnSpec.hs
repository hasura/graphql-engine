{-# LANGUAGE QuasiQuotes #-}

-- |
-- Tests around inserting non-ASCII characters in @VARCHAR@ column type.
module Test.SQLServer.InsertVarcharColumnSpec (spec) where

import Data.Aeson (Value)
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec = do
  Fixture.run
    [ (Fixture.fixture $ Fixture.Backend Fixture.SQLServer)
        { Fixture.setupTeardown = \(testEnv, _) ->
            [ Sqlserver.setupTablesAction schema testEnv
            ]
        }
    ]
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

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

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

      actual `shouldBe` expected

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

      actual `shouldBe` expected
