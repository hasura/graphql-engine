{-# LANGUAGE QuasiQuotes #-}

-- | Testing the encoding of array literals
module Test.Regression.ArrayLiteralTextEncodingSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine (postGraphql)
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema (Table (..), table)
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (table "table")
      { tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "str" Schema.TStr
          ],
        tablePrimaryKey = ["id"],
        tableData =
          [ [Schema.VInt 1, Schema.VStr "a,b"],
            [Schema.VInt 2, Schema.VStr "a,"],
            [Schema.VInt 3, Schema.VStr ""],
            [Schema.VInt 4, Schema.VStr ","],
            [Schema.VInt 5, Schema.VStr "\\"],
            [Schema.VInt 6, Schema.VStr "\""],
            [Schema.VInt 7, Schema.VStr "&"],
            [Schema.VInt 8, Schema.VStr "c"],
            [Schema.VInt 9, Schema.VStr "שלום"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  it "Query is a,b or c" \testEnvironment -> do
    let expected :: Value
        expected =
          [yaml|
            data:
              hasura_table:
              - id: 1
                str: a,b
              - id: 8
                str: c
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
                query {
                  hasura_table(where: {str: {_in: ["a,b", "c"]}}, order_by: { id: asc }) {
                    id
                    str
                  }
                }
              |]

    actual `shouldBe` expected

  it "Query is not a, empty, comma, or quote" \testEnvironment -> do
    let expected :: Value
        expected =
          [yaml|
            data:
              hasura_table:
              - id: 1
                str: a,b
              - id: 5
                str: \
              - id: 7
                str: '&'
              - id: 8
                str: c
          |]

        actual :: IO Value
        actual =
          postGraphql
            testEnvironment
            [graphql|
                query {
                  hasura_table(where: {str: {_nin: ["a,", "", ",", "\"", "שלום"]}}, order_by: { id: asc }) {
                    id
                    str
                  }
                }
              |]

    actual `shouldBe` expected
