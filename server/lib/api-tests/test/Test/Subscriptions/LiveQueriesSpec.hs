{-# LANGUAGE QuasiQuotes #-}

-- | Test that each supported backend is able to do a simple live query /
-- subscription.
module Test.Subscriptions.LiveQueriesSpec (spec) where

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
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
            }
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Schema

schema :: [Schema.Table]
schema =
  [ (Schema.table "example")
      { Schema.tableColumns =
          [ Schema.column "id" Schema.TInt,
            Schema.column "name" Schema.TStr
          ],
        Schema.tablePrimaryKey = ["id"],
        Schema.tableData = []
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = withSubscriptions $ do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Websockets-based live queries" do
    it "Sanity check" \(mkSubscription, testEnvironment) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment
      query <-
        mkSubscription
          [graphql|
            subscription {
              #{schemaName}_example {
                id
                name
              }
            }
          |]
      -- check initial query result
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example: []
              |]
            actual :: IO Value
            actual = getNextResponse query

        actual `shouldBe` expected

      -- add some data
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  insert_hasura_example:
                    affected_rows: 2
              |]
            actual :: IO Value
            actual =
              postGraphql
                testEnvironment
                [graphql|
                  mutation {
                    insert_#{schemaName}_example(
                      objects: [{id: 1, name: "A"}, {id: 2, name: "B"}]
                    ) {
                      affected_rows
                    }
                  }
                |]
        actual `shouldBe` expected

      -- fetch the next response
      do
        let expected :: Value
            expected =
              [yaml|
                data:
                  hasura_example:
                    - id: 1
                      name: "A"
                    - id: 2
                      name: "B"
              |]
            actual :: IO Value
            actual = getNextResponse query

        actual `shouldBe` expected
