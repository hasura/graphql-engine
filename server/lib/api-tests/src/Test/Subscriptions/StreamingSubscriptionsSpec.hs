{-# LANGUAGE QuasiQuotes #-}

-- | Test that each supported backend is able to do a simple streaming
-- subscriptions request.
module Test.Subscriptions.StreamingSubscriptionsSpec (spec) where

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml (yaml)
import Harness.Subscriptions
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment (..))
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import System.Timeout (timeout)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec qualified as Hspec

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
            },
          (Fixture.fixture $ Fixture.Backend Fixture.Cockroach)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Cockroach.setupTablesAction schema testEnvironment
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
        Schema.tableData =
          [ [Schema.VInt 1, Schema.VStr "A"]
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Tests

tests :: Fixture.Options -> SpecWith TestEnvironment
tests opts = withSubscriptions $ do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Streaming subscriptions" do
    it "Receives new updates in order" \(mkSubscription, testEnvironment) -> do
      let schemaName :: Schema.SchemaName
          schemaName = Schema.getSchemaName testEnvironment

      subscriptionHandle <-
        mkSubscription
          [graphql|
            subscription {
              #{schemaName}_example_stream(
                batch_size: 1,
                cursor: {
                  initial_value: { id: 0 },
                  ordering: ASC
                }
              ) {
                id
                name
              }
            }
          |]
          []

      getNextResponse subscriptionHandle
        `shouldBe` [yaml|
        data:
          hasura_example_stream:
          - id: 1
            name: "A"
      |]

      let addFirstRow :: IO Value
          addFirstRow =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_#{schemaName}_example(
                    objects: [{ id: 2, name: "B" }]
                  ) {
                    affected_rows
                  }
                }
              |]

      addFirstRow
        `shouldBe` [yaml|
        data:
          insert_hasura_example:
            affected_rows: 1
      |]

      getNextResponse subscriptionHandle
        `shouldBe` [yaml|
        data:
          hasura_example_stream:
            - id: 2
              name: "B"
      |]

      let addSecondRow :: IO Value
          addSecondRow =
            postGraphql
              testEnvironment
              [graphql|
                mutation {
                  insert_#{schemaName}_example(
                    objects: [{ id: 3, name: "C" }]
                  ) {
                    affected_rows
                  }
                }
              |]

      addSecondRow
        `shouldBe` [yaml|
        data:
          insert_hasura_example:
            affected_rows: 1
      |]

      getNextResponse subscriptionHandle
        `shouldBe` [yaml|
        data:
          hasura_example_stream:
            - id: 3
              name: "C"
      |]

      let twoSeconds = fromIntegral $ diffTimeToMicroSeconds (seconds 2)
          mapOutput = maybe (Left @String "Expecting no further messsages") pure
      result <- timeout twoSeconds (getNextResponse subscriptionHandle)
      mapOutput result `Hspec.shouldBe` mapOutput Nothing
