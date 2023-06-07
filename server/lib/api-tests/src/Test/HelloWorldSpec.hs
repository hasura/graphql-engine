{-# LANGUAGE QuasiQuotes #-}

-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Data.Aeson (Value (Null))
import Data.List.NonEmpty qualified as NE
import Harness.Backend.DataConnector.Sqlite qualified as Sqlite
import Harness.Backend.Postgres qualified as Postgres
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Services.GraphqlEngine (emptyHgeConfig, withHge)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment (..), testLogTrace)
import Harness.Yaml (shouldReturnYaml)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith GlobalTestEnvironment
spec = do
  withHge emptyHgeConfig do
    describe "An externally hosted HGE instance" do
      it "certainly runs" \_te -> do
        return @IO ()

  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Postgres.setupTablesAction schema testEnvironment
                ]
            },
          (Fixture.fixture $ Fixture.Backend Sqlite.backendTypeMetadata)
            { Fixture.setupTeardown = \(testEnvironment, _) ->
                [ Sqlite.setupTablesAction schema testEnvironment
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

tests :: SpecWith TestEnvironment
tests = do
  describe "Example test" do
    it "Works as expected" \testEnvironment -> do
      let expected :: Value
          expected = [yaml| null |]

          actual :: IO Value
          actual = pure Null

      testLogTrace testEnvironment ("A log message" :: Text)
      shouldReturnYaml testEnvironment actual expected
