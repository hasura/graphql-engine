{-# LANGUAGE QuasiQuotes #-}

-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Data.Aeson (Value (Null))
import Harness.Backend.Postgres qualified as Postgres
import Harness.Quoter.Yaml (shouldReturnYaml, yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.Test.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

spec :: SpecWith TestEnvironment
spec =
  Fixture.run
    [ (Fixture.fixture $ Fixture.Backend Fixture.Postgres)
        { Fixture.setupTeardown = \(testEnvironment, _) ->
            [ Postgres.setupTablesAction schema testEnvironment
            ]
        }
    ]
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
tests opts = do
  let shouldBe :: IO Value -> Value -> IO ()
      shouldBe = shouldReturnYaml opts

  describe "Example test" do
    it "Works as expected" \testEnvironment -> do
      let expected :: Value
          expected = [yaml| null |]

          actual :: IO Value
          actual = pure Null

      logger testEnvironment "A log message\n"
      actual `shouldBe` expected
