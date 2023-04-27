{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- Native Queries is a pro-only feature now, this ensures that this continues
-- to be the case
module Test.Queries.NativeQueriesSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

featureFlagForNativeQueries :: String
featureFlagForNativeQueries = "HASURA_FF_NATIVE_QUERY_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv [(featureFlagForNativeQueries, "True")] $
    Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
                  ]
              }
          ]
      )
      tests

-- ** Setup and teardown

schema :: [Schema.Table]
schema = []

tests :: SpecWith TestEnvironment
tests = do
  let helloWorldLogicalModel :: Schema.LogicalModel
      helloWorldLogicalModel =
        (Schema.logicalModel "hello_world_return_type")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "one" Schema.TStr,
                Schema.logicalModelScalar "two" Schema.TStr
              ]
          }

  describe "Testing Native Queries" $ do
    it "We cannot even set up a Logical Model in OSS" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          source = BackendType.backendSourceName backendTypeMetadata

      -- we expect this to fail
      void $
        GraphqlEngine.postMetadataWithStatus
          400
          testEnvironment
          (Schema.trackLogicalModelCommand source backendTypeMetadata helloWorldLogicalModel)
