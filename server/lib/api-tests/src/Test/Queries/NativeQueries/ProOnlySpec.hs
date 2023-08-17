{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- Native Queries is a pro-only feature now for anything but Postgres.
-- This test ensures that this continues to be the case.
module Test.Queries.NativeQueries.ProOnlySpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Hasura.Prelude
import Test.Hspec (SpecWith, describe, it)

-- ** Preamble

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.hgeWithEnv []
    $ Fixture.runClean -- re-run fixture setup on every test
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnvironment, _) ->
                  [ Postgres.setupTablesAction schema testEnvironment
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

-- ** Setup and teardown

schema :: [Schema.Table]
schema = []

tests :: SpecWith TestEnvironment
tests = do
  let query :: Text
      query = "SELECT * FROM (VALUES ('hello', 'world'), ('welcome', 'friend')) as t(\"one\", \"two\")"

      helloWorldLogicalModel :: Schema.LogicalModel
      helloWorldLogicalModel =
        (Schema.logicalModel "hello_world_return_type")
          { Schema.logicalModelColumns =
              [ Schema.logicalModelScalar "one" Schema.TStr,
                Schema.logicalModelScalar "two" Schema.TStr
              ]
          }

      helloWorldNativeQuery :: Schema.NativeQuery
      helloWorldNativeQuery =
        (Schema.nativeQuery "hello_world_function" (const query) "hello_world_return_type")

  describe "Testing Native Queries" $ do
    it "We cannot even set up a Logical Model in non-pg OSS" $ \testEnvironment -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          backendName = BackendType.backendTypeString backendTypeMetadata
          source = BackendType.backendSourceName backendTypeMetadata

      GraphqlEngine.postMetadata_
        testEnvironment
        (Schema.trackLogicalModelCommand source backendTypeMetadata helloWorldLogicalModel)

      -- we expect this to work only on Postgres
      let expectedStatus
            | backendName == "pg" = 200
            | otherwise = 400

      void
        $ GraphqlEngine.postMetadataWithStatus
          expectedStatus
          testEnvironment
          (Schema.trackNativeQueryCommand source backendTypeMetadata helloWorldNativeQuery)
