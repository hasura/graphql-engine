{-# LANGUAGE QuasiQuotes #-}

-- | Tests that the connection pool can be configured in different ways for SqlServer.
module Test.Databases.SQLServer.ConnectionPoolSpec (spec) where

import Data.Aeson (Value)
import Data.List.NonEmpty qualified as NE
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.Constants qualified as Constants
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment
import Harness.Yaml
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith GlobalTestEnvironment
spec =
  Fixture.run
    ( NE.fromList
        [ (Fixture.fixture $ Fixture.Backend Sqlserver.backendTypeMetadata)
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith TestEnvironment
tests = do
  describe "Connection pool" do
    it "Can be disabled" \testEnvironment -> do
      GraphqlEngine.setSource testEnvironment (sourceMetadataNoPool testEnvironment) Nothing
      actual <- GraphqlEngine.exportMetadata testEnvironment

      actual
        `shouldAtLeastBe` [yaml|
          sources:
            - configuration:
                connection_info:
                  pool_settings:
                    enable: false
        |]

sourceMetadataNoPool :: TestEnvironment -> Value
sourceMetadataNoPool TestEnvironment {uniqueTestId} =
  let source = Fixture.backendSourceName Sqlserver.backendTypeMetadata
      backendType = Fixture.backendTypeString Sqlserver.backendTypeMetadata
      sourceConfiguration = defaultSourceConfiguration
   in [yaml|
        name: *source
        kind: *backendType
        tables: []
        configuration: *sourceConfiguration
      |]
  where
    defaultSourceConfiguration :: Value
    defaultSourceConfiguration =
      [yaml|
      connection_info:
        database_url: *sqlserverConnectInfo
        pool_settings:
          enable: false
      |]

    sqlserverConnectInfo = Constants.sqlserverConnectInfo uniqueTestId
