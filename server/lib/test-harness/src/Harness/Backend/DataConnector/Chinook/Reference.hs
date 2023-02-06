{-# LANGUAGE QuasiQuotes #-}

-- | In Memory Chinook Reference Agent Configuration.
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Reference
  ( backendTypeConfig,
    mkChinookStaticTestEnvironment,
    chinookFixture,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Managed (Managed)
import Data.Aeson qualified as Aeson
import Harness.Backend.DataConnector.Chinook (ChinookTestEnv, NameFormatting (..))
import Harness.Backend.DataConnector.Chinook qualified as Chinook
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture (Fixture (..), FixtureName (..))
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

--------------------------------------------------------------------------------

backendTypeConfig :: BackendType.BackendTypeConfig
backendTypeConfig =
  BackendType.BackendTypeConfig
    { backendType = BackendType.DataConnectorReference,
      backendSourceName = "chinook_reference",
      backendCapabilities =
        Just
          [yaml|
            data_schema:
              supports_primary_keys: true
              supports_foreign_keys: true
            queries: {}
            relationships: {}
            comparisons:
              subquery:
                supports_relations: true
            scalar_types:
              DateTime:
                comparison_operators:
                  same_day_as: DateTime
                  in_year: Int
                aggregate_functions:
                  max: DateTime
                  min: DateTime
                graphql_type: String
              number:
                aggregate_functions:
                  max: number
                  min: number
                  stddev: number
                  stddev_pop: number
                  stddev_samp: number
                  sum: number
                  var_pop: number
                  var_samp: number
                  variance: number
                update_column_operators:
                  inc:
                    argument_type: number
                graphql_type: Float
              string:
                aggregate_functions:
                  longest: string
                  max: string
                  min: string
                  shortest: string
                graphql_type: String
        |],
      backendTypeString = "reference",
      backendDisplayNameString = "reference",
      backendServerUrl = Just "http://localhost:65005",
      backendSchemaKeyword = "schema"
    }

--------------------------------------------------------------------------------

mkChinookStaticTestEnvironment :: TestEnvironment -> Managed ChinookTestEnv
mkChinookStaticTestEnvironment = Chinook.mkChinookStaticTestEnvironment nameFormatting sourceConfiguration

nameFormatting :: NameFormatting
nameFormatting = NameFormatting id id id

-- | Reference Agent specific @sources@ entry @configuration@ field.
sourceConfiguration :: Aeson.Value
sourceConfiguration =
  [yaml|
    value: {}
    template:
    timeout:
  |]

chinookFixture :: Fixture ChinookTestEnv
chinookFixture =
  Fixture
    { name = Backend backendTypeConfig,
      mkLocalTestEnvironment = mkChinookStaticTestEnvironment,
      setupTeardown = \testEnvs ->
        [Chinook.setupChinookSourceAction testEnvs],
      customOptions = Nothing
    }
