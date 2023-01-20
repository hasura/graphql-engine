{-# LANGUAGE QuasiQuotes #-}

-- | In Memory Chinook Reference Agent Configuration.
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Reference
  ( agentConfig,
    sourceConfiguration,
    backendTypeMetadata,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Hasura.Prelude

--------------------------------------------------------------------------------

backendTypeMetadata :: BackendType.BackendTypeConfig
backendTypeMetadata =
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

-- | Reference Agent @backend_configs@ field.
agentConfig :: Aeson.Value
agentConfig =
  let backendType = BackendType.backendTypeString backendTypeMetadata
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65005/"
|]

-- | Reference Agent specific @sources@ entry @configuration@ field.
sourceConfiguration :: Aeson.Value
sourceConfiguration =
  [yaml|
value: {}
template:
timeout:
|]
