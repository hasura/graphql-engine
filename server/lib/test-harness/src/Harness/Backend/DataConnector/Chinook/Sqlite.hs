{-# LANGUAGE QuasiQuotes #-}

-- | Sqlite Chinook Agent Configuration
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Sqlite
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
    { backendType = BackendType.DataConnectorSqlite,
      backendSourceName = "chinook_sqlite",
      backendCapabilities =
        Just
          [yaml|
            data_schema:
              supports_primary_keys: true
              supports_foreign_keys: true
            scalar_types:
              DateTime: {}
            queries: {}
            relationships: {}
            comparisons:
              subquery:
                supports_relations: true
            explain: {}
            metrics: {}
            raw: {}
        |],
      backendTypeString = "sqlite",
      backendDisplayNameString = "Hasura SQLite (sqlite)",
      backendServerUrl = Just "http://localhost:65007",
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
    uri: "http://127.0.0.1:65007/"
|]

-- | Sqlite Agent specific @sources@ entry @configuration@ field.
sourceConfiguration :: Aeson.Value
sourceConfiguration =
  [yaml|
db: "/db.chinook.sqlite"
|]
