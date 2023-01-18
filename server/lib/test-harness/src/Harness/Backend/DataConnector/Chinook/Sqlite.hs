{-# LANGUAGE QuasiQuotes #-}

-- | Sqlite Chinook Agent Configuration
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Sqlite
  ( agentConfig,
    sourceConfiguration,
    backendTypeMetadata,
    formatForeignKeyName,
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
              DateTime:
                comparison_operators:
                  _in_year: int
              string:
                comparison_operators:
                  _like: string
                  _glob: string
                aggregate_functions:
                  min: string
                  max: string
              decimal:
                comparison_operators:
                  _modulus_is_zero: decimal
                aggregate_functions:
                  min: decimal
                  max: decimal
                  sum: decimal
              number:
                comparison_operators:
                  _modulus_is_zero: number
                aggregate_functions:
                  min: number
                  max: number
                  sum: number
              bool:
                comparison_operators:
                  _and: bool
                  _or: bool
                  _nand: bool
                  _xor: bool
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
value:
  db: "/db.chinook.sqlite"
template:
timeout:
|]

-- | Construct foreign key relationship names.
formatForeignKeyName :: Text -> Text
formatForeignKeyName = \case
  "Artist" -> "ArtistId->Artist.ArtistId"
  x -> x
