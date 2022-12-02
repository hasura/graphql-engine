{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Definition of backend types and a few helper functions.
module Harness.Test.BackendType
  ( BackendType (..),
    pattern DataConnectorMock,
    pattern DataConnectorReference,
    pattern DataConnectorSqlite,
    defaultSource,
    defaultBackendDisplayNameString,
    defaultBackendTypeString,
    defaultBackendServerUrl,
    defaultBackendCapabilities,
    schemaKeyword,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (Value)
import Data.Aeson.Key (Key)
import Harness.Quoter.Yaml (yaml)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | A supported backend type.
-- NOTE: Different data-connector agents are represented by seperate constructors
--       If we want to be able to test these generatively we may want to have a
--       parameterized constructor for new data-connectors in future.
data BackendType
  = Postgres
  | SQLServer
  | BigQuery
  | Citus
  | Cockroach
  | DataConnector String
  deriving (Eq, Ord, Show)

pattern DataConnectorSqlite :: BackendType
pattern DataConnectorSqlite = DataConnector "sqlite"

pattern DataConnectorMock :: BackendType
pattern DataConnectorMock = DataConnector "mock"

pattern DataConnectorReference :: BackendType
pattern DataConnectorReference = DataConnector "reference"

-- | The default hasura metadata source name used for a given backend in this test suite project.
defaultSource :: BackendType -> String
defaultSource = \case
  Postgres -> "postgres"
  SQLServer -> "mssql"
  BigQuery -> "bigquery"
  Citus -> "citus"
  Cockroach -> "cockroach"
  DataConnector agent -> "chinook_" <> agent

-- | The default hasura metadata backend type used for a given backend in this test suite project.
defaultBackendCapabilities :: BackendType -> Maybe Value
defaultBackendCapabilities = \case
  DataConnectorSqlite ->
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
    |]
  DataConnectorReference ->
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
          string:
            aggregate_functions:
              longest: string
              shortest: string
            graphql_type: String
    |]
  _ -> Nothing

-- | The default hasura metadata backend type used for a given backend in this test suite project.
defaultBackendTypeString :: BackendType -> String
defaultBackendTypeString = \case
  Postgres -> "pg"
  SQLServer -> "mssql"
  BigQuery -> "bigquery"
  Citus -> "citus"
  Cockroach -> "cockroach"
  DataConnector agent -> agent

defaultBackendDisplayNameString :: BackendType -> String
defaultBackendDisplayNameString b = case defaultBackendTypeString b of
  "sqlite" -> "Hasura SQLite (sqlite)"
  x -> x

-- | The default hasura metadata backend type used for a given backend in this test suite project.
defaultBackendServerUrl :: BackendType -> Maybe String
defaultBackendServerUrl = \case
  Postgres -> Nothing
  SQLServer -> Nothing
  BigQuery -> Nothing
  Citus -> Nothing
  Cockroach -> Nothing
  DataConnectorReference -> Just "http://localhost:65005"
  DataConnectorSqlite -> Just "http://localhost:65007"
  DataConnector _ -> Nothing

-- workaround until we support schema/dataset keys generically
-- https://hasurahq.atlassian.net/browse/NDAT-332
schemaKeyword :: BackendType -> Key
schemaKeyword = \case
  Postgres -> "schema"
  SQLServer -> "schema"
  BigQuery -> "dataset"
  Citus -> "schema"
  Cockroach -> "schema"
  DataConnector _ -> "schema"
