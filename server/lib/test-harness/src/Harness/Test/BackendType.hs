{-# LANGUAGE QuasiQuotes #-}

-- | Definition of backend types and a few helper functions.
module Harness.Test.BackendType
  ( BackendType (..),
    defaultSource,
    defaultBackendTypeString,
    defaultBackendServerUrl,
    defaultBackendCapabilities,
    schemaKeyword,
  )
where

import Data.Aeson (Value)
import Data.Aeson.Key (Key)
import Harness.Quoter.Yaml (yaml)
import Hasura.Prelude

-- | A supported backend type.
-- NOTE: Different data-connector agents are represented by seperate constructors
--       If we want to be able to test these generatively we may want to have a
--       parameterized constructor for data-connectors in future.
data BackendType
  = Postgres
  | MySQL
  | SQLServer
  | BigQuery
  | Citus
  | Cockroach
  | DataConnectorMock
  | DataConnectorReference
  | DataConnectorSqlite
  deriving (Eq, Show)

-- | The default hasura metadata source name used for a given backend in this test suite project.
defaultSource :: BackendType -> String
defaultSource = \case
  Postgres -> "postgres"
  MySQL -> "mysql"
  SQLServer -> "mssql"
  BigQuery -> "bigquery"
  Citus -> "citus"
  Cockroach -> "cockroach"
  DataConnectorMock -> "chinook_mock"
  DataConnectorReference -> "chinook_reference"
  DataConnectorSqlite -> "chinook_sqlite"

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
        graphql_schema: |-
          scalar DateTime
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
        graphql_schema: |-
          scalar DateTime

          input DateTimeComparisons {in_year: Int
            same_day_as: DateTime
          }
        relationships: {}
        comparisons:
          subquery:
            supports_relations: true
        scalar_types:
          DateTime:
            comparison_type: DateTimeComparisons
    |]
  _ -> Nothing

-- | The default hasura metadata backend type used for a given backend in this test suite project.
defaultBackendTypeString :: BackendType -> String
defaultBackendTypeString = \case
  Postgres -> "postgres"
  MySQL -> "mysql"
  SQLServer -> "mssql"
  BigQuery -> "bigquery"
  Citus -> "citus"
  Cockroach -> "cockroach"
  DataConnectorMock -> "mock"
  DataConnectorReference -> "reference"
  DataConnectorSqlite -> "sqlite"

-- | The default hasura metadata backend type used for a given backend in this test suite project.
defaultBackendServerUrl :: BackendType -> Maybe String
defaultBackendServerUrl = \case
  Postgres -> Nothing
  MySQL -> Nothing
  SQLServer -> Nothing
  BigQuery -> Nothing
  Citus -> Nothing
  Cockroach -> Nothing
  DataConnectorMock -> Nothing
  DataConnectorReference -> Just "http://localhost:65005"
  DataConnectorSqlite -> Just "http://localhost:65007"

schemaKeyword :: BackendType -> Key
schemaKeyword = \case
  Postgres -> "schema"
  MySQL -> "schema"
  SQLServer -> "schema"
  BigQuery -> "dataset"
  Citus -> "schema"
  Cockroach -> "schema"
  DataConnectorMock -> "schema"
  DataConnectorReference -> "schema"
  DataConnectorSqlite -> "schema"
