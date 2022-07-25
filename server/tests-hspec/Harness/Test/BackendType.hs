-- | Definition of backend types and a few helper functions.
module Harness.Test.BackendType
  ( BackendType (..),
    defaultSource,
    defaultBackendTypeString,
    defaultSchema,
    schemaKeyword,
  )
where

import Data.Aeson.Key (Key)
import Harness.Constants qualified as Constants (bigqueryDataset, citusDb, dataConnectorDb, mysqlDb, postgresDb, sqlserverDb)
import Prelude

-- | A supported backend type.
data BackendType
  = Postgres
  | MySQL
  | SQLServer
  | BigQuery
  | Citus
  | DataConnector
  deriving (Eq, Show)

-- | The default hasura metadata source name used for a given backend in this test suite project.
defaultSource :: BackendType -> String
defaultSource = \case
  Postgres -> "postgres"
  MySQL -> "mysql"
  SQLServer -> "mssql"
  BigQuery -> "bigquery"
  Citus -> "citus"
  DataConnector -> "data-connector"

-- | The default hasura metadata backend type used for a given backend in this test suite project.
defaultBackendTypeString :: BackendType -> String
defaultBackendTypeString = \case
  Postgres -> "postgres"
  MySQL -> "mysql"
  SQLServer -> "mssql"
  BigQuery -> "bigquery"
  Citus -> "citus"
  DataConnector -> "data-connector"

-- | The default hasura metadata schema name used for a given backend in this test suite project.
defaultSchema :: BackendType -> String
defaultSchema = \case
  Postgres -> Constants.postgresDb
  MySQL -> Constants.mysqlDb
  SQLServer -> Constants.sqlserverDb
  BigQuery -> Constants.bigqueryDataset
  Citus -> Constants.citusDb
  DataConnector -> Constants.dataConnectorDb

schemaKeyword :: BackendType -> Key
schemaKeyword = \case
  Postgres -> "schema"
  MySQL -> "schema"
  SQLServer -> "schema"
  BigQuery -> "dataset"
  Citus -> "schema"
  DataConnector -> "schema"
