{-# LANGUAGE PatternSynonyms #-}

-- | Definition of backend types and a few helper functions.
module Harness.Test.BackendType
  ( BackendType (..),
    BackendTypeConfig (..),
    pattern DataConnectorMock,
    pattern DataConnectorReference,
    pattern DataConnectorSqlite,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (Value)
import Data.Aeson.Key (Key)
import Hasura.Prelude

--------------------------------------------------------------------------------

data BackendTypeConfig = BackendTypeConfig
  { backendType :: BackendType,
    -- \| The default hasura metadata source name used for a given backend in this test suite project.
    backendSourceName :: String,
    -- \| The default hasura metadata backend type used for a given backend in this test suite project.
    backendCapabilities :: Maybe Value,
    -- \| The default hasura metadata backend type used for a given backend in this test suite project.
    backendTypeString :: String,
    backendDisplayNameString :: String,
    -- \| The default backend URL for the given backend in this test suite project.
    backendServerUrl :: Maybe String,
    backendSchemaKeyword :: Key
  }

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
