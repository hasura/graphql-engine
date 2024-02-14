{-# LANGUAGE PatternSynonyms #-}

-- | Definition of backend types and a few helper functions.
module Harness.Test.BackendType
  ( BackendType (..),
    BackendTypeConfig (..),
    isDataConnector,
    parseCapabilities,
    postgresishGraphQLType,
    pattern DataConnectorMock,
    pattern DataConnectorReference,
    pattern DataConnectorSqlite,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (Value)
import Data.Aeson.Key (Key)
import Data.Aeson.Types qualified as Aeson
import GHC.Stack (HasCallStack)
import Harness.Test.ScalarType
import Hasura.Backends.DataConnector.API.V0 qualified as API
import Hasura.Prelude

--------------------------------------------------------------------------------

data BackendTypeConfig = BackendTypeConfig
  { backendType :: BackendType,
    -- | The default hasura metadata source name used for a given
    -- backend in this test suite project.
    backendSourceName :: String,
    -- | The default capabilities for the backend. NOTE: This
    -- currently only applies to DataConnector backends.
    backendCapabilities :: Maybe Value,
    -- | The default hasura metadata backend type used for a given
    -- backend in this test suite project.
    backendTypeString :: String,
    backendDisplayNameString :: String,
    backendReleaseNameString :: Maybe String,
    -- | The default backend URL for the given backend in this test
    -- suite project.
    backendServerUrl :: Maybe String,
    backendSchemaKeyword :: Key,
    -- | How should we render scalar types for this backend?
    backendScalarType :: ScalarType -> Text,
    -- | How to map scalar types to GraphQL types
    backendGraphQLType :: ScalarType -> Text
  }

parseCapabilities :: BackendTypeConfig -> Maybe API.Capabilities
parseCapabilities = backendCapabilities >=> Aeson.parseMaybe Aeson.parseJSON

-- | Convenience Helper for "Postgresish" GraphQL Types.
postgresishGraphQLType :: (HasCallStack) => ScalarType -> Text
postgresishGraphQLType = \case
  TInt -> "Int"
  TStr -> "String"
  TDouble -> "float8"
  TUTCTime -> "timestamp"
  TBool -> "Boolean"
  TGeography -> "geography"
  TCustomType txt -> getBackendScalarType txt bstPostgres -- Maybe?

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

-- | Check whether the 'BackendType' is a data connector.
isDataConnector :: BackendType -> Bool
isDataConnector = \case
  DataConnector _ -> True
  _ -> False

pattern DataConnectorSqlite :: BackendType
pattern DataConnectorSqlite = DataConnector "sqlite"

pattern DataConnectorMock :: BackendType
pattern DataConnectorMock = DataConnector "mock"

pattern DataConnectorReference :: BackendType
pattern DataConnectorReference = DataConnector "reference"
