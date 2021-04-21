module Hasura.SQL.Backend
  ( PostgresKind(..)
  , BackendType(..)
  , supportedBackends
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Proxy
import           Data.Text          (unpack)
import           Data.Text.Extended

import           Hasura.Incremental


-- | Argument to Postgres; we represent backends which are variations on Postgres as sub-types of
-- Postgres. This value indicates which "flavour" of Postgres a backend is.
data PostgresKind
  = Vanilla
  deriving (Eq, Ord)

-- | An enum that represents each backend we support.
-- As we lift values to the type level, we expect this type to have an Enum instance.
data BackendType
  = Postgres PostgresKind
  | MSSQL
  | BigQuery
  deriving (Eq, Ord)


-- | The name of the backend, as we expect it to appear in our metadata and API.
instance ToTxt BackendType where
  toTxt (Postgres Vanilla) = "postgres"
  toTxt MSSQL              = "mssql"
  toTxt BigQuery           = "bigquery"

-- | The FromJSON instance uses this lookup mechanism to avoid having
-- to duplicate and hardcode the backend string.
instance FromJSON BackendType where
  parseJSON = withText "backend type" \name ->
    lookup name [(toTxt b, b) | b <- supportedBackends]
    `onNothing` fail ("got: " <> unpack name <> ", expected one of: " <> unpack (commaSeparated supportedBackends))

instance ToJSON BackendType where
  toJSON = String . toTxt

instance Cacheable (Proxy (b :: BackendType))


supportedBackends :: [BackendType]
supportedBackends =
  [ Postgres Vanilla
  , MSSQL
  , BigQuery
  ]
