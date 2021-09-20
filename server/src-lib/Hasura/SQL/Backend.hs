module Hasura.SQL.Backend
  ( PostgresKind(..)
  , BackendType(..)
  , backendShortName
  , supportedBackends
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.List.Extended (uniques)
import           Data.Proxy
import           Data.Text          (unpack)
import           Data.Text.Extended

import           Hasura.Incremental


-- | Argument to Postgres; we represent backends which are variations on Postgres as sub-types of
-- Postgres. This value indicates which "flavour" of Postgres a backend is.
data PostgresKind
  = Vanilla
  | Citus
  deriving (Show, Eq, Ord)

-- | An enum that represents each backend we support.
data BackendType
  = Postgres PostgresKind
  | MSSQL
  | BigQuery
  | MySQL
  deriving (Show, Eq, Ord)

-- | The name of the backend, as we expect it to appear in our metadata and API.
instance ToTxt BackendType where
  toTxt (Postgres Vanilla) = "postgres"
  toTxt (Postgres Citus)   = "citus"
  toTxt MSSQL              = "mssql"
  toTxt BigQuery           = "bigquery"
  toTxt MySQL              = "mysql"

-- | The FromJSON instance uses this lookup mechanism to avoid having to duplicate and hardcode the
-- backend string. We accept both the short form and the long form of the backend's name.
instance FromJSON BackendType where
  parseJSON = withText "backend type" \name -> do
    let knownBackends = supportedBackends >>= \b ->
          [ (toTxt            b, b) -- long form
          , (backendShortName b, b) -- short form
          ]
        uniqueBackends = commaSeparated $ fst <$> uniques knownBackends
    lookup name knownBackends `onNothing`
      fail ("got: " <> unpack name <> ", expected one of: " <> unpack uniqueBackends)

instance ToJSON BackendType where
  toJSON = String . toTxt

instance Cacheable (Proxy (b :: BackendType))


-- | Some generated APIs use a shortened version of the backend's name rather than its full
-- name. This function returns the "short form" of a backend, if any.
backendShortName :: BackendType -> Text
backendShortName = \case
  Postgres Vanilla -> "pg"
  b                -> toTxt b

supportedBackends :: [BackendType]
supportedBackends =
  [ Postgres Vanilla
  , Postgres Citus
  , MSSQL
  , BigQuery
  , MySQL
  ]
