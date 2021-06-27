module Hasura.SQL.Backend
  ( PostgresKind(..)
  , BackendType(..)
  , backendPrefix
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
  deriving (Eq, Ord)

-- | An enum that represents each backend we support.
-- As we lift values to the type level, we expect this type to have an Enum instance.
data BackendType
  = Postgres PostgresKind
  | MSSQL
  | BigQuery
  | SQLite
  deriving (Eq, Ord)


-- | The name of the backend, as we expect it to appear in our metadata and API.
instance ToTxt BackendType where
  toTxt (Postgres Vanilla) = "postgres"
  toTxt (Postgres Citus)   = "citus"
  toTxt MSSQL              = "mssql"
  toTxt BigQuery           = "bigquery"
  toTxt SQLite             = "sqlite"

-- | Some other generated APIs use a prefix of the backend's name. By default it's the same as the
-- output of `toTxt`, but a backend can override this default.
backendPrefix :: BackendType -> Text
backendPrefix = \case
  Postgres Vanilla -> "pg"
  b                -> toTxt b


-- | The FromJSON instance uses this lookup mechanism to avoid having to duplicate and hardcode the
-- backend string. We parse both the prefixed form and the long form.
instance FromJSON BackendType where
  parseJSON = withText "backend type" \name -> do
    let knownBackends = supportedBackends >>= \b ->
          [ (toTxt         b, b) -- long form
          , (backendPrefix b, b) -- prefix form
          ]
    lookup name knownBackends
      `onNothing` fail ("got: " <> unpack name <> ", expected one of: " <>
                        unpack (commaSeparated $ fst <$> uniques knownBackends))


instance ToJSON BackendType where
  toJSON = String . toTxt

instance Cacheable (Proxy (b :: BackendType))


supportedBackends :: [BackendType]
supportedBackends =
  [ Postgres Vanilla
  , Postgres Citus
  , MSSQL
  , BigQuery
  , SQLite
  ]
