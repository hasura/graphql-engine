module Hasura.SQL.Backend
  ( BackendType(..)
  , supportedBackends
  ) where

import           Hasura.Prelude

import           Data.Aeson
import           Data.Text          (unpack)
import           Data.Text.Extended


-- | An enum that represents each backend we support.
-- This type MUST be an enumeration (an enumeration consists of one or
-- more nullary, non-GADT constructors).
data BackendType
  = Postgres
  | MSSQL
  | BigQuery
  deriving (Eq, Ord, Bounded, Enum)

-- | The name of the backend, as we expect it to appear in our metadata and API.
instance ToTxt BackendType where
  toTxt Postgres = "postgres"
  toTxt MSSQL    = "mssql"
  toTxt BigQuery = "bigquery"

-- | The FromJSON instance uses this lookup mechanism to avoid having
-- to duplicate and hardcode the backend string.
instance FromJSON BackendType where
  parseJSON = withText "backend type" \name ->
    lookup name [(toTxt b, b) | b <- supportedBackends]
    `onNothing` fail ("got: " <> unpack name <> ", expected one of: " <> unpack (commaSeparated supportedBackends))

instance ToJSON BackendType where
  toJSON = String . toTxt


supportedBackends :: [BackendType]
supportedBackends = [minBound @BackendType .. maxBound]
