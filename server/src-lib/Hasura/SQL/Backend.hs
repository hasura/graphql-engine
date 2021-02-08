module Hasura.SQL.Backend where

import           Hasura.Prelude

data BackendType = Postgres -- MySQL | MSSQL
  deriving (Show, Bounded, Enum)
  -- TODO: introduce a "None" backend for parts of the code that
  -- should never touch an actual backend, such as introspection
  -- queries.

data BackendTag (b :: BackendType) where
  PostgresTag :: BackendTag 'Postgres

