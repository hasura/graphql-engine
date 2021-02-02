module Hasura.Backends.Postgres.Translate.Column
  ( toTxtValue
  ) where

import           Hasura.Prelude

import           Hasura.Backends.Postgres.SQL.DML
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.RQL.Types


toTxtValue :: ColumnValue 'Postgres -> SQLExp
toTxtValue ColumnValue{..} =
  withTyAnn ty . withConstructorFn ty $ txtEncoder cvValue
  where
    ty = unsafePGColumnToBackend cvType
