module Hasura.Backends.Postgres.Translate.Column
  ( toTxtValue
  ) where

import           Hasura.Prelude

import           Hasura.Backends.Postgres.SQL.DML
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend


toTxtValue :: ColumnValue ('Postgres pgKind) -> SQLExp
toTxtValue ColumnValue{..} =
  withTyAnn ty . withConstructorFn ty $ txtEncoder cvValue
  where
    ty = unsafePGColumnToBackend cvType
