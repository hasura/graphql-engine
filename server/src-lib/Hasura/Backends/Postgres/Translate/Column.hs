module Hasura.Backends.Postgres.Translate.Column
  ( toTxtValue
  ) where

import           Hasura.Prelude

import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.RQL.Types

import qualified Hasura.Backends.Postgres.SQL.DML   as PG


toTxtValue :: ColumnValue 'Postgres -> PG.SQLExp
toTxtValue ColumnValue{..} =
  PG.withTyAnn ty . withConstructorFn ty $ txtEncoder cvValue
  where
    ty = columnTypePGScalarRepresentation cvType
