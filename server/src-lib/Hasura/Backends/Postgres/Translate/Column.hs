-- | Postgres Translate Column
--
-- Translate column values to Postgres-specific SQL expressions.
module Hasura.Backends.Postgres.Translate.Column
  ( toTxtValue,
    toJSONableExp,
  )
where

import Hasura.Backends.Postgres.SQL.DML
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Types.Column
import Hasura.Prelude
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend

toTxtValue :: ColumnValue ('Postgres pgKind) -> SQLExp
toTxtValue ColumnValue {..} =
  withTyAnn ty . withConstructorFn ty $ txtEncoder cvValue
  where
    ty = unsafePGColumnToBackend cvType

-- | Formats each columns to appropriate SQL expression
toJSONableExp :: Bool -> ColumnType ('Postgres pgKind) -> Bool -> SQLExp -> SQLExp
toJSONableExp stringifyNum colType asText expression
  -- If its a numeric column greater than a 32-bit integer, we have to stringify it as JSON spec doesn't support >32-bit integers
  | asText || (isScalarColumnWhere isBigNum colType && stringifyNum) =
    expression `SETyAnn` textTypeAnn
  -- If the column is either a `Geometry` or `Geography` then apply the `ST_AsGeoJSON` function to convert it into GeoJSON format
  | isScalarColumnWhere isGeoType colType =
    SEFnApp
      "ST_AsGeoJSON"
      [ expression,
        SEUnsafe "15", -- max decimal digits
        SEUnsafe "4" -- to print out crs
      ]
      Nothing
      `SETyAnn` jsonTypeAnn
  | otherwise = expression
