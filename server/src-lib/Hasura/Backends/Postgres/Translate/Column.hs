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
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options qualified as Options

toTxtValue :: ColumnValue ('Postgres pgKind) -> SQLExp
toTxtValue ColumnValue {..} =
  withScalarTypeAnn ty . withConstructorFn ty $ txtEncoder cvValue
  where
    ty = unsafePGColumnToBackend cvType

-- | Formats each columns to appropriate SQL expression
toJSONableExp :: Options.StringifyNumbers -> ColumnType ('Postgres pgKind) -> Bool -> Maybe NamingCase -> SQLExp -> SQLExp
toJSONableExp stringifyNum colType asText tCase expression
  -- If it's a numeric column greater than a 32-bit integer, we have to stringify it as JSON spec doesn't support >32-bit integers
  | asText || (isScalarColumnWhere isBigNum colType && (case stringifyNum of Options.StringifyNumbers -> True; Options.Don'tStringifyNumbers -> False)) =
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
  | isEnumColumn colType && any isGraphqlCase tCase = applyUppercase expression
  | otherwise = expression
