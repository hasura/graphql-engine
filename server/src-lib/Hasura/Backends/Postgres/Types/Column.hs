module Hasura.Backends.Postgres.Types.Column where

import           Hasura.Backends.Postgres.Instances.Types ()
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend


-- | Gets the representation type associated with a 'ColumnType'. Avoid using this if possible.
-- Prefer 'parsePGScalarValue', 'parsePGScalarValues', or
-- 'Hasura.RQL.Types.BoolExp.mkTypedSessionVar'.
unsafePGColumnToBackend :: ColumnType ('Postgres pgKind) -> PGScalarType
unsafePGColumnToBackend = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _   -> PGText
