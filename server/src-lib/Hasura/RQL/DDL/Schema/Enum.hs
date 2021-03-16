-- | Types and functions for interacting with and manipulating SQL enums represented by
-- /single-column tables/, __not__ native Postgres enum types. Native enum types in Postgres are
-- difficult to change, so we discourage their use, but we might add support for native enum types
-- in the future.
module Hasura.RQL.DDL.Schema.Enum (
  -- * Re-exports from "Hasura.RQL.Types.Column"
    EnumReference(..)
  , EnumValues
  , EnumValueInfo(..)
  , EnumValue(..)

  -- * Loading table info
  , resolveEnumReferences
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict      as M
import qualified Data.Sequence            as Seq
import qualified Data.Sequence.NonEmpty   as NESeq


import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Table

-- | Given a map of enum tables, computes all enum references implied by the given set of foreign
-- keys. A foreign key constitutes an enum reference iff the following conditions hold:
--
--   1. The key only includes a single column.
--   2. The referenced column is the table’s primary key.
--   3. The referenced table is, in fact, an enum table.
resolveEnumReferences
  :: forall b
   . Backend b
  => HashMap (TableName b) (PrimaryKey b (Column b), EnumValues)
  -> HashSet (ForeignKey b)
  -> HashMap (Column b) (NonEmpty (EnumReference b))
resolveEnumReferences enumTables =
  M.fromListWith (<>) . map (fmap (:|[])) . mapMaybe resolveEnumReference . toList
  where
    resolveEnumReference :: ForeignKey b -> Maybe (Column b, EnumReference b)
    resolveEnumReference foreignKey = do
      [(localColumn, foreignColumn)] <- pure $ M.toList (_fkColumnMapping @b foreignKey)
      (primaryKey, enumValues) <- M.lookup (_fkForeignTable foreignKey) enumTables
      guard (_pkColumns primaryKey == foreignColumn NESeq.:<|| Seq.Empty)
      pure (localColumn, EnumReference (_fkForeignTable foreignKey) enumValues)
