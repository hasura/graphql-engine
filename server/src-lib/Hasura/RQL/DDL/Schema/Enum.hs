-- | Types and functions for interacting with and manipulating SQL enums represented by
-- /single-column tables/, __not__ native Postgres enum types. Native enum types in Postgres are
-- difficult to change, so we discourage their use, but we might add support for native enum types
-- in the future.
module Hasura.RQL.DDL.Schema.Enum
  ( -- * Re-exports from "Hasura.RQL.Types.Column"
    EnumReference (..),
    EnumValues,
    EnumValueInfo (..),
    EnumValue (..),

    -- * Loading table info
    resolveEnumReferences,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.Table.Cache (ForeignKey (..), PrimaryKey (..), TableConfig (..))

-- | Given a map of enum tables, computes all enum references implied by the given set of foreign
-- keys. A foreign key constitutes an enum reference iff the following conditions hold:
--
--   1. The key only includes a single column.
--   2. The referenced column is the table’s primary key.
--   3. The referenced table is, in fact, an enum table.
resolveEnumReferences ::
  forall b.
  (Backend b) =>
  HashMap (TableName b) (PrimaryKey b (Column b), TableConfig b, EnumValues) ->
  HashSet (ForeignKey b) ->
  HashMap (Column b) (NonEmpty (EnumReference b))
resolveEnumReferences enumTables =
  HashMap.fromListWith (<>) . map (fmap (:| [])) . mapMaybe resolveEnumReference . toList
  where
    resolveEnumReference :: ForeignKey b -> Maybe (Column b, EnumReference b)
    resolveEnumReference foreignKey = do
      [(localColumnPath, foreignColumnPath)] <- pure $ NEHashMap.toList (_fkColumnMapping @b foreignKey)
      localColumn <- tryColumnPathToColumn @b localColumnPath
      foreignColumn <- tryColumnPathToColumn @b foreignColumnPath
      let foreignKeyTableName = _fkForeignTable foreignKey
      (primaryKey, tConfig, enumValues) <- HashMap.lookup foreignKeyTableName enumTables
      let tableCustomName = _tcCustomName tConfig
      guard (_pkColumns primaryKey == foreignColumn NESeq.:<|| Seq.Empty)
      pure (localColumn, EnumReference foreignKeyTableName enumValues tableCustomName)
