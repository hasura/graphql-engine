-- | This module defines the translation functions for insert and upsert
-- mutations.
module Hasura.Backends.MSSQL.FromIr.Insert
  ( fromInsert,
    toMerge,
    toInsertValuesIntoTempTable,
  )
where

import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashSet qualified as HS
import Hasura.Backends.MSSQL.FromIr (FromIr)
import Hasura.Backends.MSSQL.FromIr.Constants (tempTableNameInserted, tempTableNameValues)
import Hasura.Backends.MSSQL.FromIr.Expression (fromGBoolExp)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Insert (IfMatched (..))
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as IR

fromInsert :: IR.AnnotatedInsert 'MSSQL Void Expression -> Insert
fromInsert IR.AnnotatedInsert {..} =
  let IR.AnnotatedInsertData {..} = _aiData
      (insertColumnNames, insertRows) = normalizeInsertRows _aiPresetValues $ _aiInsertObject
      insertValues = map (Values . HashMap.elems) insertRows
      allColumnNames = map IR.ciColumn _aiTableColumns
      insertOutput = Output Inserted $ map OutputColumn allColumnNames
      tempTable = TempTable tempTableNameInserted allColumnNames
   in Insert _aiTableName (HS.toList insertColumnNames) insertOutput tempTable insertValues

-- | Normalize a row by adding missing columns with @DEFAULT@ value and sort by
-- column name to make sure all rows are consistent in column values and order.
--
-- Example: A table "author" is defined as:
--
-- > CREATE TABLE author ([id] INTEGER NOT NULL PRIMARY KEY, name TEXT NOT NULL, age INTEGER)
--
-- Consider the following mutation:
--
-- > mutation {
-- >   insert_author(
-- >     objects: [{id: 1, name: "Foo", age: 21}, {id: 2, name: "Bar"}]
-- >   ){
-- >     affected_rows
-- >   }
-- > }
--
-- We consider @DEFAULT@ value for @age@ column which is missing in second
-- insert row.
--
-- The corresponding @INSERT@ statement looks like:
--
-- > INSERT INTO author (id, name, age)
-- >   OUTPUT INSERTED.id
-- >   VALUES (1, 'Foo', 21), (2, 'Bar', DEFAULT)
normalizeInsertRows ::
  HashMap.HashMap (Column 'MSSQL) Expression ->
  [IR.AnnotatedInsertRow 'MSSQL Expression] ->
  (HashSet (Column 'MSSQL), [HashMap.HashMap (Column 'MSSQL) Expression])
normalizeInsertRows presets insertRows =
  HashMap.homogenise
    DefaultExpression
    (map ((presets <>) . HashMap.fromList . IR.getInsertColumns) insertRows)

-- | Construct a MERGE statement from AnnotatedInsert information.
--   A MERGE statement is responsible for actually inserting and/or updating
--   the data in the table.
toMerge ::
  TableName ->
  [IR.AnnotatedInsertRow 'MSSQL Expression] ->
  [IR.ColumnInfo 'MSSQL] ->
  IfMatched Expression ->
  FromIr Merge
toMerge tableName insertRows allColumns IfMatched {..} = do
  let insertColumnNames =
        HS.toList
          $ HashMap.keysSet _imColumnPresets
          <> HS.unions (map (HashMap.keysSet . HashMap.fromList . IR.getInsertColumns) insertRows)
      allColumnNames = map IR.ciColumn allColumns

  matchConditions <-
    flip runReaderT (EntityAlias "target")
      $ fromGBoolExp _imConditions -- the table is aliased as "target" in MERGE sql
  pure
    $ Merge
      { mergeTargetTable = tableName,
        mergeUsing = MergeUsing tempTableNameValues insertColumnNames,
        mergeOn = MergeOn _imMatchColumns,
        mergeWhenMatched = MergeWhenMatched _imUpdateColumns matchConditions _imColumnPresets,
        mergeWhenNotMatched = MergeWhenNotMatched insertColumnNames,
        mergeInsertOutput = Output Inserted $ map OutputColumn allColumnNames,
        mergeOutputTempTable = TempTable tempTableNameInserted allColumnNames
      }

-- | As part of an INSERT/UPSERT process, insert VALUES into a temporary table.
--   The content of the temporary table will later be inserted into the original table
--   using a MERGE statement.
--
--   We insert the values into a temporary table first in order to replace the missing
--   fields with @DEFAULT@ in @normalizeInsertRows@, and we can't do that in a
--   MERGE statement directly.
toInsertValuesIntoTempTable :: TempTableName -> IR.AnnotatedInsert 'MSSQL Void Expression -> InsertValuesIntoTempTable
toInsertValuesIntoTempTable tempTable IR.AnnotatedInsert {..} =
  let IR.AnnotatedInsertData {..} = _aiData
      (insertColumnNames, insertRows) = normalizeInsertRows _aiPresetValues _aiInsertObject
      insertValues = map (Values . HashMap.elems) insertRows
   in InsertValuesIntoTempTable
        { ivittTempTableName = tempTable,
          ivittColumns = HS.toList insertColumnNames,
          ivittValues = insertValues
        }
