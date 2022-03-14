-- | This module defines the translation functions for insert and upsert
-- mutations.
module Hasura.Backends.MSSQL.FromIr.Insert
  ( fromInsert,
    toMerge,
    toInsertValuesIntoTempTable,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict qualified as HM
import Hasura.Backends.MSSQL.FromIr (FromIr)
import Hasura.Backends.MSSQL.FromIr.Constants (tempTableNameInserted, tempTableNameValues)
import Hasura.Backends.MSSQL.FromIr.Expression (fromGBoolExp)
import Hasura.Backends.MSSQL.Instances.Types ()
import Hasura.Backends.MSSQL.Types.Insert (IfMatched (..))
import Hasura.Backends.MSSQL.Types.Internal as TSQL
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Column qualified as IR
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax (unName)

fromInsert :: IR.AnnInsert 'MSSQL Void Expression -> Insert
fromInsert IR.AnnInsert {..} =
  let IR.AnnIns {..} = _aiData
      insertRows = normalizeInsertRows _aiDefVals $ map IR.getInsertColumns _aiInsObj
      insertColumnNames = maybe [] (map fst) $ listToMaybe insertRows
      insertValues = map (Values . map snd) insertRows
      allColumnNames = map (ColumnName . unName . IR.ciName) _aiTableCols
      insertOutput = Output Inserted $ map OutputColumn allColumnNames
      tempTable = TempTable tempTableNameInserted allColumnNames
   in Insert _aiTableName insertColumnNames insertOutput tempTable insertValues

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
  HM.HashMap (Column 'MSSQL) Expression ->
  [[(Column 'MSSQL, Expression)]] ->
  [[(Column 'MSSQL, Expression)]]
normalizeInsertRows presets insertRows =
  let insertColumns = nubOrd (concatMap (map fst) insertRows <> HM.keys presets)
      allColumnsWithDefaultValue =
        map (\col -> (col, fromMaybe DefaultExpression $ HM.lookup col presets)) insertColumns
      addMissingColumns insertRow =
        HM.toList $ HM.fromList insertRow `HM.union` HM.fromList allColumnsWithDefaultValue
      sortByColumn = sortBy (\l r -> compare (fst l) (fst r))
   in map (sortByColumn . addMissingColumns) insertRows

-- | Construct a MERGE statement from AnnInsert information.
--   A MERGE statement is responsible for actually inserting and/or updating
--   the data in the table.
toMerge ::
  TableName ->
  [IR.AnnotatedInsertRow 'MSSQL Expression] ->
  [IR.ColumnInfo 'MSSQL] ->
  IfMatched Expression ->
  FromIr Merge
toMerge tableName insertRows allColumns IfMatched {..} = do
  let normalizedInsertRows = normalizeInsertRows _imColumnPresets $ map IR.getInsertColumns insertRows
      insertColumnNames = maybe [] (map fst) $ listToMaybe normalizedInsertRows
      allColumnNames = map (ColumnName . unName . IR.ciName) allColumns

  matchConditions <-
    flip runReaderT (EntityAlias "target") $ -- the table is aliased as "target" in MERGE sql
      fromGBoolExp _imConditions

  pure $
    Merge
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
toInsertValuesIntoTempTable :: TempTableName -> IR.AnnInsert 'MSSQL Void Expression -> InsertValuesIntoTempTable
toInsertValuesIntoTempTable tempTable IR.AnnInsert {..} =
  let IR.AnnIns {..} = _aiData
      insertRows = normalizeInsertRows _aiDefVals $ map IR.getInsertColumns _aiInsObj
      insertColumnNames = maybe [] (map fst) $ listToMaybe insertRows
      insertValues = map (Values . map snd) insertRows
   in InsertValuesIntoTempTable
        { ivittTempTableName = tempTable,
          ivittColumns = insertColumnNames,
          ivittValues = insertValues
        }
