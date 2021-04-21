module Hasura.Backends.Postgres.Translate.Mutation
  ( mkSelectExpFromColumnValues
  )
where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as Map

import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML      as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.RQL.Instances                  ()
import           Hasura.RQL.Types
import           Hasura.SQL.Types


-- | Note:- Using sorted columns is necessary to enable casting the rows returned by VALUES expression to table type.
-- For example, let's consider the table, `CREATE TABLE test (id serial primary key, name text not null, age int)`.
-- The generated values expression should be in order of columns;
-- `SELECT ("row"::table).* VALUES (1, 'Robert', 23) AS "row"`.
mkSelectExpFromColumnValues
  :: (MonadError QErr m)
  => QualifiedTable -> [ColumnInfo 'Postgres] -> [ColumnValues 'Postgres TxtEncodedVal] -> m S.Select
mkSelectExpFromColumnValues qt allCols = \case
  []       -> return selNoRows
  colVals  -> do
    tuples <- mapM mkTupsFromColVal colVals
    let fromItem = S.FIValues (S.ValuesExp tuples) (S.Alias rowAlias) Nothing
    return S.mkSelect
      { S.selExtr = [extractor]
      , S.selFrom = Just $ S.FromExp [fromItem]
      }
  where
    rowAlias = Identifier "row"
    extractor = S.selectStar' $ S.QualifiedIdentifier rowAlias $ Just $ S.TypeAnn $ toSQLTxt qt
    sortedCols = sortCols allCols
    mkTupsFromColVal colVal =
      fmap S.TupleExp $ forM sortedCols $ \ci -> do
        let pgCol = pgiColumn ci
        val <- onNothing (Map.lookup pgCol colVal) $
          throw500 $ "column " <> pgCol <<> " not found in returning values"
        pure $ txtEncodedToSQLExp (pgiType ci) val

    selNoRows =
      S.mkSelect { S.selExtr = [S.selectStar]
                 , S.selFrom = Just $ S.mkSimpleFromExp qt
                 , S.selWhere = Just $ S.WhereFrag $ S.BELit False
                 }

    txtEncodedToSQLExp colTy = \case
      TENull          -> S.SENull
      TELit textValue ->
        S.withTyAnn (unsafePGColumnToBackend colTy) $ S.SELit textValue
