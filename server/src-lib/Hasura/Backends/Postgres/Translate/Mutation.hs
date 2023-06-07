-- | Postgres Translate Mutation
--
-- Provide a combinator for generating a Postgres SQL SELECT statement for the
-- selected columns in mutation queries.
--
-- See 'Hasura.Backends.Postgres.Execute.Mutation' and note
-- [Prepared statements in Mutations]
module Hasura.Backends.Postgres.Translate.Mutation
  ( mkSelectExpFromColumnValues,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.SQL.Types
import Hasura.Table.Cache

-- | Note:- Using sorted columns is necessary to enable casting the rows returned by VALUES expression to table type.
-- For example, let's consider the table, `CREATE TABLE test (id serial primary key, name text not null, age int)`.
-- The generated values expression should be in order of columns;
-- `SELECT ("row"::table).* VALUES (1, 'Robert', 23) AS "row"`.
mkSelectExpFromColumnValues ::
  forall pgKind m.
  (MonadError QErr m) =>
  QualifiedTable ->
  [ColumnInfo ('Postgres pgKind)] ->
  [ColumnValues ('Postgres pgKind) TxtEncodedVal] ->
  m S.Select
mkSelectExpFromColumnValues qt allCols = \case
  [] -> return selNoRows
  colVals -> do
    tuples <- mapM mkTupsFromColVal colVals
    let fromItem = S.FIValues (S.ValuesExp tuples) rowAlias Nothing
    return
      S.mkSelect
        { S.selExtr = [extractor],
          S.selFrom = Just $ S.FromExp [fromItem]
        }
  where
    rowAlias = S.mkTableAlias "row"
    rowIdentifier = S.tableAliasToIdentifier rowAlias
    extractor = S.selectStar' $ S.QualifiedIdentifier rowIdentifier $ Just $ S.TypeAnn $ toSQLTxt qt
    sortedCols = sortCols allCols
    mkTupsFromColVal colVal =
      fmap S.TupleExp
        $ forM sortedCols
        $ \ci -> do
          let pgCol = ciColumn ci
          val <-
            onNothing (HashMap.lookup pgCol colVal)
              $ throw500
              $ "column "
              <> pgCol
              <<> " not found in returning values"
          pure $ txtEncodedToSQLExp (ciType ci) val

    selNoRows =
      S.mkSelect
        { S.selExtr = [S.selectStar],
          S.selFrom = Just $ S.mkSimpleFromExp qt,
          S.selWhere = Just $ S.WhereFrag $ S.BELit False
        }

    txtEncodedToSQLExp colTy = \case
      TENull -> S.SENull
      TELit textValue ->
        withScalarTypeAnn (unsafePGColumnToBackend colTy) $ S.SELit textValue
