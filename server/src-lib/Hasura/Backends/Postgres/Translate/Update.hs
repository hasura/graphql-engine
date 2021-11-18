module Hasura.Backends.Postgres.Translate.Update
  ( mkUpdateCTE,
  )
where

import Data.HashMap.Strict qualified as Map
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Backends.Postgres.Translate.Insert
import Hasura.Backends.Postgres.Translate.Returning
import Hasura.Backends.Postgres.Types.Update
import Hasura.Prelude
import Hasura.RQL.IR.Update
import Hasura.RQL.Types
import Hasura.SQL.Types

mkUpdateCTE ::
  Backend ('Postgres pgKind) =>
  AnnotatedUpdateNode ('Postgres pgKind) ->
  S.CTE
mkUpdateCTE (AnnotatedUpdateNode tn (permFltr, wc) chk (BackendUpdate opExps) _ columnsInfo) =
  S.CTEUpdate update
  where
    update =
      S.SQLUpdate tn setExp Nothing tableFltr
        . Just
        . S.RetExp
        $ [ S.selectStar,
            asCheckErrorExtractor $ insertCheckConstraint checkExpr
          ]
    setExp = S.SetExp $ map (expandOperator columnsInfo) (Map.toList opExps)
    tableFltr = Just $ S.WhereFrag tableFltrExpr
    tableFltrExpr = toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps permFltr wc
    checkExpr = toSQLBoolExp (S.QualTable tn) chk

expandOperator :: [ColumnInfo ('Postgres pgKind)] -> (PGCol, UpdOpExpG S.SQLExp) -> S.SetExpItem
expandOperator infos (column, op) = S.SetExpItem $
  (column,) $ case op of
    UpdSet e -> e
    UpdInc e -> S.mkSQLOpExp S.incOp identifier (asNum e)
    UpdAppend e -> S.mkSQLOpExp S.jsonbConcatOp identifier (asJSON e)
    UpdPrepend e -> S.mkSQLOpExp S.jsonbConcatOp (asJSON e) identifier
    UpdDeleteKey e -> S.mkSQLOpExp S.jsonbDeleteOp identifier (asText e)
    UpdDeleteElem e -> S.mkSQLOpExp S.jsonbDeleteOp identifier (asInt e)
    UpdDeleteAtPath a -> S.mkSQLOpExp S.jsonbDeleteAtPathOp identifier (asArray a)
  where
    identifier = S.SEIdentifier $ toIdentifier column
    asInt e = S.SETyAnn e S.intTypeAnn
    asText e = S.SETyAnn e S.textTypeAnn
    asJSON e = S.SETyAnn e S.jsonbTypeAnn
    asArray a = S.SETyAnn (S.SEArray a) S.textArrTypeAnn
    asNum e = S.SETyAnn e $
      case find (\info -> pgiColumn info == column) infos <&> pgiType of
        Just (ColumnScalar s) -> S.mkTypeAnn $ CollectableTypeScalar s
        _ -> S.numericTypeAnn
