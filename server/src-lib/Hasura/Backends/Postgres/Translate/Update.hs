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
  AnnotatedUpdate ('Postgres pgKind) ->
  S.CTE
mkUpdateCTE (AnnotatedUpdateG tn (permFltr, wc) chk (BackendUpdate opExps) _ columnsInfo) =
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

expandOperator :: [ColumnInfo ('Postgres pgKind)] -> (PGCol, UpdateOpExpression S.SQLExp) -> S.SetExpItem
expandOperator infos (column, op) = S.SetExpItem $
  (column,) $ case op of
    UpdateSet e -> e
    UpdateInc e -> S.mkSQLOpExp S.incOp identifier (asNum e)
    UpdateAppend e -> S.mkSQLOpExp S.jsonbConcatOp identifier (asJSON e)
    UpdatePrepend e -> S.mkSQLOpExp S.jsonbConcatOp (asJSON e) identifier
    UpdateDeleteKey e -> S.mkSQLOpExp S.jsonbDeleteOp identifier (asText e)
    UpdateDeleteElem e -> S.mkSQLOpExp S.jsonbDeleteOp identifier (asInt e)
    UpdateDeleteAtPath a -> S.mkSQLOpExp S.jsonbDeleteAtPathOp identifier (asArray a)
  where
    identifier = S.SEIdentifier $ toIdentifier column
    asInt e = S.SETyAnn e S.intTypeAnn
    asText e = S.SETyAnn e S.textTypeAnn
    asJSON e = S.SETyAnn e S.jsonbTypeAnn
    asArray a = S.SETyAnn (S.SEArray a) S.textArrTypeAnn
    asNum e = S.SETyAnn e $
      case find (\info -> ciColumn info == column) infos <&> ciType of
        Just (ColumnScalar s) -> S.mkTypeAnn $ CollectableTypeScalar s
        _ -> S.numericTypeAnn
