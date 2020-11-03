module Hasura.Backends.Postgres.Translate.Update
  ( mkUpdateCTE
  ) where

import           Hasura.Prelude

import           Instances.TH.Lift                          ()

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.Backends.Postgres.Translate.Insert
import           Hasura.RQL.IR.Update
import           Hasura.RQL.Instances                       ()
import           Hasura.RQL.Types


mkUpdateCTE
  :: AnnUpd 'Postgres -> S.CTE
mkUpdateCTE (AnnUpd tn opExps (permFltr, wc) chk _ columnsInfo) =
  S.CTEUpdate update
  where
    update =
      S.SQLUpdate tn setExp Nothing tableFltr
        . Just
        . S.RetExp
        $ [ S.selectStar
          , S.Extractor (insertCheckExpr "update check constraint failed" checkExpr) Nothing
          ]
    setExp    = S.SetExp $ map (expandOperator columnsInfo) opExps
    tableFltr = Just $ S.WhereFrag tableFltrExpr
    tableFltrExpr = toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps permFltr wc
    checkExpr = toSQLBoolExp (S.QualTable tn) chk

expandOperator :: [ColumnInfo 'Postgres] -> (PGCol, UpdOpExpG S.SQLExp) -> S.SetExpItem
expandOperator infos (column, op) = S.SetExpItem $ (column,) $ case op of
  UpdSet          e -> e
  UpdInc          e -> S.mkSQLOpExp S.incOp               identifier (asNum  e)
  UpdAppend       e -> S.mkSQLOpExp S.jsonbConcatOp       identifier (asJSON e)
  UpdPrepend      e -> S.mkSQLOpExp S.jsonbConcatOp       (asJSON e) identifier
  UpdDeleteKey    e -> S.mkSQLOpExp S.jsonbDeleteOp       identifier (asText e)
  UpdDeleteElem   e -> S.mkSQLOpExp S.jsonbDeleteOp       identifier (asInt  e)
  UpdDeleteAtPath a -> S.mkSQLOpExp S.jsonbDeleteAtPathOp identifier (asArray a)
  where
    identifier = S.SEIdentifier $ toIdentifier column
    asInt  e   = S.SETyAnn e S.intTypeAnn
    asText e   = S.SETyAnn e S.textTypeAnn
    asJSON e   = S.SETyAnn e S.jsonbTypeAnn
    asArray a  = S.SETyAnn (S.SEArray a) S.textArrTypeAnn
    asNum  e   = S.SETyAnn e $
      case find (\info -> pgiColumn info == column) infos <&> pgiType of
        Just (PGColumnScalar s) -> S.mkTypeAnn $ PGTypeScalar s
        _                       -> S.numericTypeAnn
