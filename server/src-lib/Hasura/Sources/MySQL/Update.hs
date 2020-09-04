module Hasura.Sources.MySQL.Update
  ( execUpdateQuery
  ) where

import           Instances.TH.Lift             ()

import qualified Data.Sequence                 as DS

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Insert         (insertCheckExpr)
import           Hasura.RQL.DML.Update.Types
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types
import           Hasura.Server.Version         (HasVersion)
import           Hasura.Sources.MySQL.Mutation
import           Hasura.SQL.Types

import qualified Data.Environment              as Env
import qualified Database.PG.Query             as Q
import qualified Hasura.SQL.DML                as S
import qualified Hasura.Tracing                as Tracing


mkUpdateCTE
  :: AnnUpd -> S.CTE
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

expandOperator :: [PGColumnInfo] -> (PGCol, UpdOpExpG S.SQLExp) -> S.SetExpItem
expandOperator infos (column, op) = S.SetExpItem $ (column,) $ case op of
  UpdSet          e -> e
  UpdInc          e -> S.mkSQLOpExp S.incOp               identifier (asNum  e)
  UpdAppend       e -> S.mkSQLOpExp S.jsonbConcatOp       identifier (asJSON e)
  UpdPrepend      e -> S.mkSQLOpExp S.jsonbConcatOp       (asJSON e) identifier
  UpdDeleteKey    e -> S.mkSQLOpExp S.jsonbDeleteOp       identifier (asText e)
  UpdDeleteElem   e -> S.mkSQLOpExp S.jsonbDeleteOp       identifier (asInt  e)
  UpdDeleteAtPath a -> S.mkSQLOpExp S.jsonbDeleteAtPathOp identifier (asArray a)
  where
    identifier = S.SEIden $ toIdentifier column
    asInt  e   = S.SETyAnn e S.intTypeAnn
    asText e   = S.SETyAnn e S.textTypeAnn
    asJSON e   = S.SETyAnn e S.jsonbTypeAnn
    asArray a  = S.SETyAnn (S.SEArray a) S.textArrTypeAnn
    asNum  e   = S.SETyAnn e $
      case find (\info -> pgiColumn info == column) infos <&> pgiType of
        Just (PGColumnScalar s) -> S.mkTypeAnn $ PGTypeScalar s
        _                       -> S.numericTypeAnn

execUpdateQuery
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Bool
  -> Maybe MutationRemoteJoinCtx
  -> (AnnUpd, DS.Seq Q.PrepArg)
  -> m EncJSON
execUpdateQuery env strfyNum remoteJoinCtx (u, p) =
  runMutation env $ mkMutation remoteJoinCtx (uqp1Table u) (updateCTE, p)
                (uqp1Output u) (uqp1AllCols u) strfyNum
  where
    updateCTE = mkUpdateCTE u
