module Hasura.GraphQL.Resolve.Mutation
  ( convertUpdate
  , convertDelete
  , convertMutResp
  , buildEmptyMutResp
  ) where

import           Data.Has                          (getter)
import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Data.Sequence                     as Seq
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Delete             as RD
import qualified Hasura.RQL.DML.Internal           as RI
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.DML.Select             as RS
import qualified Hasura.RQL.DML.Update             as RU

import qualified Hasura.SQL.DML                    as S

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Select
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

convertMutQuery
  :: PrepFn Convert -> G.NamedType -> SelSet -> Convert RR.MutQFlds
convertMutQuery f ty selSet = fmap toFields $
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename" -> return $ RR.MQFExp $ G.unName $ G.unNamedType ty
    fName -> do
      opCtxMap <- asks getter
      userInfo <- asks getter
      let validateHdrs' = validateHdrs userInfo
      opCtx <- getOpCtx opCtxMap fName
      fieldAsPath fld $ case opCtx of

        OCSelect tn permFilter permLimit hdrs -> do
          validateHdrs' hdrs
          RR.MQFSimple False <$>
            fromField f tn permFilter permLimit fld

        OCSelectPkey tn permFilter hdrs -> do
          validateHdrs' hdrs
          RR.MQFSimple True <$>
            fromFieldByPKey f tn permFilter fld

        OCSelectAgg tn permFilter permLimit hdrs -> do
          validateHdrs' hdrs
          RR.MQFAgg <$>
            fromAggField f tn permFilter permLimit fld

        OCFuncQuery tn fn permFilter permLimit hdrs -> do
          validateHdrs' hdrs
          resolveFuncQuery tn fn permFilter permLimit False fld

        OCFuncAggQuery tn fn permFilter permLimit hdrs -> do
          validateHdrs' hdrs
          resolveFuncQuery tn fn permFilter permLimit True fld

        _ -> throw500 "mutations are not supported"

  where
    getOpCtx opCtxMap fld =
      onNothing (Map.lookup fld opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName fld

    resolveFuncQuery tn fn permFilter permLimit isAgg fld = do
      (tableArgs, sel, frmItem) <- fromFuncQueryField f fn isAgg fld
      let tabPerm = RS.TablePerm permFilter permLimit
      return $ RR.MQFFunc $
        RS.SQLFunctionSel fn tn sel tableArgs tabPerm frmItem

convertMutResp
  :: PrepFn Convert -> G.NamedType -> SelSet -> Convert RR.MutFlds
convertMutResp f ty selSet =
  withSelSet selSet $ \fld -> fieldAsPath fld $
  case _fName fld of
    "__typename"    -> return $ RR.MExp $ G.unName $ G.unNamedType ty
    "affected_rows" -> return RR.MCount
    "returning"     -> fmap RR.MRet $
                       fromSelSet f (_fType fld) $ _fSelSet fld
    "query"         -> fmap RR.MQuery $
                       convertMutQuery f (_fType fld) $ _fSelSet fld
    G.Name t        -> throw500 $ "unexpected field in mutation resp : " <> t

convertRowObj
  :: (MonadError QErr m, MonadState PrepArgs m)
  => AnnGValue
  -> m [(PGCol, S.SQLExp)]
convertRowObj val =
  flip withObject val $ \_ obj ->
  forM (OMap.toList obj) $ \(k, v) -> do
    prepExpM <- asPGColValM v >>= mapM prepare
    let prepExp = fromMaybe (S.SEUnsafe "NULL") prepExpM
    return (PGCol $ G.unName k, prepExp)

type ApplySQLOp =  (PGCol, S.SQLExp) -> S.SQLExp

rhsExpOp :: S.SQLOp -> S.AnnType -> ApplySQLOp
rhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op (S.SEIden $ toIden col) annExp
  where
    annExp = S.SETyAnn e annTy

lhsExpOp :: S.SQLOp -> S.AnnType -> ApplySQLOp
lhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op annExp $ S.SEIden $ toIden col
  where
    annExp = S.SETyAnn e annTy

convObjWithOp
  :: (MonadError QErr m)
  => ApplySQLOp -> AnnGValue -> m [(PGCol, S.SQLExp)]
convObjWithOp opFn val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
  (_, colVal) <- asPGColVal v
  let pgCol = PGCol $ G.unName k
      encVal = txtEncoder colVal
      sqlExp = opFn (pgCol, encVal)
  return (pgCol, sqlExp)

convDeleteAtPathObj
  :: (MonadError QErr m)
  => AnnGValue -> m [(PGCol, S.SQLExp)]
convDeleteAtPathObj val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
    vals <- flip withArray v $ \_ annVals -> mapM asPGColVal annVals
    let valExps = map (txtEncoder . snd) vals
        pgCol = PGCol $ G.unName k
        annEncVal = S.SETyAnn (S.SEArray valExps) S.textArrType
        sqlExp = S.SEOpApp S.jsonbDeleteAtPathOp
                 [S.SEIden $ toIden pgCol, annEncVal]
    return (pgCol, sqlExp)

convertUpdate
  :: QualifiedTable -- table
  -> AnnBoolExpSQL -- the filter expression
  -> Field -- the mutation field
  -> Convert RespTx
convertUpdate tn filterExp fld = do
  -- a set expression is same as a row object
  setExpM   <- withArgM args "_set" convertRowObj
  -- where bool expression to filter column
  whereExp <- withArg args "where" (parseBoolExp prepare)
  -- increment operator on integer columns
  incExpM <- withArgM args "_inc" $
    convObjWithOp $ rhsExpOp S.incOp S.intType
  -- append jsonb value
  appendExpM <- withArgM args "_append" $
    convObjWithOp $ rhsExpOp S.jsonbConcatOp S.jsonbType
  -- prepend jsonb value
  prependExpM <- withArgM args "_prepend" $
    convObjWithOp $ lhsExpOp S.jsonbConcatOp S.jsonbType
  -- delete a key in jsonb object
  deleteKeyExpM <- withArgM args "_delete_key" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.textType
  -- delete an element in jsonb array
  deleteElemExpM <- withArgM args "_delete_elem" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.intType
  -- delete at path in jsonb value
  deleteAtPathExpM <- withArgM args "_delete_at_path" convDeleteAtPathObj

  mutFlds  <- convertMutResp prepare (_fType fld) $ _fSelSet fld
  prepArgs <- get
  let updExpsM = [ setExpM, incExpM, appendExpM, prependExpM
                 , deleteKeyExpM, deleteElemExpM, deleteAtPathExpM
                 ]
      setItems = concat $ catMaybes updExpsM
  -- atleast one of update operators is expected
  unless (any isJust updExpsM) $ throwVE $
    "atleast any one of _set, _inc, _append, _prepend, _delete_key, _delete_elem and "
    <> " _delete_at_path operator is expected"
  let p1 = RU.UpdateQueryP1 tn setItems (filterExp, whereExp) mutFlds
      whenNonEmptyItems = return $ RU.updateQueryToTx (p1, prepArgs)
      whenEmptyItems = buildEmptyMutResp mutFlds prepArgs
  -- if there are not set items then do not perform
  -- update and return empty mutation response
  bool whenNonEmptyItems whenEmptyItems $ null setItems
  where
    args = _fArguments fld

convertDelete
  :: QualifiedTable -- table
  -> AnnBoolExpSQL -- the filter expression
  -> Field -- the mutation field
  -> Convert RespTx
convertDelete tn filterExp fld = do
  whereExp <- withArg (_fArguments fld) "where" (parseBoolExp prepare)
  mutFlds  <- convertMutResp prepare (_fType fld) $ _fSelSet fld
  args <- get
  let p1 = RD.DeleteQueryP1 tn (filterExp, whereExp) mutFlds
  return $ RD.deleteQueryToTx (p1, args)

-- | build mutation response for empty objects
buildEmptyMutResp
  :: Monad m
  => RR.MutFlds -> Seq.Seq Q.PrepArg -> m RespTx
buildEmptyMutResp mutFlds p = return tx
  where
    tx = RI.execSingleRowAndCol p sel

    sel = S.mkSelect { S.selExtr = [S.Extractor extrExp Nothing] }

    extrExp = S.applyJsonBuildObj jsonBuildObjArgs
    jsonBuildObjArgs =
      flip concatMap mutFlds $
      \(k, fld) -> [S.SELit k, mkFldExp fld]

    -- generate empty mutation response
    mkFldExp = \case
      RR.MCount       -> S.SEUnsafe "0"
      RR.MExp t       -> S.SELit t
      RR.MRet _       -> S.SETyAnn (S.SELit "[]") S.jsonbType
      RR.MQuery qFlds -> RR.mkMutQueryExp qFlds
