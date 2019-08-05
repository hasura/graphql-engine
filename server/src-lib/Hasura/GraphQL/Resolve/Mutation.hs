module Hasura.GraphQL.Resolve.Mutation
  ( convertUpdate
  , convertDelete
  , convertMutResp
  , buildEmptyMutResp
  ) where

import           Control.Arrow                     (second)
import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                        as J
import qualified Data.HashMap.Strict               as Map
import qualified Data.HashMap.Strict.InsOrd        as OMap
import qualified Language.GraphQL.Draft.Syntax     as G

import qualified Hasura.RQL.DML.Delete             as RD
import qualified Hasura.RQL.DML.Returning          as RR
import qualified Hasura.RQL.DML.Update             as RU

import qualified Hasura.SQL.DML                    as S
import qualified Hasura.RQL.DML.Select             as RS

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.BoolExp
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.InputValue
import           Hasura.GraphQL.Resolve.Select     (fromSelSet)
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

-- withPrepFn
--   :: (MonadReader r m)
--   => PrepFn m -> ReaderT (r, PrepFn m) m a -> m a
-- withPrepFn fn m = do
--   r <- ask
--   runReaderT m (r, fn)

convertMutResp
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => G.NamedType -> SelSet -> m (RR.MutFldsG UnresolvedVal)
convertMutResp ty selSet =
  withSelSet selSet $ \fld -> case _fName fld of
    "__typename"    -> return $ RR.MExp $ G.unName $ G.unNamedType ty
    "affected_rows" -> return RR.MCount
    "returning"     -> do
      annFlds <- fromSelSet (_fType fld) $ _fSelSet fld
      annFldsResolved <- traverse
        (traverse (RS.traverseAnnFld convertUnresolvedVal)) annFlds
      return $ RR.MRet annFldsResolved
    G.Name t        -> throw500 $ "unexpected field in mutation resp : " <> t
  where
    convertUnresolvedVal = \case
      UVPG annPGVal -> UVSQL <$> txtConverter annPGVal
      UVSessVar colTy sessVar -> pure $ UVSessVar colTy sessVar
      UVSQL sqlExp -> pure $ UVSQL sqlExp

convertRowObj
  :: (MonadError QErr m)
  => AnnInpVal
  -> m [(PGCol, UnresolvedVal)]
convertRowObj val =
  flip withObject val $ \_ obj ->
  forM (OMap.toList obj) $ \(k, v) -> do
    prepExpM <- fmap UVPG <$> asPGColValM v
    let prepExp = fromMaybe (UVSQL $ S.SEUnsafe "NULL") prepExpM
    return (PGCol $ G.unName k, prepExp)

type ApplySQLOp =  (PGCol, S.SQLExp) -> S.SQLExp

rhsExpOp :: S.SQLOp -> S.TypeAnn -> ApplySQLOp
rhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op (S.SEIden $ toIden col) annExp
  where
    annExp = S.SETyAnn e annTy

lhsExpOp :: S.SQLOp -> S.TypeAnn -> ApplySQLOp
lhsExpOp op annTy (col, e) =
  S.mkSQLOpExp op annExp $ S.SEIden $ toIden col
  where
    annExp = S.SETyAnn e annTy

convObjWithOp
  :: (MonadError QErr m)
  => ApplySQLOp -> AnnInpVal -> m [(PGCol, UnresolvedVal)]
convObjWithOp opFn val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
  colVal <- _apvValue <$> asPGColVal v
  let pgCol = PGCol $ G.unName k
      -- TODO: why are we using txtEncoder here?
      encVal = txtEncoder colVal
      sqlExp = opFn (pgCol, encVal)
  return (pgCol, UVSQL sqlExp)

convDeleteAtPathObj
  :: (MonadError QErr m)
  => AnnInpVal -> m [(PGCol, UnresolvedVal)]
convDeleteAtPathObj val =
  flip withObject val $ \_ obj -> forM (OMap.toList obj) $ \(k, v) -> do
    vals <- flip withArray v $ \_ annVals -> mapM asPGColVal annVals
    let valExps = map (txtEncoder . _apvValue) vals
        pgCol = PGCol $ G.unName k
        annEncVal = S.SETyAnn (S.SEArray valExps) S.textArrTypeAnn
        sqlExp = S.SEOpApp S.jsonbDeleteAtPathOp
                 [S.SEIden $ toIden pgCol, annEncVal]
    return (pgCol, UVSQL sqlExp)

convertUpdateP1
  :: ( MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => UpdOpCtx -- the update context
  -> Field -- the mutation field
  -> m (RU.AnnUpdG UnresolvedVal)
convertUpdateP1 opCtx fld = do
  -- a set expression is same as a row object
  setExpM   <- withArgM args "_set" convertRowObj
  -- where bool expression to filter column
  whereExp <- withArg args "where" parseBoolExp
  -- increment operator on integer columns
  incExpM <- withArgM args "_inc" $
    convObjWithOp $ rhsExpOp S.incOp S.intTypeAnn
  -- append jsonb value
  appendExpM <- withArgM args "_append" $
    convObjWithOp $ rhsExpOp S.jsonbConcatOp S.jsonbTypeAnn
  -- prepend jsonb value
  prependExpM <- withArgM args "_prepend" $
    convObjWithOp $ lhsExpOp S.jsonbConcatOp S.jsonbTypeAnn
  -- delete a key in jsonb object
  deleteKeyExpM <- withArgM args "_delete_key" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.textTypeAnn
  -- delete an element in jsonb array
  deleteElemExpM <- withArgM args "_delete_elem" $
    convObjWithOp $ rhsExpOp S.jsonbDeleteOp S.intTypeAnn
  -- delete at path in jsonb value
  deleteAtPathExpM <- withArgM args "_delete_at_path" convDeleteAtPathObj
  mutFlds <- convertMutResp (_fType fld) $ _fSelSet fld

  let resolvedPreSetItems =
        Map.toList $ fmap partialSQLExpToUnresolvedVal preSetCols

  let updExpsM = [ setExpM, incExpM, appendExpM, prependExpM
                 , deleteKeyExpM, deleteElemExpM, deleteAtPathExpM
                 ]
      setItems = resolvedPreSetItems ++ concat (catMaybes updExpsM)

  -- atleast one of update operators is expected
  -- or preSetItems shouldn't be empty
  -- this is not equivalent to (null setItems)
  unless (any isJust updExpsM || not (null resolvedPreSetItems)) $ throwVE $
    "atleast any one of _set, _inc, _append, _prepend, "
    <> "_delete_key, _delete_elem and "
    <> "_delete_at_path operator is expected"

  let unresolvedPermFltr = fmapAnnBoolExp partialSQLExpToUnresolvedVal filterExp

  return $ RU.AnnUpd tn setItems
    (unresolvedPermFltr, whereExp) mutFlds allCols
  where
    UpdOpCtx tn _ filterExp preSetCols allCols = opCtx
    args = _fArguments fld

convertUpdate
  :: ( MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => UpdOpCtx -- the update context
  -> Field -- the mutation field
  -> m RespTx
convertUpdate opCtx fld = do
  annUpdUnresolved <- convertUpdateP1 opCtx fld
  (annUpdResolved, prepArgs) <- withPrepArgs $ RU.traverseAnnUpd
                                resolveValPrep annUpdUnresolved
  strfyNum <- stringifyNum <$> asks getter
  let whenNonEmptyItems = return $ RU.updateQueryToTx strfyNum
                          (annUpdResolved, prepArgs)
      whenEmptyItems    = return $ return $
                          buildEmptyMutResp $ RU.uqp1MutFlds annUpdResolved
   -- if there are not set items then do not perform
   -- update and return empty mutation response
  bool whenNonEmptyItems whenEmptyItems $ null $ RU.uqp1SetExps annUpdResolved

convertDelete
  :: ( MonadError QErr m
     , MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r
     )
  => DelOpCtx -- the delete context
  -> Field -- the mutation field
  -> m RespTx
convertDelete opCtx fld = do
  whereExp <- withArg (_fArguments fld) "where" parseBoolExp
  mutFlds  <- convertMutResp (_fType fld) $ _fSelSet fld
  let unresolvedPermFltr =
        fmapAnnBoolExp partialSQLExpToUnresolvedVal filterExp
      annDelUnresolved = RD.AnnDel tn (unresolvedPermFltr, whereExp)
                         mutFlds allCols
  (annDelResolved, prepArgs) <- withPrepArgs $ RD.traverseAnnDel
                                resolveValPrep annDelUnresolved
  strfyNum <- stringifyNum <$> asks getter
  return $ RD.deleteQueryToTx strfyNum (annDelResolved, prepArgs)
  where
    DelOpCtx tn _ filterExp allCols = opCtx

-- | build mutation response for empty objects
buildEmptyMutResp :: RR.MutFlds -> EncJSON
buildEmptyMutResp = mkTx
  where
    mkTx = encJFromJValue . OMap.fromList . map (second convMutFld)
    -- generate empty mutation response
    convMutFld = \case
      RR.MCount -> J.toJSON (0 :: Int)
      RR.MExp e -> J.toJSON e
      RR.MRet _ -> J.toJSON ([] :: [J.Value])
