module Hasura.GraphQL.Resolve
  ( resolveMutationSelSet
  , resolveQuerySelSet
  ) where

import qualified Data.HashMap.Strict               as Map
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.Introspect
import           Hasura.GraphQL.Validate.Field
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Execute.Plan       as Plan
import qualified Hasura.GraphQL.Resolve.Insert     as RI
import qualified Hasura.GraphQL.Resolve.Mutation   as RM
import qualified Hasura.GraphQL.Resolve.Select     as RS

validateHdrs
  :: (Foldable t, QErrM m) => UserInfo -> t Text -> m ()
validateHdrs userInfo hdrs = do
  let receivedVars = userVars userInfo
  forM_ hdrs $ \hdr ->
    unless (isJust $ getVarVal hdr receivedVars) $
    throw400 NotFound $ hdr <<> " header is expected but not found"

-- {-# SCC resolveMutationFld #-}
resolveMutationFld
  :: (MonadTx m)
  => UserInfo
  -> GCtx
  -> SQLGenCtx
  -> Field
  -> m EncJSON
resolveMutationFld userInfo gCtx sqlCtx fld =
  case _fName fld of
    "__typename" -> return $ encJFromJ $ mkRootTypeName G.OperationTypeMutation
    _            -> liftTx $ do
      opCxt <- getOpCtx $ _fName fld
      join $ fmap fst $ runConvert (fldMap, orderByCtx, insCtxMap, sqlCtx) $
        case opCxt of

        OCInsert ctx    -> do
          validateHdrs userInfo $ _iocHeaders ctx
          RI.convertInsert roleName (_iocTable ctx) fld

        OCUpdate ctx -> do
          validateHdrs userInfo $ _uocHeaders ctx
          RM.convertUpdate ctx fld

        OCDelete ctx -> do
          validateHdrs userInfo $ _docHeaders ctx
          RM.convertDelete ctx fld

        OCSelect {} ->
          throw500 "unexpected OCSelect in mutation root"

        OCSelectPkey {} ->
          throw500 "unexpected OCSelectPkey in mutation root"

        OCSelectAgg {} ->
          throw500 "unexpected OCSelectAgg in mutation root"

        OCFuncQuery {} ->
          throw500 "unexpected OCFuncQuery in mutation root"

        OCFuncAggQuery {} ->
          throw500 "unexpected OCFuncAggQuery in mutation root"

      where
        roleName = userRole userInfo
        opCtxMap = _gOpCtxMap gCtx
        fldMap = _gFields gCtx
        orderByCtx = _gOrdByCtx gCtx
        insCtxMap = _gInsCtxMap gCtx

        getOpCtx f =
          onNothing (Map.lookup f opCtxMap) $ throw500 $
          "lookup failed: opctx: " <> showName f

mkRootTypeName :: G.OperationType -> Text
mkRootTypeName = \case
  G.OperationTypeQuery        -> "query_root"
  G.OperationTypeMutation     -> "mutation_root"
  G.OperationTypeSubscription -> "subscription_root"

resolveMutationSelSet
  :: (MonadTx m)
  => UserInfo
  -> GCtx
  -> SQLGenCtx
  -> SelSet
  -> m EncJSON
resolveMutationSelSet userInfo gCtx sqlGenCtx fields =
  fmap encJFromAL $ forM (toList fields) $ \fld -> do
    fldResp <- resolveMutationFld userInfo gCtx sqlGenCtx fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)

resolveQuerySelSet
  :: (MonadError QErr m)
  => UserInfo
  -> GCtx
  -> SQLGenCtx
  -> SelSet
  -> m [(G.Alias, Plan.RootFieldPlan)]
resolveQuerySelSet userInfo gCtx sqlGenCtx fields =
  forM (toList fields) $ \fld -> do
    fldResp <- resolveQueryFld userInfo gCtx sqlGenCtx fld
    return (_fAlias fld, fldResp)

resolveQueryFld
  :: (MonadError QErr m)
  => UserInfo
  -> GCtx
  -> SQLGenCtx
  -> Field
  -> m Plan.RootFieldPlan
resolveQueryFld userInfo gCtx sqlGenCtx fld =
  case _fName fld of
    "__type"     -> Plan.RFPRaw . encJFromJ <$> runReaderT (typeR fld) gCtx
    "__schema"   -> Plan.RFPRaw . encJFromJ <$> runReaderT (schemaR fld) gCtx
    "__typename" -> return $ Plan.RFPRaw $ encJFromJ queryRoot
    _ -> do
      opCxt <- getOpCtx $ _fName fld
      RS.runPlanM (fldMap, orderByCtx, sqlGenCtx) $ case opCxt of
        OCSelect ctx -> do
          validateHdrs userInfo $ _socHeaders ctx
          RS.convertSelectWithPlan ctx fld
        OCSelectPkey ctx -> do
          validateHdrs userInfo $ _spocHeaders ctx
          RS.convertSelectByPKeyWithPlan ctx fld
        OCSelectAgg ctx -> do
          validateHdrs userInfo $ _socHeaders ctx
          RS.convertAggSelectWithPlan ctx fld
        OCFuncQuery ctx -> do
          validateHdrs userInfo $ _fqocHeaders ctx
          RS.convertFuncQueryWithPlan ctx False fld
        OCFuncAggQuery ctx -> do
          validateHdrs userInfo $ _fqocHeaders ctx
          RS.convertFuncQueryWithPlan ctx True fld
        OCInsert {}    ->
          throw500 "unexpected OCInsert in query_root"
        OCUpdate {} ->
          throw500 "unexpected OCUpdate in query_root"
        OCDelete {} ->
          throw500 "unexpected OCDelete in query_root"
  where
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx
    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f
    queryRoot :: Text
    queryRoot = "query_root"
