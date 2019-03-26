module Hasura.GraphQL.Resolve
  ( resolveQuerySelSet
  , resolveMutSelSet
  , resolveSubsFld
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.Introspect
import           Hasura.GraphQL.Validate.Field
import           Hasura.RQL.Types
import           Hasura.SQL.Types

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

resolvePGFld
  :: UserInfo
  -> GCtx
  -> ServeOptsCtx
  -> Field
  -> Q.TxE QErr EncJSON
resolvePGFld userInfo gCtx sqlCtx fld = do
  opCxt <- getOpCtx $ _fName fld
  join $ runConvert (fldMap, orderByCtx, insCtxMap, sqlCtx) $ case opCxt of
    OCSelect ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      RS.convertSelect ctx fld
    OCSelectPkey ctx -> do
      validateHdrs userInfo (_spocHeaders ctx)
      RS.convertSelectByPKey ctx fld
    OCSelectAgg ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      RS.convertAggSelect ctx fld
    OCFuncQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      RS.convertFuncQuery ctx False fld
    OCFuncAggQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      RS.convertFuncQuery ctx True fld
    OCInsert ctx -> do
      validateHdrs userInfo (_iocHeaders ctx)
      RI.convertInsert roleName (_iocTable ctx) fld
    OCUpdate ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      RM.convertUpdate ctx fld
    OCDelete ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      RM.convertDelete ctx fld
  where
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx
    insCtxMap = _gInsCtxMap gCtx
    roleName = userRole userInfo

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

mkRootTypeName :: G.OperationType -> Text
mkRootTypeName = \case
  G.OperationTypeQuery        -> "query_root"
  G.OperationTypeMutation     -> "mutation_root"
  G.OperationTypeSubscription -> "subscription_root"

resolveQuerySelSet
  :: (MonadTx m)
  => UserInfo
  -> GCtx
  -> ServeOptsCtx
  -> SelSet
  -> m EncJSON
resolveQuerySelSet userInfo gCtx sqlGenCtx fields =
  fmap encJFromAssocList $ forM (toList fields) $ \fld -> do
    fldResp <- case _fName fld of
      "__type"     -> encJFromJValue <$> runReaderT (typeR fld) gCtx
      "__schema"   -> encJFromJValue <$> runReaderT (schemaR fld) gCtx
      "__typename" -> return $ encJFromJValue $
                      mkRootTypeName G.OperationTypeQuery
      _            -> liftTx $ resolvePGFld userInfo gCtx sqlGenCtx fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)

resolveMutSelSet
  :: (MonadTx m)
  => UserInfo
  -> GCtx
  -> ServeOptsCtx
  -> SelSet
  -> m EncJSON
resolveMutSelSet userInfo gCtx sqlGenCtx fields =
  fmap encJFromAssocList $ forM (toList fields) $ \fld -> do
    fldResp <- case _fName fld of
      "__typename" -> return $ encJFromJValue $
                      mkRootTypeName G.OperationTypeMutation
      _            -> liftTx $ resolvePGFld userInfo gCtx sqlGenCtx fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)

resolveSubsFld
  :: (MonadTx m)
  => UserInfo
  -> GCtx
  -> ServeOptsCtx
  -> Field
  -> m EncJSON
resolveSubsFld userInfo gCtx sqlGenCtx fld = do
  resp <- case _fName fld of
    "__typename" -> return $ encJFromJValue $
                    mkRootTypeName G.OperationTypeSubscription
    _            -> liftTx $ resolvePGFld userInfo gCtx sqlGenCtx fld
  return $ encJFromAssocList [(G.unName $ G.unAlias $ _fAlias fld, resp)]
