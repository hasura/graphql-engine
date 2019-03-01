module Hasura.GraphQL.Resolve
  ( resolveSelSet
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G


import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.Introspect
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Field
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.Insert          as RI
import qualified Hasura.GraphQL.Resolve.Mutation        as RM
import qualified Hasura.GraphQL.Resolve.Select          as RS

-- {-# SCC buildTx #-}
buildTx :: UserInfo -> GCtx -> Field -> Q.TxE QErr BL.ByteString
buildTx userInfo gCtx fld = do
  opCxt <- getOpCtx $ _fName fld
  join $ fmap fst $ runConvert (fldMap, orderByCtx, insCtxMap) $ case opCxt of

    OCSelect ctx ->
      validateHdrs (_socHeaders ctx) >> RS.convertSelect ctx fld

    OCSelectPkey ctx ->
      validateHdrs (_spocHeaders ctx) >> RS.convertSelectByPKey ctx fld

    OCSelectAgg ctx ->
      validateHdrs (_socHeaders ctx) >> RS.convertAggSelect ctx fld

    OCFuncQuery ctx ->
      validateHdrs (_fqocHeaders ctx) >> RS.convertFuncQuery ctx False fld

    OCFuncAggQuery ctx ->
      validateHdrs (_fqocHeaders ctx) >> RS.convertFuncQuery ctx True fld

    OCInsert ctx    ->
      validateHdrs (_iocHeaders ctx) >> RI.convertInsert roleName (_iocTable ctx) fld

    OCUpdate ctx ->
      validateHdrs (_uocHeaders ctx) >> RM.convertUpdate ctx fld

    OCDelete ctx ->
      validateHdrs (_docHeaders ctx) >> RM.convertDelete ctx fld
  where
    roleName = userRole userInfo
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx
    insCtxMap = _gInsCtxMap gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    validateHdrs hdrs = do
      let receivedVars = userVars userInfo
      forM_ hdrs $ \hdr ->
        unless (isJust $ getVarVal hdr receivedVars) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

-- {-# SCC resolveFld #-}
resolveFld
  :: (MonadTx m)
  => UserInfo -> GCtx
  -> G.OperationType
  -> Field
  -> m BL.ByteString
resolveFld userInfo gCtx opTy fld =
  case _fName fld of
    "__type"     -> J.encode <$> runReaderT (typeR fld) gCtx
    "__schema"   -> J.encode <$> runReaderT (schemaR fld) gCtx
    "__typename" -> return $ J.encode $ mkRootTypeName opTy
    _            -> liftTx $ buildTx userInfo gCtx fld
  where
    mkRootTypeName :: G.OperationType -> Text
    mkRootTypeName = \case
      G.OperationTypeQuery        -> "query_root"
      G.OperationTypeMutation     -> "mutation_root"
      G.OperationTypeSubscription -> "subscription_root"

resolveSelSet
  :: (MonadTx m)
  => UserInfo -> GCtx
  -> G.OperationType
  -> SelSet
  -> m BL.ByteString
resolveSelSet userInfo gCtx opTy fields =
  fmap mkJSONObj $ forM (toList fields) $ \fld -> do
    fldResp <- resolveFld userInfo gCtx opTy fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)
