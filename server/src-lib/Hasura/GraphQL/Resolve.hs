module Hasura.GraphQL.Resolve
  ( resolveSelSet
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.GraphQL.Context
import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.Introspect
import           Hasura.GraphQL.Validate.Field
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.Insert     as RI
import qualified Hasura.GraphQL.Resolve.Mutation   as RM
import qualified Hasura.GraphQL.Resolve.Select     as RS

-- {-# SCC buildTx #-}
buildTx :: UserInfo -> GCtx -> SQLGenCtx -> Field -> Q.TxE QErr EncJSON
buildTx userInfo gCtx sqlCtx fld = do
  opCxt <- getOpCtx $ _fName fld
  join $ fmap fst $ runConvert ( fldMap
                               , orderByCtx
                               , insCtxMap
                               , sqlCtx
                               ) $ case opCxt of

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
  => UserInfo -> GCtx -> SQLGenCtx
  -> G.OperationType
  -> Field
  -> m EncJSON
resolveFld userInfo gCtx sqlGenCtx opTy fld =
  case _fName fld of
    "__type"     -> encJFromJValue <$> runReaderT (typeR fld) gCtx
    "__schema"   -> encJFromJValue <$> runReaderT (schemaR fld) gCtx
    "__typename" -> return $ encJFromJValue $ mkRootTypeName opTy
    _            -> liftTx $ buildTx userInfo gCtx sqlGenCtx fld
  where
    mkRootTypeName :: G.OperationType -> Text
    mkRootTypeName = \case
      G.OperationTypeQuery        -> "query_root"
      G.OperationTypeMutation     -> "mutation_root"
      G.OperationTypeSubscription -> "subscription_root"

resolveSelSet
  :: (MonadTx m)
  => UserInfo -> GCtx -> SQLGenCtx
  -> G.OperationType
  -> SelSet
  -> m EncJSON
resolveSelSet userInfo gCtx sqlGenCtx opTy fields =
  fmap encJFromAssocList $ forM (toList fields) $ \fld -> do
    fldResp <- resolveFld userInfo gCtx sqlGenCtx opTy fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)
