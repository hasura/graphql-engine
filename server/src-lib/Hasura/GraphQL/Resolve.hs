{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Resolve
  ( resolveSelSet
  , resolveQuerySelSet
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.ByteString.Lazy                   as BL
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Resolve.Introspect
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Field
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Execute.Plan            as Plan
import qualified Hasura.GraphQL.Resolve.Mutation        as RM
import qualified Hasura.GraphQL.Resolve.Select          as RS

-- {-# SCC buildTx #-}
buildTx :: UserInfo -> GCtx -> Field -> Q.TxE QErr BL.ByteString
buildTx userInfo gCtx fld = do
  opCxt <- getOpCtx $ _fName fld
  join $ fmap fst $ runConvert (fldMap, orderByCtx) $ case opCxt of

    -- OCSelect tn permFilter permLimit hdrs ->
    --   validateHdrs hdrs >> RS.convertSelect tn permFilter permLimit fld
      -- RS.convertSelect tn permFilter fld
    OCInsert tn vn cols hdrs    ->
      validateHdrs hdrs >> RM.convertInsert roleName (tn, vn) cols fld
      -- RM.convertInsert (tn, vn) cols fld
    OCUpdate tn permFilter hdrs ->
      validateHdrs hdrs >> RM.convertUpdate tn permFilter fld
      -- RM.convertUpdate tn permFilter fld
    OCDelete tn permFilter hdrs ->
      validateHdrs hdrs >> RM.convertDelete tn permFilter fld
      -- RM.convertDelete tn permFilter fld
    OCSelect {} -> throw500 "unexpected OCSelect for a mutation root field"
  where
    roleName = userRole userInfo
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByEnums gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    validateHdrs hdrs = do
      let receivedHdrs = userHeaders userInfo
      forM_ hdrs $ \hdr ->
        unless (Map.member hdr receivedHdrs) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

resolveQuerySelSet
  :: (MonadError QErr m)
  => UserInfo
  -> GCtx
  -> SelSet
  -> m [(G.Alias, Plan.RootFieldPlan)]
resolveQuerySelSet userInfo gCtx fields =
  forM (toList fields) $ \fld -> do
    fldResp <- resolveQueryFld userInfo gCtx fld
    return (_fAlias fld, fldResp)

resolveQueryFld
  :: (MonadError QErr m)
  => UserInfo
  -> GCtx
  -> Field
  -> m Plan.RootFieldPlan
resolveQueryFld userInfo gCtx fld =
  case _fName fld of
    "__type"     -> Plan.RFPRaw . J.encode <$> runReaderT (typeR fld) gCtx
    "__schema"   -> Plan.RFPRaw . J.encode <$> runReaderT (schemaR fld) gCtx
    "__typename" -> return $ Plan.RFPRaw $ J.encode queryRoot
    _ -> do
      opCxt <- getOpCtx $ _fName fld
      RS.runPlanM (fldMap, orderByCtx) $ case opCxt of
        OCSelect tn permFilter permLimit hdrs ->
          validateHdrs hdrs >> RS.convertSelect2 tn permFilter permLimit fld
        _ -> throw500 "expecting OCSelect for a query root field"
  where
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByEnums gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    validateHdrs hdrs = do
      let receivedHdrs = userHeaders userInfo
      forM_ hdrs $ \hdr ->
        unless (Map.member hdr receivedHdrs) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

    queryRoot :: Text
    queryRoot = "query_root"

-- {-# SCC resolveFld #-}
resolveFld
  :: UserInfo -> GCtx
  -> G.OperationType
  -> Field
  -> Q.TxE QErr BL.ByteString
resolveFld userInfo gCtx opTy fld =
  case _fName fld of
    "__type"     -> J.encode <$> runReaderT (typeR fld) gCtx
    "__schema"   -> J.encode <$> runReaderT (schemaR fld) gCtx
    "__typename" -> return $ J.encode $ mkRootTypeName opTy
    _            -> buildTx userInfo gCtx fld
  where
    mkRootTypeName :: G.OperationType -> Text
    mkRootTypeName = \case
      G.OperationTypeQuery        -> "query_root"
      G.OperationTypeMutation     -> "mutation_root"
      G.OperationTypeSubscription -> "subscription_root"

resolveSelSet
  :: UserInfo -> GCtx
  -> G.OperationType
  -> SelSet
  -> Q.TxE QErr BL.ByteString
resolveSelSet userInfo gCtx opTy fields =
  fmap mkJSONObj $ forM (toList fields) $ \fld -> do
    fldResp <- resolveFld userInfo gCtx opTy fld
    return (G.unName $ G.unAlias $ _fAlias fld, fldResp)
