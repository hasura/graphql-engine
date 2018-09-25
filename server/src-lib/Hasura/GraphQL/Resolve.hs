{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Resolve
  ( resolveSelSet
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

import qualified Hasura.GraphQL.Resolve.Insert          as RI
import qualified Hasura.GraphQL.Resolve.Mutation        as RM
import qualified Hasura.GraphQL.Resolve.Select          as RS

-- {-# SCC buildTx #-}
buildTx :: UserInfo -> GCtx -> Field -> Q.TxE QErr BL.ByteString
buildTx userInfo gCtx fld = do
  opCxt <- getOpCtx $ _fName fld
  join $ fmap fst $ runConvert (fldMap, orderByCtx, insCtxMap) $ case opCxt of

    OCSelect tn permFilter permLimit hdrs ->
      validateHdrs hdrs >> RS.convertSelect tn permFilter permLimit fld

    OCSelectPkey tn permFilter hdrs ->
      validateHdrs hdrs >> RS.convertSelectByPKey tn permFilter fld
      -- RS.convertSelect tn permFilter fld
    OCInsert tn hdrs    ->
      validateHdrs hdrs >> RI.convertInsert roleName tn fld
      -- RM.convertInsert (tn, vn) cols fld
    OCUpdate tn permFilter hdrs ->
      validateHdrs hdrs >> RM.convertUpdate tn permFilter fld
      -- RM.convertUpdate tn permFilter fld
    OCDelete tn permFilter hdrs ->
      validateHdrs hdrs >> RM.convertDelete tn permFilter fld
      -- RM.convertDelete tn permFilter fld
  where
    roleName = userRole userInfo
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByEnums gCtx
    insCtxMap = _gInsCtxMap gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    validateHdrs hdrs = do
      let receivedHdrs = userHeaders userInfo
      forM_ hdrs $ \hdr ->
        unless (Map.member hdr receivedHdrs) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

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
