{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , reqToTx
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy                   as BL
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Plan                    as EP
import qualified Hasura.GraphQL.QueryPlanCache          as QP
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.Server.Query                    as RQ

reqToTx
  :: (MonadIO m, MonadError QErr m)
  => UserInfo -> GCtxMap
  -> QP.QueryPlanCache
  -> GraphQLRequest
  -> m (G.OperationType, Q.TxE QErr BL.ByteString)
reqToTx userInfo gCtxMap planCache req = do
  queryPlanM <- liftIO $ QP.getQueryPlan (userRole userInfo) req planCache
  case queryPlanM of
    Just queryPlan -> do
      (isSubs, tx) <- flip runReaderT gCtx $
        EP.mkNewQueryTx (_grVariables req) queryPlan
      let opTy = bool G.OperationTypeQuery G.OperationTypeSubscription isSubs
      return (opTy, tx)
    Nothing -> do
      (varDefs, opTy, fields) <- runReaderT (VQ.validateGQ req) gCtx
      (opTy,) <$> case opTy of
        G.OperationTypeMutation ->
          return $ R.resolveSelSet userInfo gCtx opTy fields
        _ -> do
          let isSubs = opTy == G.OperationTypeSubscription
          queryPlan <- EP.QueryPlan isSubs varDefs <$>
                       R.resolveQuerySelSet userInfo gCtx fields
          when (EP.isReusable queryPlan) $
            liftIO $ QP.addQueryPlan (userRole userInfo) req queryPlan planCache
          return $ EP.mkCurPlanTx queryPlan
  where
    gCtx = getGCtx (userRole userInfo) gCtxMap

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtxMap
  -> QP.QueryPlanCache
  -> GraphQLRequest
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxMap planCache req = do
  (opTy, tx) <- reqToTx userInfo gCtxMap planCache req
  when (opTy == G.OperationTypeSubscription) $
    throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx userInfo >> tx
