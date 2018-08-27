{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Hasura.GraphQL.Execute
  ( reqToTx
  , EC.QueryCache
  , EC.initQueryCache
  , EC.clearQueryCache

  , EP.QueryPlan(..)
  , EP.PGPlan(..)
  , EP.RootFieldPlan
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy                   as BL
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.QueryCache      as EC
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ

reqToTx
  :: (MonadIO m, MonadError QErr m)
  => UserInfo
  -> GCtxMap
  -> EC.QueryCache
  -> GQLReqUnparsed
  -> m (G.OperationType, GQLReqParsed, Q.TxE QErr BL.ByteString)
reqToTx userInfo gCtxMap queryCache unParsedReq = do
  astM <- liftIO $ EC.getAST (_grQuery unParsedReq) queryCache
  req <- maybe (toParsed unParsedReq)
         (\ast -> return $ unParsedReq { _grQuery = ast}) astM

  queryPlanM <- liftIO $ EC.getPlan (userRole userInfo) req queryCache
  case queryPlanM of
    Just queryPlan -> do
      (isSubs, tx) <- flip runReaderT gCtx $
        EP.mkNewQueryTx (_grVariables req) queryPlan
      let opTy = bool G.OperationTypeQuery G.OperationTypeSubscription isSubs
      return (opTy, req, tx)
    Nothing -> do
      (varDefs, opTy, fields) <- runReaderT (VQ.validateGQ req) gCtx
      (opTy, req,) <$> case opTy of
        G.OperationTypeMutation ->
          return $ R.resolveSelSet userInfo gCtx opTy fields
        _ -> do
          let isSubs = opTy == G.OperationTypeSubscription
          queryPlan <- EP.QueryPlan isSubs varDefs <$>
                       R.resolveQuerySelSet userInfo gCtx fields
          when (EP.isReusable queryPlan) $
            liftIO $ EC.addPlan (userRole userInfo) req queryPlan queryCache
          return $ EP.mkCurPlanTx queryPlan
  where
    gCtx = getGCtx (userRole userInfo) gCtxMap
