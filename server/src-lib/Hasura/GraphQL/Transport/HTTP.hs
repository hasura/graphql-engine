{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy                   as BL
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.Server.Query                    as RQ

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtxMap
  -> GraphQLRequest
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxMap req = do
  (opTy, fields) <- runReaderT (VQ.validateGQ req) gCtx
  when (opTy == G.OperationTypeSubscription) $ throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  let tx = R.resolveSelSet userInfo gCtx opTy fields
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    gCtx = getGCtx (userRole userInfo) gCtxMap
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx userInfo >> tx
