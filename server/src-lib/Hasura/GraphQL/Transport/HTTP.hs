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

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.Server.Query                    as RQ

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> GCtxMap
  -> E.QueryCache
  -> GQLReqUnparsed
  -> m BL.ByteString
runGQ pool isoL userInfo gCtxMap planCache req = do
  (opTy, _, tx) <- E.reqToTx userInfo gCtxMap planCache req
  when (opTy == G.OperationTypeSubscription) $
    throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    runTx tx =
      Q.runTx pool (isoL, Nothing) $
      RQ.setHeadersTx userInfo >> tx
