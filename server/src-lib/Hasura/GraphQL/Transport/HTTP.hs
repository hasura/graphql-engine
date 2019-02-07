module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.ByteString.Lazy                   as BL
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as V

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqParsed
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m EncJSON
runGQ pool isoL userInfo sqlGenCtx sc manager reqHdrs req rawReq = do
  execPlan <- E.getExecPlan userInfo sc req
  case execPlan of
    E.GExPHasura gCtx rootSelSet ->
      runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet
    E.GExPRemote rsi opDef  ->
      E.execRemoteGQ manager userInfo reqHdrs rawReq rsi opDef

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> GCtx
  -> V.RootSelSet
  -> m EncJSON
runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet = do
  tx <- case rootSelSet of
    V.RQuery selSet ->
      return $ R.resolveQuerySelSet userInfo gCtx sqlGenCtx selSet
    V.RMutation selSet ->
      return $ R.resolveMutSelSet userInfo gCtx sqlGenCtx selSet
    V.RSubscription _  ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
  where
    runTx tx = runLazyTx pool isoL $ withUserInfo userInfo tx
