module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.ByteString.Lazy                   as BL
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> E.PlanCache
  -> SchemaCache
  -> SchemaCacheVer
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m EncJSON
runGQ pool isoL userInfo sqlGenCtx planCache sc scVer manager reqHdrs req rawReq = do
  execPlan <- E.getResolvedExecPlan planCache userInfo sqlGenCtx sc scVer req
  case execPlan of
    E.GExPHasura resolvedOp ->
      runHasuraGQ pool isoL userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      E.execRemoteGQ manager userInfo reqHdrs rawReq rsi opDef

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> E.ResolvedOp
  -> m EncJSON
runHasuraGQ pool isoL userInfo resolvedOp = do
  opTx <- case resolvedOp of
    E.ROQuery tx    -> return tx
    E.ROMutation tx -> return tx
    E.ROSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftIO (runExceptT $ runTx opTx) >>= liftEither
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
  where
    runTx tx = runLazyTx pool isoL $ withUserInfo userInfo tx
