module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.ByteString.Lazy                   as BL
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> SchemaCache
  -> SchemaCacheVer
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m (HttpResponse EncJSON)
runGQ pgExecCtx userInfo sqlGenCtx enableAL planCache sc scVer
  manager reqHdrs req rawReq = do
  execPlans <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  results <- forM execPlans $ \execPlan ->
    case execPlan of
      E.GExPHasura resolvedOp -> do
        encJson <- runHasuraGQ pgExecCtx userInfo resolvedOp
        pure (HttpResponse encJson Nothing)
      E.GExPRemote rsi ->
        E.execRemoteGQ manager userInfo reqHdrs rawReq rsi
  pure
    (HttpResponse
       (encJFromList (toList (fmap _hrBody results)))
       (foldMap _hrHeaders results))

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ pgExecCtx userInfo resolvedOp = do
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx    ->
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx ->
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
