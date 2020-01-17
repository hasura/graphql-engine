module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  ) where

import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.Logging                         as L

runGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLReq GQLQueryText
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo reqHdrs req = do
  E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer _ enableAL <- ask
  execPlan <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  case execPlan of
    E.GExPHasura resolvedOp ->
      flip HttpResponse Nothing <$> runHasuraGQ reqId req userInfo resolvedOp
    E.GExPRemote rsi opDef  ->
      E.execRemoteGQ reqId userInfo reqHdrs req rsi opDef

runGQBatched
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLBatchedReqs GQLQueryText
  -> m (HttpResponse EncJSON)
runGQBatched reqId userInfo reqHdrs reqs =
  case reqs of
    GQLSingleRequest req ->
      runGQ reqId userInfo reqHdrs req
    GQLBatchedReqs batch -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let removeHeaders = 
            flip HttpResponse Nothing 
            . encJFromList 
            . map (either (encJFromJValue . encodeGQErr False) _hrBody)
          try = flip catchError (pure . Left) . fmap Right
      fmap removeHeaders $ 
        traverse (try . runGQ reqId userInfo reqHdrs) batch

runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ reqId query userInfo resolvedOp = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      -- log the generated SQL and the graphql query
      L.unLogger logger $ QueryLog query genSql reqId
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx -> do
      -- log the graphql query
      L.unLogger logger $ QueryLog query Nothing reqId
      runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
