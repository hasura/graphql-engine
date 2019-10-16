{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import qualified Data.Sequence                          as Seq
import qualified Data.Text                              as T
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo reqHdrs req = do
  E.ExecutionCtx logger sqlGenCtx pgExecCtx planCache sc scVer _ enableAL <- ask
  fieldPlans <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  (fieldResps, qLog) <- flip runStateT (QueryLog req [] reqId) $
    forM fieldPlans $ \case
    E.GQFieldResolvedHasura resolvedOp ->
      flip HttpResponse Nothing <$> runHasuraGQ userInfo resolvedOp
    E.GQFieldResolvedRemote rsi opType field ->
      E.execRemoteGQ userInfo reqHdrs rsi opType (Seq.singleton field)

  -- log the generated SQL and the graphql query
  liftIO $ logGraphqlQuery logger qLog

  let mergedResp = mergeResponses (fmap _hrBody fieldResps)
  case mergedResp of
    Left e ->
      throw400
        UnexpectedPayload
        ("could not merge data from results: " <> T.pack e)
    Right mergedGQResp ->
      pure (HttpResponse mergedGQResp (foldMap _hrHeaders fieldResps))

runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadState QueryLog m
     )
  => UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ userInfo resolvedOp = do
  E.ExecutionCtx{_ecxPgExecCtx} <- ask
  respE <- case resolvedOp of
    E.ExOpQuery tx genSql  -> do
      E.addToLog genSql
      liftIO $ runExceptT $ runLazyTx' _ecxPgExecCtx tx
    E.ExOpMutation tx -> do
      liftIO $ runExceptT $ runLazyTx _ecxPgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
