module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , getMergedGQResp
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
import qualified Hasura.GraphQL.Execute.RemoteJoins     as E
import qualified Language.GraphQL.Draft.Syntax          as G

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
  E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer _ enableAL <- ask
  fieldPlans <- E.getResolvedExecPlan pgExecCtx planCache
              userInfo sqlGenCtx enableAL sc scVer req
  fieldResps <- forM fieldPlans $ \case
    (remoteRelPlans, E.GQFieldResolvedHasura resolvedOp) -> do
      initJson <- runHasuraGQ reqId req userInfo resolvedOp
      let (initValue, joinInputs) =
            E.extractRemoteRelArguments initJson $
              map E.rrpRemoteRelField remoteRelPlans

      let batchesRemotePlans =
            map (uncurry E.mkQuery) $ catMaybes $
            map sequence $ zip remoteRelPlans joinInputs

      results <- forM batchesRemotePlans $ \(E.GQRemoteRelPlan remoteRelSplice rsi, batch) ->
        -- NOTE: discard remote headers (for now):
        (remoteRelSplice, ) <$>
        (fmap _hrBody $ E.execRemoteGQ reqId userInfo reqHdrs rsi G.OperationTypeQuery (Seq.fromList batch))

      pure $
        -- NOTE: preserve headers (see test_response_headers_from_remote)
        HttpResponse
          (E.encodeGQRespValue
             (E.joinResults initValue results))
          Nothing
    (_remoteRelPlans, E.GQFieldResolvedRemote rsi opType field) ->
      E.execRemoteGQ reqId userInfo reqHdrs rsi opType (Seq.singleton field)

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
      liftIO $ logGraphqlQuery logger $ QueryLog query genSql reqId
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx -> do
      -- log the graphql query
      liftIO $ logGraphqlQuery logger $ QueryLog query Nothing reqId
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp
