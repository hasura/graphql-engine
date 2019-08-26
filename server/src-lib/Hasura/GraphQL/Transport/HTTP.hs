module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Control.Lens
import qualified Data.Aeson.Ordered                     as OJ
import qualified Data.Text                              as T
import qualified Network.HTTP.Types                     as N
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Validate
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.RemoteJoins     as E

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
  execPlans <-
    E.getExecPlan pgExecCtx planCache userInfo sqlGenCtx enableAL sc scVer req
  results <-
    forM execPlans $ \execPlan ->
      case execPlan of
        E.Leaf plan -> runLeafPlan plan
        E.Tree resolvedPlan unresolvedPlansNE -> do
          let unresolvedPlans = toList unresolvedPlansNE -- it's safe to convert here
          HttpResponse initJson _ <- runLeafPlan resolvedPlan
          let remoteRels =
                map
                  (\(E.QExecPlanUnresolved remoteRelField _) -> remoteRelField)
                  unresolvedPlans
          let (initValue, remoteBatchInputs) =
                E.extractRemoteRelArguments
                  initJson
                  remoteRels

          let joinParamsPartial = E.JoinParams G.OperationTypeQuery -- TODO: getOpType
              resolvedPlansWithBatches =
                zipWith (E.mkQuery . joinParamsPartial) remoteBatchInputs unresolvedPlans 
                  -- TODO ^ can we be sure we're not throwing away a tail of either of these lists?
          results <-
            traverse
              (\(batch, resolvedSubPlan) -> do
                 HttpResponse res _ <- runLeafPlan resolvedSubPlan
                 pure (batch, res))
              resolvedPlansWithBatches
          pure $
            HttpResponse
                 (E.encodeGQRespValue
                    (E.joinResults initValue $ toList results))
              Nothing
  let mergedRespResult = mergeResponseData (toList (fmap _hrBody results))
  case mergedRespResult of
    Left e ->
      throw400
        UnexpectedPayload
        ("could not merge data from results: " <> T.pack e)
    Right mergedResp ->
      pure (HttpResponse mergedResp (foldMap _hrHeaders results))
  where
    runLeafPlan plan =
      case plan of
        E.ExPHasura resolvedOp -> do
          hasuraJson <- runHasuraGQ reqId req userInfo resolvedOp
          pure (HttpResponse hasuraJson Nothing)
        E.ExPRemote rt -> do
          let (rsi, fields) = remoteTopQueryEither rt
          resp@(HttpResponse _res _) <-
            E.execRemoteGQ
              reqId
              userInfo
              reqHdrs
              (rtqOperationType rt)
              rsi
              fields
          return resp

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

-- | Merge the list of response objects by the @data@ key.
mergeResponseData :: [EncJSON] -> Either String EncJSON
mergeResponseData =
  fmap E.encodeGQRespValue . mergeGQResp <=< traverse E.parseGQRespValue

  where mergeGQResp = flip foldM E.emptyResp $ \respAcc E.GQRespValue{..} ->
          respAcc & E.gqRespErrors <>~ _gqRespErrors
                  & mapMOf E.gqRespData (OJ.safeUnion _gqRespData)
