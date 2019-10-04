module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , getMergedGQResp
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
  -- One for each top-level field in the client's query:
  execPlans <-
    E.getResolvedExecPlan pgExecCtx planCache userInfo sqlGenCtx enableAL sc scVer req
  topLevelResults <-
    forM execPlans $ \(unresolvedPlans, resolvedPlan) -> do
      HttpResponse initJson initHeaders <- runLeafPlan resolvedPlan
      let (initValue, joinInputs) =
            E.extractRemoteRelArguments initJson $
              map E.remoteRelField unresolvedPlans

      let batchesRemotePlans =
            -- TODO pass 'G.OperationType' properly when we support mutations, etc.
            map (uncurry $ E.mkQuery G.OperationTypeQuery) $ catMaybes $
            map sequence $ zip unresolvedPlans joinInputs 

      results <- forM batchesRemotePlans $
        -- NOTE: discard remote headers (for now):
        traverse (fmap _hrBody . runLeafPlan . E.ExPRemote)

      pure $
        -- NOTE: preserve headers (see test_response_headers_from_remote)
        HttpResponse
          (E.encodeGQRespValue
             (E.joinResults initValue results))
          initHeaders
  let mergedRespResult = mergeResponseData (fmap _hrBody topLevelResults)
  case mergedRespResult of
    Left e ->
      throw400
        UnexpectedPayload
        ("could not merge data from results: " <> T.pack e)
    Right mergedResp ->
      pure (HttpResponse mergedResp (foldMap _hrHeaders topLevelResults))
  where
    runLeafPlan = \case
      E.ExPHasura resolvedOp -> do
        hasuraJson <- runHasuraGQ reqId req userInfo resolvedOp
        pure (HttpResponse hasuraJson Nothing)
      E.ExPRemote RemoteTopField{..} ->
        E.execRemoteGQ
          reqId
          userInfo
          reqHdrs
          rtqOperationType
          rtqRemoteSchemaInfo
          (Right rtqFields)

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

-- | See 'mergeResponseData'.
getMergedGQResp :: Traversable t=> t EncJSON -> Either String GQRespValue
getMergedGQResp =
  mergeGQResp <=< traverse E.parseGQRespValue
  where mergeGQResp = flip foldM E.emptyResp $ \respAcc E.GQRespValue{..} ->
          respAcc & E.gqRespErrors <>~ _gqRespErrors
                  & mapMOf E.gqRespData (OJ.safeUnion _gqRespData)

-- | Union several graphql responses, with the ordering of the top-level fields
-- determined by the input list.
mergeResponseData :: Traversable t=> t EncJSON -> Either String EncJSON
mergeResponseData =
  fmap E.encodeGQRespValue . getMergedGQResp
