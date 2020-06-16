-- | Execution of GraphQL queries over HTTP transport
module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  -- * imported from HTTP.Protocol; required by pro
  , GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , GQLQueryText(..)
  ) where

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs')
runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     )
  => RequestId
  -> ResponseInternalErrorsConfig
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLBatchedReqs GQLQueryText
  -- ^ the batched request with unparsed GraphQL query
  -> m (HttpResponse EncJSON)
runGQBatched reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query = do
  case query of
    GQLSingleRequest req -> runGQ reqId userInfo ipAddress reqHdrs queryType req
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)

      fmap removeHeaders $ traverse (try . runGQ reqId userInfo ipAddress reqHdrs queryType) reqs
  where
    try = flip catchError (pure . Left) . fmap Right


-- | Run (execute) a single GraphQL query
runGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     )
  => RequestId
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  -- The response and misc telemetry data:
  let telemTransport = Telem.HTTP
  (telemTimeTot_DT, (telemCacheHit, telemLocality, (telemTimeIO_DT, telemQueryType, !resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <- E.getResolvedExecPlan pgExecCtx planCache
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    case execPlan of
      E.GExPHasura resolvedOp -> do
        (telemTimeIO, telemQueryType, respHdrs, resp) <- runHasuraGQ reqId reqUnparsed userInfo resolvedOp
        return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp respHdrs))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                           | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHeaders reqUnparsed rsi $ G._todType opDef
        pure (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT

  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp


runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -- ^ needs the unparsed req for logging
  -> UserInfo
  -> E.ExecOp
  -> m (DiffTime, Telem.QueryType, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runHasuraGQ reqId query userInfo resolvedOp = do
  (E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _) <- ask
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql -> do
      -- log the generated SQL and the graphql query
      L.unLogger logger $ QueryLog query genSql reqId
      ([],) <$> runLazyTx' pgExecCtx tx

    E.ExOpMutation respHeaders tx -> do
      -- log the graphql query
      L.unLogger logger $ QueryLog query Nothing reqId
      (respHeaders,) <$> runLazyTx pgExecCtx Q.ReadWrite (withUserInfo userInfo tx)

    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  (respHdrs, resp) <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = case resolvedOp of E.ExOpMutation{} -> Telem.Mutation ; _ -> Telem.Query
  return (telemTimeIO, telemQueryType, respHdrs, json)
