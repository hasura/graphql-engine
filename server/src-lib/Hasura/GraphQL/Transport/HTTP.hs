{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  ) where

import qualified Network.HTTP.Types                     as N

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

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Language.GraphQL.Draft.Syntax          as G

runGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> E.GraphQLQueryType
  -> GQLReq GQLQueryText
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo reqHdrs queryType req = do
  -- The response and misc telemetry data:
  let telemTransport = Telem.HTTP
  (telemTimeTot_DT, (telemCacheHit, telemLocality, (telemTimeIO_DT, telemQueryType, !resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer httpManager enableAL <- ask
    (telemCacheHit, execPlan) <- E.getResolvedExecPlan pgExecCtx planCache
                                 userInfo sqlGenCtx enableAL sc scVer queryType httpManager reqHdrs req
    case execPlan of
      E.QueryExecutionPlan queryPlan -> do
        case queryPlan of
          E.ExecStepDB txGenSql -> do
            (telemTimeIO, telemQueryType, resp) <- runQueryDB reqId req userInfo txGenSql
            return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp []))
          E.ExecStepRemote (rsi, opDef, _varValsM) ->
            runRemoteGQ telemCacheHit rsi opDef
          E.ExecStepRaw (name, json) -> do
            (telemTimeIO, obj) <- withElapsedTime $ do
              return $ encJFromJValue $ J.Object $ Map.singleton (G.unName name) json
            return (telemCacheHit, Telem.Local, (telemTimeIO, Telem.Query, HttpResponse obj []))
      E.MutationExecutionPlan mutationPlan -> do
        case mutationPlan of
          E.ExecStepDB (tx, responseHeaders) -> do
            (telemTimeIO, telemQueryType, resp) <- runMutationDB reqId req userInfo tx
            return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp responseHeaders))
          E.ExecStepRemote (rsi, opDef, _varValsM) ->
            runRemoteGQ telemCacheHit rsi opDef
          E.ExecStepRaw (name, json) -> do
            (telemTimeIO, obj) <- withElapsedTime $ do
              return $ encJFromJValue $ J.Object $ Map.singleton (G.unName name) json
            return (telemCacheHit, Telem.Local, (telemTimeIO, Telem.Query, HttpResponse obj []))
      E.SubscriptionExecutionPlan _sub -> do
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
{-
      E.GExPHasura resolvedOp -> do
        (telemTimeIO, telemQueryType, respHdrs, resp) <- runHasuraGQ reqId req userInfo resolvedOp
        return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp respHdrs))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                           | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHdrs req rsi $ G._todType opDef
        return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))
-}
  let telemTimeIO = fromUnits telemTimeIO_DT
      telemTimeTot = fromUnits telemTimeTot_DT
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp
  where
    runRemoteGQ telemCacheHit rsi opDef = do
      let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                         | otherwise = Telem.Query
      (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHdrs req rsi opDef
      return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> ResponseInternalErrorsConfig
  -> UserInfo
  -> [N.Header]
  -> E.GraphQLQueryType
  -> GQLBatchedReqs GQLQueryText
  -> m (HttpResponse EncJSON)
runGQBatched reqId responseErrorsConfig userInfo reqHdrs queryType query =
  case query of
    GQLSingleRequest req ->
      runGQ reqId userInfo reqHdrs queryType req
    GQLBatchedReqs batch -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)
          try = flip catchError (pure . Left) . fmap Right
      removeHeaders <$> traverse (try . runGQ reqId userInfo reqHdrs queryType) batch

runQueryDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> (LazyRespTx, EQ.GeneratedSqlMap)
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runQueryDB reqId query userInfo (tx, genSql) = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ do
    -- log the generated SQL and the graphql query
    L.unLogger logger $ QueryLog query (Just genSql) reqId
    runQueryTx pgExecCtx tx
  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = Telem.Query
  return (telemTimeIO, telemQueryType, json)

runMutationDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> LazyRespTx
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutationDB reqId query userInfo tx = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ do
    -- log the graphql query
    L.unLogger logger $ QueryLog query Nothing reqId
    runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo tx
  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = Telem.Mutation
  return (telemTimeIO, telemQueryType, json)

{-
runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m (DiffTime, Telem.QueryType, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runHasuraGQ reqId query userInfo resolvedOp = do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _ <- ask
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql  -> do
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
-}
