-- | Execution of GraphQL queries over HTTP transport
{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( MonadExecuteQuery(..)
  , runGQ
  , runGQBatched
  -- * imported from HTTP.Protocol; required by pro
  , GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , GQLQueryText(..)
  , ResultsFragment(..)
  ) where

import           Control.Monad.Morph                    (hoist)

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Logging                 (MonadQueryLog (..))
import           Hasura.GraphQL.Parser.Column           (UnpreparedValue)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                         (MonadTrace, TraceT, trace)

import qualified Data.Aeson.Ordered                     as JO
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai


class Monad m => MonadExecuteQuery m where
  executeQuery
    :: GQLReqParsed
    -> [QueryRootField UnpreparedValue]
    -> Maybe EQ.PreparedSql -- EQ.GeneratedSqlMap -- HashMap G.Name EQ.PreparedSql
    -> PGExecCtx
    -> Q.TxAccess
    -> TraceT (LazyTx QErr) EncJSON
    -> TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, EncJSON)

instance MonadExecuteQuery m => MonadExecuteQuery (ReaderT r m) where
  executeQuery a b c d e f = hoist (hoist lift) $ executeQuery a b c d e f

instance MonadExecuteQuery m => MonadExecuteQuery (ExceptT r m) where
  executeQuery a b c d e f = hoist (hoist lift) $ executeQuery a b c d e f

instance MonadExecuteQuery m => MonadExecuteQuery (TraceT m) where
  executeQuery a b c d e f = hoist (hoist lift) $ executeQuery a b c d e f

data ResultsFragment = ResultsFragment
  { rfTimeIO :: DiffTime
  , rfLocality :: Telem.Locality
  , rfResponse :: EncJSON
  , rfHeaders :: HTTP.ResponseHeaders
  }

-- | Run (execute) a single GraphQL query
runGQ
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> RequestId
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  (telemTimeTot_DT, (telemQueryType, telemTimeIO_DT, telemLocality, resp)) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx {- planCache -} sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <- E.getResolvedExecPlan env logger pgExecCtx {- planCache -}
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    case execPlan of
      E.QueryExecutionPlan queryPlans asts -> do
        results <- forWithKey queryPlans $ \fieldName -> \case
          E.ExecStepDB txGenSql -> do
            (telemTimeIO_DT, _telemQueryType, respHdrs, resp) <-
              runQueryDB reqId (reqUnparsed,reqParsed) asts userInfo txGenSql
            return $ ResultsFragment telemTimeIO_DT Telem.Local resp respHdrs
          E.ExecStepRemote (rsi, opDef, _varValsM) -> do
            (_telemCacheHit, _, (telemTimeIO_DT, _telemQueryType, HttpResponse resp respHdrs)) <- runRemoteGQ telemCacheHit rsi opDef
            value <- extractData fieldName $ encJToLBS resp
            pure $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) respHdrs
          E.ExecStepRaw json -> do
            let obj = encJFromJValue json
                telemTimeIO_DT = 0
            return $ ResultsFragment telemTimeIO_DT Telem.Local obj []
        let (durationsIO, localities, bodies, headers) =
              (fmap rfTimeIO results, fmap rfLocality results, fmap rfResponse results, fmap rfHeaders results)
        return $ (Telem.Query, sum durationsIO, fold localities, ) $ HttpResponse (encodeGQResp $ pure $ encJToLBS $ encJFromInsOrdHashMap bodies) (fold headers)

      E.MutationExecutionPlan mutationPlans -> do
        results <- forWithKey mutationPlans $ \fieldName -> \case
          E.ExecStepDB (tx, responseHeaders) -> do
            (telemTimeIO_DT, _telemQueryType, resp) <- runMutationDB reqId reqUnparsed userInfo tx
            return $ ResultsFragment telemTimeIO_DT Telem.Local resp responseHeaders
          E.ExecStepRemote (rsi, opDef, _varValsM) -> do
            (_telemCacheHit, _, (telemTimeIO_DT, _telemQueryType, HttpResponse resp respHdrs)) <- runRemoteGQ telemCacheHit rsi opDef
            value <- extractData fieldName $ encJToLBS resp
            pure $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) respHdrs
          E.ExecStepRaw json -> do
            let obj = encJFromJValue json
                telemTimeIO_DT = 0
            return $ ResultsFragment telemTimeIO_DT Telem.Local obj []
        let (durationsIO, localities, bodies, headers) =
              (fmap rfTimeIO results, fmap rfLocality results, fmap rfResponse results, fmap rfHeaders results)
        return $ (Telem.Mutation, sum durationsIO, fold localities, ) $ HttpResponse (encodeGQResp $ pure $ encJToLBS $ encJFromInsOrdHashMap bodies) (fold headers)

      E.SubscriptionExecutionPlan _sub ->
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
  -- The response and misc telemetry data:
  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT
      telemTransport = Telem.HTTP
      telemCacheHit = Telem.Miss -- TODO fix if we're reimplementing query caching
  -- Disabled for now until we make up our mind on the naming of localities
  when False $ Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp
  where
    forWithKey = flip OMap.traverseWithKey

    runRemoteGQ telemCacheHit rsi opDef = do
      let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                         | otherwise = Telem.Query
      (telemTimeIO, resp) <- E.execRemoteGQ env reqId userInfo reqHeaders reqUnparsed rsi opDef
      pure (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

    extractData :: Text -> LBS.ByteString -> m JO.Value
    extractData fieldName = runAesonParser $ \bs ->
      let lookup' key object = maybe (Left $ "expecting key " ++ T.unpack key) Right $ JO.lookup key object
      in either fail pure $
         JO.eitherDecode bs >>=
         JO.asObject        >>=
         lookup' "data"     >>=
         JO.asObject        >>=
         lookup' fieldName


-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs')
runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> RequestId
  -> ResponseInternalErrorsConfig
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLBatchedReqs GQLQueryText
  -- ^ the batched request with unparsed GraphQL query
  -> m (HttpResponse EncJSON)
runGQBatched env logger reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query =
  case query of
    GQLSingleRequest req ->
      runGQ env logger reqId userInfo ipAddress reqHdrs queryType req
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)

      removeHeaders <$> traverse (try . runGQ env logger reqId userInfo ipAddress reqHdrs queryType) reqs
  where
    try = flip catchError (pure . Left) . fmap Right


runQueryDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     )
  => RequestId
  -> (GQLReqUnparsed, GQLReqParsed)
  -> [QueryRootField UnpreparedValue]
  -> UserInfo
  -> (Tracing.TraceT (LazyTx QErr) EncJSON, Maybe EQ.PreparedSql)
  -> m (DiffTime, Telem.QueryType, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runQueryDB reqId (query, queryParsed) asts _userInfo (tx, genSql) =  do
  -- log the generated SQL and the graphql query
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  logQueryLog logger query Nothing reqId -- TODO genSql
  (telemTimeIO, respE) <- withElapsedTime $ runExceptT $ trace "Query" $
    Tracing.interpTraceT id $ executeQuery queryParsed asts Nothing pgExecCtx Q.ReadOnly tx -- TODO genSql
  (respHdrs,!resp) <- liftEither respE
  let telemQueryType = Telem.Query
  return (telemTimeIO, telemQueryType, respHdrs, resp)

runMutationDB
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     , MonadTrace m
     )
  => RequestId
  -> GQLReqUnparsed
  -> UserInfo
  -> Tracing.TraceT (LazyTx QErr) EncJSON
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutationDB reqId query userInfo tx =  do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  -- log the graphql query
  logQueryLog logger query Nothing reqId
  ctx <- Tracing.currentContext
  (telemTimeIO, respE) <- withElapsedTime $  runExceptT $ trace "Mutation" $
    Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadWrite . withTraceContext ctx .  withUserInfo userInfo)  tx
  !resp <- liftEither respE
  let telemQueryType = Telem.Mutation
  return (telemTimeIO, telemQueryType, resp)

{-
runHasuraGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , MonadQueryLog m
     , MonadTrace m
     , MonadExecuteQuery m
     )
  => RequestId
  -> (GQLReqUnparsed, GQLReqParsed)
  -> UserInfo
  -> E.ExecOp (Tracing.TraceT (LazyTx QErr))
  -> m (DiffTime, Telem.QueryType, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runHasuraGQ reqId (query, queryParsed) userInfo resolvedOp = do
  (E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _) <- ask
  (telemTimeIO, respE) <- withElapsedTime $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx genSql asts -> trace "Query" $ do
      -- log the generated SQL and the graphql query
      logQueryLog logger query genSql reqId
      Tracing.interpTraceT id $ executeQuery queryParsed asts genSql pgExecCtx Q.ReadOnly tx

    E.ExOpMutation respHeaders tx -> trace "Mutate" $ do
      logQueryLog logger query Nothing reqId
      ctx <- Tracing.currentContext
      (respHeaders,) <$>
        Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadWrite . withTraceContext ctx . withUserInfo userInfo) tx

    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"

  (respHdrs, resp) <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = case resolvedOp of E.ExOpMutation{} -> Telem.Mutation ; _ -> Telem.Query
  return (telemTimeIO, telemQueryType, respHdrs, json)
-}
