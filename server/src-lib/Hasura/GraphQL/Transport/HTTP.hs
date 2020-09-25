-- | Execution of GraphQL queries over HTTP transport
{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( MonadExecuteQuery(..)
  , runGQ
  , runGQBatched
  , extractFieldFromResponse
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
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai


class Monad m => MonadExecuteQuery m where
  executeQuery
    :: GQLReqParsed
    -> [QueryRootField UnpreparedValue]
    -> Maybe EQ.PreparedSql
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

    (_telemCacheHit, execPlan) <- E.getResolvedExecPlan env logger pgExecCtx {- planCache -}
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    case execPlan of
      E.QueryExecutionPlan queryPlans asts -> do
        conclusion <- runExceptT $ forWithKey queryPlans $ \fieldName -> \case
          E.ExecStepDB txGenSql -> doQErr $ do
            (telemTimeIO_DT, respHdrs, resp) <-
              runQueryDB reqId (reqUnparsed,reqParsed) asts userInfo txGenSql
            return $ ResultsFragment telemTimeIO_DT Telem.Local resp respHdrs
          E.ExecStepRemote (rsi, opDef, varValsM) ->
            runRemoteGQ fieldName rsi opDef varValsM
          E.ExecStepRaw json -> doQErr $ do
            let obj = encJFromJValue json
                telemTimeIO_DT = 0
            return $ ResultsFragment telemTimeIO_DT Telem.Local obj []
        buildResult Telem.Query conclusion

      E.MutationExecutionPlan mutationPlans -> do
        conclusion <- runExceptT $ forWithKey mutationPlans $ \fieldName -> \case
          E.ExecStepDB (tx, responseHeaders) -> doQErr $ do
            (telemTimeIO_DT, resp) <- runMutationDB reqId reqUnparsed userInfo tx
            return $ ResultsFragment telemTimeIO_DT Telem.Local resp responseHeaders
          E.ExecStepRemote (rsi, opDef, varValsM) ->
            runRemoteGQ fieldName rsi opDef varValsM
          E.ExecStepRaw json -> doQErr $ do
            let obj = encJFromJValue json
                telemTimeIO_DT = 0
            return $ ResultsFragment telemTimeIO_DT Telem.Local obj []
        buildResult Telem.Mutation conclusion

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
    doQErr = withExceptT Right

    forWithKey = flip OMap.traverseWithKey

    runRemoteGQ fieldName rsi opDef varValsM = do
      (telemTimeIO_DT, HttpResponse resp respHdrs) <-
        doQErr $ E.execRemoteGQ env reqId userInfo reqHeaders rsi opDef varValsM
      value <- extractFieldFromResponse fieldName $ encJToLBS resp
      pure $ ResultsFragment telemTimeIO_DT Telem.Remote (JO.toEncJSON value) respHdrs

    buildResult telemType (Left (Left err)) = pure
      ( telemType
      , 0
      , Telem.Remote
      , HttpResponse
        (encodeGQResp $ throwError $ err)
        []
      )
    buildResult _telemType (Left (Right err)) = throwError err
    buildResult telemType (Right results) = pure
      ( telemType
      , sum (fmap rfTimeIO results)
      , fold (fmap rfLocality results)
      , HttpResponse
        (encodeGQResp $ pure $ encJToLBS $ encJFromInsOrdHashMap (fmap rfResponse results))
        (fold (fmap rfHeaders results))
      )

extractFieldFromResponse
  :: Monad m => Text -> LBS.ByteString -> ExceptT (Either GQExecError QErr) m JO.Value
extractFieldFromResponse fieldName bs = do
  val <- case JO.eitherDecode bs of
    Left err -> doQErr $ throw400 RemoteSchemaError $ T.pack err
    Right v -> pure v
  valObj <- case JO.asObject val of
    Left err -> doQErr $ throw400 RemoteSchemaError err
    Right v -> pure v
  dataVal <- case JO.toList valObj of
    [("data", v)] -> pure v
    _ -> case JO.lookup "errors" valObj of
      Just (JO.Array err) -> doGQExecError $ throwError $ GQExecError $ toList $ fmap JO.fromOrdered err
      _ -> doQErr $ throw400 RemoteSchemaError "Received invalid JSON value from remote"
  dataObj <- case JO.asObject dataVal of
    Left err -> doQErr $ throw400 RemoteSchemaError err
    Right v -> pure v
  fieldVal <- case JO.lookup fieldName dataObj of
    Nothing -> doQErr $ throw400 RemoteSchemaError $ "expecting key " <> fieldName
    Just v -> pure v
  return fieldVal
  where
    doQErr = withExceptT Right
    doGQExecError = withExceptT Left

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
  -> m (DiffTime, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runQueryDB reqId (query, queryParsed) asts _userInfo (tx, _genSql) =  do
  -- log the generated SQL and the graphql query
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  logQueryLog logger query Nothing reqId -- TODO genSql
  (telemTimeIO, respE) <- withElapsedTime $ runExceptT $ trace "Query" $
    Tracing.interpTraceT id $ executeQuery queryParsed asts Nothing pgExecCtx Q.ReadOnly tx -- TODO genSql
  (respHdrs,!resp) <- liftEither respE
  return (telemTimeIO, respHdrs, resp)

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
  -> m (DiffTime, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runMutationDB reqId query userInfo tx =  do
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  -- log the graphql query
  logQueryLog logger query Nothing reqId
  ctx <- Tracing.currentContext
  (telemTimeIO, respE) <- withElapsedTime $ runExceptT $ trace "Mutation" $
    Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadWrite . withTraceContext ctx .  withUserInfo userInfo)  tx
  !resp <- liftEither respE
  return (telemTimeIO, resp)
