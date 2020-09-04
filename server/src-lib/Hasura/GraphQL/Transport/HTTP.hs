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
  ) where

import           Control.Concurrent.MVar
import           Control.Exception                      (catch)
import           Control.Monad.Morph                    (hoist)
import           Data.Maybe                             (fromJust)

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

import qualified Data.ByteString.Lazy                   as BS
-- For dirty construction of fake query execution durations
import           Data.Time.Clock
-- Dirty result construction
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as TE

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Database.MySQL.Simple                  as My
import qualified Database.MySQL.Simple.Types            as My
import qualified Database.MySQL.Base                    as MyBase
import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai
import qualified System.IO.Streams.List                 as IOSL

-- DO NOT SUBMIT
import qualified Debug.Trace                            as UGLY


class Monad m => MonadExecuteQuery m where
  executeQuery
    :: GQLReqParsed
    -> [QueryRootField UnpreparedValue]
    -> Maybe EQ.GeneratedSqlMap
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


-- | Run (execute) a single GraphQL query
runGQ
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
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  -- The response and misc telemetry data:
  let telemTransport = Telem.HTTP
  (telemTimeTot_DT, (telemCacheHit, telemLocality, (telemTimeIO_DT, telemQueryType, !resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx {- planCache -} sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <- E.getResolvedExecPlan env logger pgExecCtx {- planCache -}
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    case execPlan of
      E.QueryExecutionPlan queryPlan asts ->
        case queryPlan of
          E.ExecStepPostgres txGenSql -> do
            (telemTimeIO, telemQueryType, respHdrs, resp) <-
              runQueryDB reqId (reqUnparsed,reqParsed) asts userInfo txGenSql
            return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp respHdrs))
          E.ExecStepMySQL queries -> liftIO $ do
            connection <- fromJust <$> readMVar mySQLConnection
            let
              handleFormat e = GQExecError [J.Object $ Map.singleton "message" $ J.String $ stringlyCoerce $ My.fmtMessage e]
              handleQuery e = GQExecError [J.Object $ Map.singleton "message" $ J.String $ stringlyCoerce $ My.qeMessage e]
              handleResult e = GQExecError [J.Object $ Map.singleton "message" $ J.String $ stringlyCoerce $ My.errMessage e]
              handleConnection e = GQExecError [J.Object $ Map.singleton "message" $ J.String $ stringlyCoerce $ MyBase.errMessage e]
            gqResult <- (flip catch (pure . handleFormat) . flip catch (pure . handleQuery) . flip catch (pure . handleResult) . flip catch (pure . handleConnection)) $ do
              assocResults <- for queries \(name, queryString) -> do
                UGLY.traceShowM queryString
                [My.Only result] <- My.query_ connection (My.Query (BS.toStrict queryString))
                UGLY.traceShowM result
                return (name, encJFromText result)
              pure $ GQSuccess $ encJToLBS $ encJFromAssocList assocResults
            -- TODO fill in proper values for telemTimeIO and telemQueryType below
            pure (telemCacheHit, Telem.Local, (secondsToDiffTime 0, Telem.Query, HttpResponse (encodeGQResp gqResult) []))
          E.ExecStepRemote (rsi, opDef, _varValsM) ->
            runRemoteGQ telemCacheHit rsi opDef
          E.ExecStepRaw (name, json) -> do
            (telemTimeIO, obj) <- withElapsedTime $
              return $ encJFromJValue $ J.Object $ Map.singleton (G.unName name) json
            return (telemCacheHit, Telem.Local, (telemTimeIO, Telem.Query, HttpResponse obj []))
      E.MutationExecutionPlan mutationPlan ->
        case mutationPlan of
          E.ExecStepPostgres (tx, responseHeaders) -> do
            (telemTimeIO, telemQueryType, resp) <- runMutationDB reqId reqUnparsed userInfo tx
            return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp responseHeaders))
          E.ExecStepMySQL _ -> error "Dolphin: not supported for now"
          E.ExecStepRemote (rsi, opDef, _varValsM) ->
            runRemoteGQ telemCacheHit rsi opDef
          E.ExecStepRaw (name, json) -> do
            (telemTimeIO, obj) <- withElapsedTime $
              return $ encJFromJValue $ J.Object $ Map.singleton (G.unName name) json
            return (telemCacheHit, Telem.Local, (telemTimeIO, Telem.Query, HttpResponse obj []))
      E.SubscriptionExecutionPlan _sub ->
        throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
{-
      E.GExPHasura resolvedOp -> do
        (telemTimeIO, telemQueryType, respHdrs, resp) <- runHasuraGQ reqId (reqUnparsed, reqParsed) userInfo resolvedOp
        return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp respHdrs))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                           | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHeaders reqUnparsed rsi $ G._todType opDef
        return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))
-}
  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp
  where
    runRemoteGQ telemCacheHit rsi opDef = do
      let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                         | otherwise = Telem.Query
      (telemTimeIO, resp) <- E.execRemoteGQ env reqId userInfo reqHeaders reqUnparsed rsi opDef
      return (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

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
  -> (Tracing.TraceT (LazyTx QErr) EncJSON, EQ.GeneratedSqlMap)
  -> m (DiffTime, Telem.QueryType, HTTP.ResponseHeaders, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runQueryDB reqId (query, queryParsed) asts _userInfo (tx, genSql) =  do
  -- log the generated SQL and the graphql query
  E.ExecutionCtx logger _ pgExecCtx _ _ _ _ <- ask
  logQueryLog logger query (Just genSql) reqId
  (telemTimeIO, respE) <- withElapsedTime $ runExceptT $ trace "pg" $
    Tracing.interpTraceT id $ executeQuery queryParsed asts (Just genSql) pgExecCtx Q.ReadOnly tx
  (respHdrs,resp) <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = Telem.Query
  return (telemTimeIO, telemQueryType, respHdrs, json)

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
  (telemTimeIO, respE) <- withElapsedTime $  runExceptT $ trace "pg" $
    Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadWrite . withTraceContext ctx .  withUserInfo userInfo)  tx
  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = Telem.Mutation
  return (telemTimeIO, telemQueryType, json)

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
