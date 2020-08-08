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

import           Control.Monad.Morph                    (hoist)

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                 (MonadQueryLog (..))
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                         (MonadTrace, TraceT, trace)

import qualified Data.Environment                       as Env
import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai


class Monad m => MonadExecuteQuery m where
  executeQuery
    :: GQLReqParsed
    -> [R.QueryRootFldUnresolved]
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
    E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer httpManager enableAL <- ask

    -- run system authorization on the GraphQL API
    reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
                 >>= flip onLeft throwError

    (telemCacheHit, execPlan) <- E.getResolvedExecPlan env logger pgExecCtx planCache
                                 userInfo sqlGenCtx sc scVer queryType
                                 httpManager reqHeaders (reqUnparsed, reqParsed)
    case execPlan of
      E.GExPHasura resolvedOp -> do
        (telemTimeIO, telemQueryType, respHdrs, resp) <- runHasuraGQ reqId (reqUnparsed, reqParsed) userInfo resolvedOp
        return (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp respHdrs))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                           | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ env reqId userInfo reqHeaders reqUnparsed rsi $ G._todType opDef
        pure (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

  let telemTimeIO = convertDuration telemTimeIO_DT
      telemTimeTot = convertDuration telemTimeTot_DT

  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp

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
runGQBatched env logger reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query = do
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
    E.ExOpQuery tx genSql asts -> trace "pg" $ do
      -- log the generated SQL and the graphql query
      logQueryLog logger query genSql reqId
      Tracing.interpTraceT id $ executeQuery queryParsed asts genSql pgExecCtx Q.ReadOnly tx

    E.ExOpMutation respHeaders tx -> trace "pg" $ do
      logQueryLog logger query Nothing reqId
      ctx <- Tracing.currentContext
      (respHeaders,) <$> Tracing.interpTraceT (runLazyTx pgExecCtx Q.ReadWrite . withTraceContext ctx . withUserInfo userInfo) tx

    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"

  (respHdrs, resp) <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = case resolvedOp of E.ExOpMutation{} -> Telem.Mutation ; _ -> Telem.Query
  return (telemTimeIO, telemQueryType, respHdrs, json)
