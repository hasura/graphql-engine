-- | Execution of GraphQL queries over HTTP transport

module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  -- , buildRaw
  -- * imported from HTTP.Protocol; required by pro
  , GQLReq(..)
  , GQLReqUnparsed
  , GQLReqParsed
  , GQLExecDoc(..)
  , OperationName(..)
  , GQLQueryText(..)

  ) where

import           Hasura.Prelude

import qualified Data.Environment                           as Env
import qualified Network.HTTP.Types                         as HTTP
import qualified Network.Wai.Extended                       as Wai

import           Control.Monad.Trans.Control                (MonadBaseControl)

import qualified Hasura.GraphQL.Execute                     as E
import qualified Hasura.GraphQL.Execute.Mutation            as EM
import qualified Hasura.GraphQL.Execute.Query               as EQ
import qualified Hasura.Logging                             as L

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging                     (MonadQueryLog (logQueryLog),
                                                             QueryLog (..), QueryLogKind (..))
import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Init.Config
import           Hasura.Server.Logging
import           Hasura.Server.Types                        (RequestId)
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                             (MonadTrace, trace)

-- | Run (execute) a single GraphQL query
runGQ
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , EQ.MonadExecuteQuery m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> RequestId
  -> UserInfo
  -> Wai.IpAddress
  -> [HTTP.Header]
  -> E.GraphQLQueryType
  -> GQLReqUnparsed
  -> m (ParameterizedQueryHash, HttpResponse EncJSON)
runGQ env logger reqId userInfo ipAddress reqHeaders queryType reqUnparsed = do
  E.ExecutionCtx _ sqlGenCtx@SQLGenCtx{stringifyNum} sc _scVer httpManager enableAL <- ask
  let introspectionOptions = scSetGraphqlIntrospectionOptions sc

  -- run system authorization on the GraphQL API
  reqParsed <- E.checkGQLExecution userInfo (reqHeaders, ipAddress) enableAL sc reqUnparsed
               >>= flip onLeft throwError

  (normalizedSelectionSet, execPlan) <-
    E.getResolvedExecPlan userInfo sqlGenCtx sc queryType (reqUnparsed, reqParsed)

  (response, responseHeaders) <- case execPlan of
    E.ResolvedPlanQuery queryPlans asts dirMap -> trace "Query" $ do
      EQ.runExecutionPlan env logger userInfo httpManager reqHeaders reqId introspectionOptions reqParsed queryPlans asts dirMap

    E.ResolvedPlanMutation plan -> trace "Mutation" $ do
      EM.runExecutionPlan env logger userInfo httpManager reqHeaders reqId stringifyNum plan

    E.ResolvedPlanSubscription _plan ->
      throw400 UnexpectedPayload "subscriptions are not supported over HTTP, use websockets instead"
  let gqlResponse = encodeGQResp $ Right $ encJToLBS response
  return (normalizedSelectionSet, HttpResponse gqlResponse responseHeaders)

-- | Run (execute) a batched GraphQL query (see 'GQLBatchedReqs').
runGQBatched
  :: forall m
   . ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     , E.MonadGQLExecutionCheck m
     , MonadQueryLog m
     , MonadTrace m
     , EQ.MonadExecuteQuery m
     , HttpLog m
     , MonadMetadataStorage (MetadataStorageT m)
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
  -> m (HTTPLoggingMetadata m, HttpResponse EncJSON)
runGQBatched env logger reqId responseErrorsConfig userInfo ipAddress reqHdrs queryType query =
  case query of
    GQLSingleRequest req -> do
      (parameterizedQueryHash, httpResp) <- runGQ env logger reqId userInfo ipAddress reqHdrs queryType req
      let httpLoggingMetadata = buildHTTPLoggingMetadata @m [parameterizedQueryHash]
      pure (httpLoggingMetadata, httpResp)
    GQLBatchedReqs reqs -> do
      -- It's unclear what we should do if we receive multiple responses with
      -- distinct headers, so just do the simplest thing in this case, and
      -- don't forward any.
      let includeInternal = shouldIncludeInternal (_uiRole userInfo) responseErrorsConfig
          removeHeaders =
            flip HttpResponse []
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr includeInternal) _hrBody)
      responses <- traverse (try . runGQ env logger reqId userInfo ipAddress reqHdrs queryType) reqs
      let httpLoggingMetadata = buildHTTPLoggingMetadata @m $ rights $ map (fmap fst) responses
      pure (httpLoggingMetadata, removeHeaders (map (fmap snd) responses))
  where
    try = flip catchError (pure . Left) . fmap Right
