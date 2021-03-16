{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Transport () where

import           Hasura.Prelude

import qualified Data.Aeson                                 as J
import qualified Data.ByteString                            as B
import qualified Database.PG.Query                          as Q
import qualified Language.GraphQL.Draft.Syntax              as G

import           Control.Monad.Morph                        (hoist)
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.Execute.LiveQuery as PGL
import qualified Hasura.GraphQL.Execute.Query               as EQ
import qualified Hasura.Logging                             as L
import qualified Hasura.Tracing                             as Tracing

import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types
import           Hasura.Server.Types                        (RequestId)
import           Hasura.Session
import           Hasura.Tracing


instance BackendTransport 'Postgres where
  runDBQuery = runPGQuery
  runDBMutation = runPGMutation
  runDBSubscription = runPGSubscription

runPGQuery
  :: ( MonadIO m
     , MonadError QErr m
     , MonadQueryLog m
     , MonadTrace m
     )
  => RequestId
  -> GQLReqUnparsed
  -> G.Name
  -> UserInfo
  -> L.Logger L.Hasura
  -> SourceConfig 'Postgres
  -> Tracing.TraceT (LazyTxT QErr IO) EncJSON
  -> Maybe EQ.PreparedSql
  -> m (DiffTime, EncJSON)
  -- ^ Also return the time spent in the PG query; for telemetry.
runPGQuery reqId query fieldName _userInfo logger sourceConfig tx genSql =  do
  -- log the generated SQL and the graphql query
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $ trace ("Postgres Query for root field " <>> fieldName) $
    Tracing.interpTraceT id $ hoist (runQueryTx $ _pscExecCtx sourceConfig) tx

runPGMutation
  :: ( MonadIO m
     , MonadError QErr m
     , MonadQueryLog m
     , MonadTrace m
     )
  => RequestId
  -> GQLReqUnparsed
  -> G.Name
  -> UserInfo
  -> L.Logger L.Hasura
  -> SourceConfig 'Postgres
  -> Tracing.TraceT (LazyTxT QErr IO) EncJSON
  -> Maybe EQ.PreparedSql
  -> m (DiffTime, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runPGMutation reqId query fieldName userInfo logger sourceConfig tx _genSql =  do
  -- log the graphql query
  logQueryLog logger $ mkQueryLog query fieldName Nothing reqId
  ctx <- Tracing.currentContext
  withElapsedTime $ trace ("Postgres Mutation for root field " <>> fieldName) $
    Tracing.interpTraceT (
      liftEitherM . liftIO . runExceptT
      . runLazyTx (_pscExecCtx sourceConfig) Q.ReadWrite
      . withTraceContext ctx
      . withUserInfo userInfo
      )  tx

runPGSubscription
  :: ( MonadIO m
     )
  => SourceConfig 'Postgres
  -> MultiplexedQuery 'Postgres
  -> [(CohortId, CohortVariables)]
  -> m (DiffTime, Either QErr [(CohortId, B.ByteString)])
runPGSubscription sourceConfig query variables = withElapsedTime
  $ runExceptT
  $ runQueryTx (_pscExecCtx sourceConfig)
  $ PGL.executeMultiplexedQuery query variables


mkQueryLog
  :: GQLReqUnparsed
  -> G.Name
  -> Maybe EQ.PreparedSql
  -> RequestId
  -> QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId
  where
    generatedQuery = preparedSql <&> \(EQ.PreparedSql query args _) ->
      GeneratedQuery (Q.getQueryText query) (J.toJSON $ pgScalarValueToJson . snd <$> args)
