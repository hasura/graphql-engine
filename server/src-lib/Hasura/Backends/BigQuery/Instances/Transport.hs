{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Transport () where

import Data.Aeson qualified as J
import Hasura.Backends.BigQuery.Instances.Execute ()
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Logging
  ( GeneratedQuery (..),
    MonadQueryLog (..),
    QueryLog (..),
    QueryLogKind (QueryLogKindDatabase),
  )
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing
import Hasura.Tracing qualified as Tracing

instance BackendTransport 'BigQuery where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = runMutation
  runDBSubscription = error "Not supported."
  runDBStreamingSubscription = error "Not supported"

runQuery ::
  ( MonadIO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig 'BigQuery ->
  Tracing.TraceT (ExceptT QErr IO) EncJSON ->
  Maybe Text ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _sourceConfig tx genSql = do
  -- log the generated SQL and the graphql query
  -- FIXME: fix logging by making logQueryLog expect something backend agnostic!
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $ Tracing.interpTraceT run tx

runQueryExplain ::
  ( MonadIO m,
    MonadError QErr m
  ) =>
  DBStepInfo 'BigQuery ->
  m EncJSON
runQueryExplain (DBStepInfo _ _ _ action) = run $ runTraceTWithReporter noReporter "explain" action

runMutation ::
  ( MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig 'BigQuery ->
  Tracing.TraceT (ExceptT QErr IO) EncJSON ->
  Maybe Text ->
  -- | Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runMutation _reqId _query _fieldName _userInfo _logger _sourceConfig _tx _genSql =
  -- do
  throw500 "BigQuery does not support mutations!"

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError

mkQueryLog ::
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe Text ->
  RequestId ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId QueryLogKindDatabase
  where
    generatedQuery = preparedSql <&> \qs -> GeneratedQuery qs J.Null
