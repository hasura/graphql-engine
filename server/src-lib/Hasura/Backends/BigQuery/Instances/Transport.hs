{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Transport () where

import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Hasura.Backends.BigQuery.Instances.Execute ()
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Logging
  ( ExecutionStats (..),
    GeneratedQuery (..),
    MonadQueryLog (..),
    QueryLog (..),
    QueryLogKind (QueryLogKindDatabase, QueryLogKindDatabaseResponse),
  )
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.SQL.Backend
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

instance BackendTransport 'BigQuery where
  runDBQuery = runQuery
  runDBQueryExplain = runQueryExplain
  runDBMutation = runMutation
  runDBSubscription = error "Not supported."
  runDBStreamingSubscription = error "Not supported"

runQuery ::
  ( MonadIO m,
    MonadBaseControl IO m,
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
  OnBaseMonad IdentityT (Maybe (AnyBackend ExecutionStats), EncJSON) ->
  Maybe Text ->
  ResolvedConnectionTemplate 'BigQuery ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _sourceConfig tx genSql _ = do
  -- log the generated SQL and the graphql query
  -- FIXME: fix logging by making logQueryLog expect something backend agnostic!
  logQueryLog logger $ mkQueryLog (QueryLogKindDatabase Nothing) query fieldName genSql reqId Nothing
  (diffTime, (stats, result)) <- withElapsedTime $ run tx

  logQueryLog logger $ mkQueryLog (QueryLogKindDatabaseResponse Nothing) query fieldName Nothing reqId stats

  pure (diffTime, result)

runQueryExplain ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  DBStepInfo 'BigQuery ->
  m EncJSON
runQueryExplain (DBStepInfo _ _ _ action _) = fmap arResult (run action)

runMutation ::
  ( MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig 'BigQuery ->
  OnBaseMonad IdentityT EncJSON ->
  Maybe Text ->
  ResolvedConnectionTemplate 'BigQuery ->
  -- | Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runMutation _reqId _query _fieldName _userInfo _logger _sourceConfig _tx _genSql _ =
  -- do
  throw500 "BigQuery does not support mutations!"

run ::
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadTrace m
  ) =>
  OnBaseMonad IdentityT a ->
  m a
run = runIdentityT . runOnBaseMonad

-- @QueryLogKindDatabase Nothing@ means that the backend doesn't support connection templates
mkQueryLog ::
  QueryLogKind ->
  GQLReqUnparsed ->
  RootFieldAlias ->
  Maybe Text ->
  RequestId ->
  Maybe (AnyBackend ExecutionStats) ->
  QueryLog
mkQueryLog _qlKind _qlQuery fieldName preparedSql _qlRequestId _qlStatistics = QueryLog {..}
  where
    _qlGeneratedSql = preparedSql <&> \query -> (fieldName, GeneratedQuery query J.Null)
