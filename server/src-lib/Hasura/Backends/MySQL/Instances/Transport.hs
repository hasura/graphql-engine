{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.MySQL.Instances.Transport (runQuery) where

import Data.Aeson qualified as J
import Data.Text.Extended
import Hasura.Backends.MySQL.Instances.Execute ()
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend (PreparedQuery)
import Hasura.GraphQL.Logging
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing
import Language.GraphQL.Draft.Syntax qualified as G

instance BackendTransport 'MySQL where
  runDBQuery = runQuery
  runDBQueryExplain = error "runDBQueryExplain: MySQL backend does not support this operation yet."
  runDBMutation = error "runDBMutation: MySQL backend does not support this operation yet."
  runDBSubscription = error "runDBSubscription: MySQL backend does not support this operation yet."

runQuery ::
  ( MonadIO m,
    MonadQueryLog m,
    MonadTrace m,
    MonadError QErr m
  ) =>
  RequestId ->
  GQLReqUnparsed ->
  G.Name ->
  UserInfo ->
  L.Logger L.Hasura ->
  SourceConfig 'MySQL ->
  ExceptT QErr IO EncJSON ->
  Maybe (PreparedQuery 'MySQL) ->
  -- | Also return the time spent in the PG query; for telemetry.
  m (DiffTime, EncJSON)
runQuery reqId query fieldName _userInfo logger _sourceConfig tx genSql = do
  logQueryLog logger $ mkQueryLog query fieldName genSql reqId
  withElapsedTime $
    trace ("MySQL Query for root field " <>> fieldName) $
      run tx

run :: (MonadIO m, MonadError QErr m) => ExceptT QErr IO a -> m a
run action = do
  result <- liftIO $ runExceptT action
  result `onLeft` throwError

mkQueryLog ::
  GQLReqUnparsed ->
  G.Name ->
  Maybe (PreparedQuery 'MySQL) ->
  RequestId ->
  QueryLog
mkQueryLog gqlQuery fieldName preparedSql requestId =
  QueryLog gqlQuery ((fieldName,) <$> generatedQuery) requestId QueryLogKindDatabase
  where
    generatedQuery =
      preparedSql <&> \queryString ->
        GeneratedQuery queryString J.Null
