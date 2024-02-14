module Hasura.GraphQL.Transport.Backend
  ( BackendTransport (..),
  )
where

import Control.Monad.Trans.Control
import Data.ByteString qualified as B
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Logging (ExecutionStats, MonadExecutionLog, MonadQueryLog)
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.SQL.AnyBackend (AnyBackend)
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

-- | This typeclass enacapsulates how a given backend sends queries and mutations over the
-- network. Each backend is currently responsible for both logging and tracing, for now.
class (BackendExecute b) => BackendTransport (b :: BackendType) where
  runDBQuery ::
    forall m.
    ( MonadIO m,
      MonadBaseControl IO m,
      MonadError QErr m,
      MonadQueryLog m,
      MonadExecutionLog m,
      MonadTrace m
    ) =>
    RequestId ->
    GQLReqUnparsed ->
    RootFieldAlias ->
    UserInfo ->
    L.Logger L.Hasura ->
    Maybe (CredentialCache AgentLicenseKey) ->
    SourceConfig b ->
    OnBaseMonad (ExecutionMonad b) (Maybe (AnyBackend ExecutionStats), EncJSON) ->
    Maybe (PreparedQuery b) ->
    ResolvedConnectionTemplate b ->
    m (DiffTime, EncJSON)
  runDBMutation ::
    forall m.
    ( MonadIO m,
      MonadBaseControl IO m,
      MonadError QErr m,
      MonadQueryLog m,
      MonadTrace m
    ) =>
    RequestId ->
    GQLReqUnparsed ->
    RootFieldAlias ->
    UserInfo ->
    L.Logger L.Hasura ->
    Maybe (CredentialCache AgentLicenseKey) ->
    SourceConfig b ->
    OnBaseMonad (ExecutionMonad b) EncJSON ->
    Maybe (PreparedQuery b) ->
    ResolvedConnectionTemplate b ->
    m (DiffTime, EncJSON)
  runDBSubscription ::
    forall m.
    (MonadIO m, MonadBaseControl IO m) =>
    SourceConfig b ->
    MultiplexedQuery b ->
    -- | WARNING: Postgres-specific, ignored by other backends
    [(CohortId, CohortVariables)] ->
    ResolvedConnectionTemplate b ->
    m (DiffTime, Either QErr [(CohortId, B.ByteString)])
  runDBStreamingSubscription ::
    forall m.
    (MonadIO m, MonadBaseControl IO m) =>
    SourceConfig b ->
    MultiplexedQuery b ->
    -- | WARNING: Postgres-specific, ignored by other backends
    [(CohortId, CohortVariables)] ->
    ResolvedConnectionTemplate b ->
    m (DiffTime, Either QErr [(CohortId, B.ByteString, CursorVariableValues)])
  runDBQueryExplain ::
    forall m.
    ( MonadIO m,
      MonadError QErr m,
      MonadBaseControl IO m,
      MonadTrace m
    ) =>
    Maybe (CredentialCache AgentLicenseKey) ->
    DBStepInfo b ->
    m EncJSON
