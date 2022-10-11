module Hasura.GraphQL.Transport.Backend
  ( BackendTransport (..),
  )
where

import Data.ByteString qualified as B
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Logging (MonadQueryLog)
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing

-- | This typeclass enacapsulates how a given backend sends queries and mutations over the
-- network. Each backend is currently responsible for both logging and tracing, for now.
class BackendExecute b => BackendTransport (b :: BackendType) where
  runDBQuery ::
    forall m.
    ( MonadIO m,
      MonadError QErr m,
      MonadQueryLog m,
      MonadTrace m
    ) =>
    RequestId ->
    GQLReqUnparsed ->
    RootFieldAlias ->
    UserInfo ->
    L.Logger L.Hasura ->
    SourceConfig b ->
    ExecutionMonad b EncJSON ->
    Maybe (PreparedQuery b) ->
    m (DiffTime, EncJSON)
  runDBMutation ::
    forall m.
    ( MonadIO m,
      MonadError QErr m,
      MonadQueryLog m,
      MonadTrace m
    ) =>
    RequestId ->
    GQLReqUnparsed ->
    RootFieldAlias ->
    UserInfo ->
    L.Logger L.Hasura ->
    SourceConfig b ->
    ExecutionMonad b EncJSON ->
    Maybe (PreparedQuery b) ->
    m (DiffTime, EncJSON)
  runDBSubscription ::
    forall m.
    MonadIO m =>
    SourceConfig b ->
    MultiplexedQuery b ->
    -- | WARNING: Postgres-specific, ignored by other backends
    [(CohortId, CohortVariables)] ->
    m (DiffTime, Either QErr [(CohortId, B.ByteString)])
  runDBStreamingSubscription ::
    forall m.
    MonadIO m =>
    SourceConfig b ->
    MultiplexedQuery b ->
    -- | WARNING: Postgres-specific, ignored by other backends
    [(CohortId, CohortVariables)] ->
    m (DiffTime, Either QErr [(CohortId, B.ByteString, CursorVariableValues)])
  runDBQueryExplain ::
    forall m.
    ( MonadIO m,
      MonadError QErr m
    ) =>
    DBStepInfo b ->
    m EncJSON
