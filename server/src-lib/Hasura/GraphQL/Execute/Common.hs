module Hasura.GraphQL.Execute.Common
  ( MonadGQLExecutionCheck (..),
  )
where

import Data.Aeson.Ordered qualified as JO
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Prelude
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.RQL.Types.SchemaCache
import Hasura.Server.Init (AllowListStatus)
import Hasura.Server.Types (RequestId)
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Network.HTTP.Types qualified as HTTP
import Network.Wai.Extended qualified as Wai

-- | Typeclass representing safety checks (if any) that need to be performed
-- before a GraphQL query should be allowed to be executed. In OSS, the safety
-- check is to check in the query is in the allow list.
--
-- the `executeIntrospection` function has different implementations in OSS and
-- Pro. In Pro, the GraphQL schema introspection can be disabled for specified
-- roles and in OSS there is no restrictions.
--
-- | TODO (from master): Limitation: This parses the query, which is not ideal if we already
-- have the query cached. The parsing happens unnecessary. But getting this to
-- either return a plan or parse was tricky and complicated.
class (Monad m) => MonadGQLExecutionCheck m where
  checkGQLExecution ::
    UserInfo ->
    ([HTTP.Header], Wai.IpAddress) ->
    -- | allow list enabled?
    AllowListStatus ->
    -- | needs allow list
    SchemaCache ->
    -- | the unparsed GraphQL query string (and related values)
    GQLReqUnparsed ->
    RequestId ->
    m (Either QErr GQLReqParsed)

  executeIntrospection ::
    UserInfo ->
    JO.Value ->
    SetGraphqlIntrospectionOptions ->
    m (Either QErr ExecutionStep)

  checkGQLBatchedReqs ::
    UserInfo ->
    RequestId ->
    [GQLReq GQLQueryText] ->
    SchemaCache ->
    m (Either QErr ())

instance (MonadGQLExecutionCheck m) => MonadGQLExecutionCheck (ReaderT r m) where
  checkGQLExecution ui det enableAL sc req requestId =
    lift $ checkGQLExecution ui det enableAL sc req requestId

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

  checkGQLBatchedReqs userInfo requestId reqs sc =
    lift $ checkGQLBatchedReqs userInfo requestId reqs sc

instance (MonadGQLExecutionCheck m) => MonadGQLExecutionCheck (ExceptT e m) where
  checkGQLExecution ui det enableAL sc req requestId =
    lift $ checkGQLExecution ui det enableAL sc req requestId

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

  checkGQLBatchedReqs userInfo requestId reqs sc =
    lift $ checkGQLBatchedReqs userInfo requestId reqs sc

instance (MonadGQLExecutionCheck m) => MonadGQLExecutionCheck (Tracing.TraceT m) where
  checkGQLExecution ui det enableAL sc req requestId =
    lift $ checkGQLExecution ui det enableAL sc req requestId

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

  checkGQLBatchedReqs userInfo requestId reqs sc =
    lift $ checkGQLBatchedReqs userInfo requestId reqs sc
