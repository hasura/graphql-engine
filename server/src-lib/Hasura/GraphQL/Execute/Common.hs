module Hasura.GraphQL.Execute.Common
  where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                     as JO
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

import qualified Hasura.Tracing                         as Tracing

import           Hasura.Base.Error
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Session

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
class Monad m => MonadGQLExecutionCheck m where
  checkGQLExecution
    :: UserInfo
    -> ([HTTP.Header], Wai.IpAddress)
    -> Bool
    -- ^ allow list enabled?
    -> SchemaCache
    -- ^ needs allow list
    -> GQLReqUnparsed
    -- ^ the unparsed GraphQL query string (and related values)
    -> m (Either QErr GQLReqParsed)

  executeIntrospection
    :: UserInfo
    -> JO.Value
    -> SetGraphqlIntrospectionOptions
    -> m (Either QErr ExecutionStep)

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (ExceptT e m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (ReaderT r m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (Tracing.TraceT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (MetadataStorageT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

  executeIntrospection userInfo introspectionQuery rolesDisabled =
    lift $ executeIntrospection userInfo introspectionQuery rolesDisabled

