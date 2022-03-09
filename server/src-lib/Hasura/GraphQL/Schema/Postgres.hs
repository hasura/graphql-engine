-- | Postgres-specific schema combinators. Those should be moved to
-- the corresponding instance of `BackendSchema`, when actions are
-- generalized.
module Hasura.GraphQL.Schema.Postgres
  ( buildActionQueryFields,
    buildActionSubscriptionFields,
    buildActionMutationFields,
  )
where

import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Schema.Action
import Hasura.GraphQL.Schema.Backend (MonadBuildSchema)
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types

buildActionQueryFields ::
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  m [FieldParser n (QueryRootField UnpreparedValue)]
buildActionQueryFields customTypes actionInfo =
  maybeToList <$> case _adType (_aiDefinition actionInfo) of
    ActionQuery ->
      fmap (fmap (RFAction . AQQuery)) <$> actionExecute customTypes actionInfo
    ActionMutation ActionSynchronous -> pure Nothing
    ActionMutation ActionAsynchronous ->
      fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery (_actObjects customTypes) actionInfo

buildActionMutationFields ::
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  m [FieldParser n (MutationRootField UnpreparedValue)]
buildActionMutationFields customTypes actionInfo =
  maybeToList <$> case _adType (_aiDefinition actionInfo) of
    ActionQuery -> pure Nothing
    ActionMutation ActionSynchronous ->
      fmap (fmap (RFAction . AMSync)) <$> actionExecute customTypes actionInfo
    ActionMutation ActionAsynchronous ->
      fmap (fmap (RFAction . AMAsync)) <$> actionAsyncMutation (_actNonObjects customTypes) actionInfo

buildActionSubscriptionFields ::
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  m [FieldParser n (QueryRootField UnpreparedValue)]
buildActionSubscriptionFields customTypes actionInfo =
  maybeToList <$> case _adType (_aiDefinition actionInfo) of
    ActionQuery -> pure Nothing
    ActionMutation ActionSynchronous -> pure Nothing
    ActionMutation ActionAsynchronous ->
      fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery (_actObjects customTypes) actionInfo
