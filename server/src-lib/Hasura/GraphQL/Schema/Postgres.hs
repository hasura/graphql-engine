-- | Postgres-specific schema combinators. Those should be moved to
-- the corresponding instance of `BackendSchema`, when actions are
-- generalized.
module Hasura.GraphQL.Schema.Postgres
  ( buildActionQueryFields
  , buildActionSubscriptionFields
  , buildActionMutationFields
  ) where

import           Hasura.Prelude

import           Hasura.GraphQL.Parser         hiding (EnumValueInfo, field)
import           Hasura.GraphQL.Schema.Action
import           Hasura.GraphQL.Schema.Backend (MonadBuildSchema)
import           Hasura.RQL.IR
import           Hasura.RQL.Types


buildActionQueryFields
  :: MonadBuildSchema ('Postgres 'Vanilla) r m n
  => NonObjectTypeMap
  -> ActionInfo
  -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
buildActionQueryFields nonObjectCustomTypes actionInfo =
  maybeToList <$> case _adType (_aiDefinition actionInfo) of
    ActionQuery                       ->
      fmap (fmap (RFAction . AQQuery)) <$> actionExecute nonObjectCustomTypes actionInfo
    ActionMutation ActionSynchronous  -> pure Nothing
    ActionMutation ActionAsynchronous ->
      fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery actionInfo

buildActionMutationFields
  :: MonadBuildSchema ('Postgres 'Vanilla) r m n
  => NonObjectTypeMap
  -> ActionInfo
  -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
buildActionMutationFields nonObjectCustomTypes actionInfo =
  maybeToList <$> case _adType (_aiDefinition actionInfo) of
    ActionQuery -> pure Nothing
    ActionMutation ActionSynchronous ->
      fmap (fmap (RFAction . AMSync)) <$> actionExecute nonObjectCustomTypes actionInfo
    ActionMutation ActionAsynchronous ->
      fmap (fmap (RFAction . AMAsync)) <$> actionAsyncMutation nonObjectCustomTypes actionInfo

buildActionSubscriptionFields
  :: MonadBuildSchema ('Postgres 'Vanilla) r m n
  => ActionInfo
  -> m [FieldParser n (QueryRootField UnpreparedValue
                       UnpreparedValue)]
buildActionSubscriptionFields actionInfo =
  maybeToList <$> case _adType (_aiDefinition actionInfo) of
    ActionQuery                       -> pure Nothing
    ActionMutation ActionSynchronous  -> pure Nothing
    ActionMutation ActionAsynchronous ->
      fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery actionInfo
