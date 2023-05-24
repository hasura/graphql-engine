-- | Postgres-specific schema combinators. Those should be moved to
-- the corresponding instance of `BackendSchema`, when actions are
-- generalized.
module Hasura.GraphQL.Schema.Postgres
  ( buildActionQueryFields,
    buildActionSubscriptionFields,
    buildActionMutationFields,
  )
where

import Hasura.GraphQL.Schema.Action
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Metadata.Object

buildActionQueryFields ::
  (MonadBuildActionSchema r m n) =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  SchemaT r m [FieldParser n (QueryRootField UnpreparedValue)]
buildActionQueryFields customTypes actionInfo =
  maybeToList
    . applyActionOrigin actionInfo
    <$> case _adType (_aiDefinition actionInfo) of
      ActionQuery ->
        fmap (fmap (RFAction . AQQuery)) <$> actionExecute customTypes actionInfo
      ActionMutation ActionSynchronous -> pure Nothing
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery (_actObjectTypes customTypes) actionInfo

buildActionMutationFields ::
  (MonadBuildActionSchema r m n) =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  SchemaT r m [FieldParser n (MutationRootField UnpreparedValue)]
buildActionMutationFields customTypes actionInfo =
  maybeToList
    . applyActionOrigin actionInfo
    <$> case _adType (_aiDefinition actionInfo) of
      ActionQuery -> pure Nothing
      ActionMutation ActionSynchronous ->
        fmap (fmap (RFAction . AMSync)) <$> actionExecute customTypes actionInfo
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AMAsync)) <$> actionAsyncMutation (_actInputTypes customTypes) actionInfo

buildActionSubscriptionFields ::
  (MonadBuildActionSchema r m n) =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  SchemaT r m [FieldParser n (QueryRootField UnpreparedValue)]
buildActionSubscriptionFields customTypes actionInfo =
  maybeToList
    . applyActionOrigin actionInfo
    <$> case _adType (_aiDefinition actionInfo) of
      ActionQuery -> pure Nothing
      ActionMutation ActionSynchronous -> pure Nothing
      ActionMutation ActionAsynchronous ->
        fmap (fmap (RFAction . AQAsync)) <$> actionAsyncQuery (_actObjectTypes customTypes) actionInfo

applyActionOrigin ::
  ActionInfo ->
  Maybe (FieldParser n a) ->
  Maybe (FieldParser n a)
applyActionOrigin actionInfo = fmap (setFieldParserOrigin (MOAction (_aiName actionInfo)))
