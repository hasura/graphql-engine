module Hasura.GraphQL.Execute
  ( EB.ExecutionStep (..),
    ResolvedExecutionPlan (..),
    ET.GraphQLQueryType (..),
    getResolvedExecPlan,
    getExecPlanPartial,
    execRemoteGQ,
    SubscriptionExecution (..),
    buildSubscriptionPlan,
    ExecutionCtx (..),
    EC.MonadGQLExecutionCheck (..),
    checkQueryInAllowlist,
    MultiplexedSubscriptionQueryPlan (..),
    SubscriptionQueryPlan (..),
    SourceSubscription (..),
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Containers.ListUtils (nubOrd)
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as HS
import Data.Tagged qualified as Tagged
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Context qualified as C
import Hasura.GraphQL.Execute.Action qualified as EA
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.Common qualified as EC
import Hasura.GraphQL.Execute.Mutation qualified as EM
import Hasura.GraphQL.Execute.Query qualified as EQ
import Hasura.GraphQL.Execute.RemoteJoin qualified as RJ
import Hasura.GraphQL.Execute.Resolve qualified as ER
import Hasura.GraphQL.Execute.Subscription.Plan qualified as ES
import Hasura.GraphQL.Execute.Types qualified as ET
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Parser.Monad
import Hasura.GraphQL.RemoteServer (execRemoteGQ)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Subscription
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Types (ReadOnlyMode (..), RequestId (..))
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP

-- | Execution context
data ExecutionCtx = ExecutionCtx
  { _ecxLogger :: L.Logger L.Hasura,
    _ecxSqlGenCtx :: SQLGenCtx,
    _ecxSchemaCache :: SchemaCache,
    _ecxSchemaCacheVer :: SchemaCacheVer,
    _ecxHttpManager :: HTTP.Manager,
    _ecxEnableAllowList :: Bool,
    _ecxReadOnlyMode :: ReadOnlyMode
  }

-- | Construct a single step of an execution plan.
getExecPlanPartial ::
  (MonadError QErr m) =>
  UserInfo ->
  SchemaCache ->
  ET.GraphQLQueryType ->
  GQLReqParsed ->
  m (C.GQLContext, SingleOperation)
getExecPlanPartial userInfo sc queryType req =
  (getGCtx,) <$> getSingleOperation req
  where
    role = _uiRole userInfo

    contextMap =
      case queryType of
        ET.QueryHasura -> scGQLContext sc
        ET.QueryRelay -> scRelayContext sc

    defaultContext =
      case queryType of
        ET.QueryHasura -> scUnauthenticatedGQLContext sc
        ET.QueryRelay -> scUnauthenticatedRelayContext sc

    getGCtx :: C.GQLContext
    getGCtx =
      case Map.lookup role contextMap of
        Nothing -> defaultContext
        Just (C.RoleContext frontend backend) ->
          case _uiBackendOnlyFieldAccess userInfo of
            BOFAAllowed -> fromMaybe frontend backend
            BOFADisallowed -> frontend

-- The graphql query is resolved into a sequence of execution operations
data ResolvedExecutionPlan
  = -- | query execution; remote schemas and introspection possible
    QueryExecutionPlan EB.ExecutionPlan [IR.QueryRootField IR.UnpreparedValue] DirectiveMap
  | -- | mutation execution; only __typename introspection supported
    MutationExecutionPlan EB.ExecutionPlan
  | -- | either action query or live query execution; remote schemas and introspection not supported
    SubscriptionExecutionPlan SubscriptionExecution

newtype MultiplexedSubscriptionQueryPlan (b :: BackendType)
  = MultiplexedSubscriptionQueryPlan (ES.SubscriptionQueryPlan b (EB.MultiplexedQuery b))

newtype SubscriptionQueryPlan = SubscriptionQueryPlan (AB.AnyBackend MultiplexedSubscriptionQueryPlan)

data SourceSubscription
  = SSLivequery !(HashSet ActionId) !(ActionLogResponseMap -> ExceptT QErr IO (SourceName, SubscriptionQueryPlan))
  | SSStreaming !RootFieldAlias !(SourceName, SubscriptionQueryPlan)

-- | The comprehensive subscription plan. We only support either
-- 1. Fields with only async action queries with no associated relationships
--    or
-- 2. Source database query fields from same source and also can be mixed with async
--    action query fields whose relationships are defined to tables in the source
data SubscriptionExecution
  = SEAsyncActionsWithNoRelationships !(RootFieldMap (ActionId, ActionLogResponse -> Either QErr EncJSON))
  | SEOnSourceDB !SourceSubscription

buildSubscriptionPlan ::
  forall m.
  (MonadError QErr m, EB.MonadQueryTags m, MonadIO m, MonadBaseControl IO m) =>
  UserInfo ->
  RootFieldMap (IR.QueryRootField IR.UnpreparedValue) ->
  ParameterizedQueryHash ->
  m SubscriptionExecution
buildSubscriptionPlan userInfo rootFields parameterizedQueryHash = do
  ((liveQueryOnSourceFields, noRelationActionFields), streamingFields) <- foldlM go ((mempty, mempty), mempty) (OMap.toList rootFields)

  if
      | null liveQueryOnSourceFields && null streamingFields ->
        pure $ SEAsyncActionsWithNoRelationships noRelationActionFields
      | null noRelationActionFields -> do
        if
            | null liveQueryOnSourceFields -> do
              case OMap.toList streamingFields of
                [] -> throw500 "empty selset for subscription"
                [(rootFieldName, (sourceName, exists))] -> do
                  subscriptionPlan <- AB.dispatchAnyBackend @EB.BackendExecute
                    exists
                    \(IR.SourceConfigWith sourceConfig queryTagsConfig (IR.QDBR qdb) :: IR.SourceConfigWith db b) -> do
                      let subscriptionQueryTagsAttributes = encodeQueryTags $ QTLiveQuery $ LivequeryMetadata rootFieldName parameterizedQueryHash
                          queryTagsComment = Tagged.untag $ EB.createQueryTags @m subscriptionQueryTagsAttributes queryTagsConfig
                      SubscriptionQueryPlan . AB.mkAnyBackend . MultiplexedSubscriptionQueryPlan
                        <$> runReaderT
                          ( EB.mkDBStreamingSubscriptionPlan
                              userInfo
                              sourceName
                              sourceConfig
                              (rootFieldName, qdb)
                          )
                          queryTagsComment
                  pure $
                    SEOnSourceDB $
                      SSStreaming rootFieldName $ (sourceName, subscriptionPlan)
                _ -> throw400 NotSupported "exactly one root field is allowed for streaming subscriptions"
            | null streamingFields -> do
              let allActionIds = HS.fromList $ map fst $ lefts $ toList liveQueryOnSourceFields
              pure $
                SEOnSourceDB $
                  SSLivequery allActionIds $ \actionLogMap -> do
                    sourceSubFields <- for liveQueryOnSourceFields $ \case
                      Right x -> pure x
                      Left (actionId, (srcConfig, dbExecution)) -> do
                        let sourceName = EA._aaqseSource dbExecution
                        actionLogResponse <-
                          Map.lookup actionId actionLogMap
                            `onNothing` throw500 "unexpected: cannot lookup action_id in the map"
                        let selectAST = EA._aaqseSelectBuilder dbExecution $ actionLogResponse
                            queryDB = case EA._aaqseJsonAggSelect dbExecution of
                              JASMultipleRows -> IR.QDBMultipleRows selectAST
                              JASSingleObject -> IR.QDBSingleRow selectAST
                        pure $ (sourceName, AB.mkAnyBackend $ IR.SourceConfigWith srcConfig Nothing (IR.QDBR queryDB))

                    case OMap.toList sourceSubFields of
                      [] -> throw500 "empty selset for subscription"
                      ((rootFieldName, sub) : _) -> buildAction sub sourceSubFields rootFieldName
            | otherwise -> throw400 NotSupported "streaming and livequery subscriptions cannot be executed in the same subscription"
      | otherwise ->
        throw400
          NotSupported
          "async action queries with no relationships aren't expected to mix with normal source database queries"
  where
    go ::
      ( ( RootFieldMap
            ( Either
                (ActionId, (PGSourceConfig, EA.AsyncActionQuerySourceExecution (IR.UnpreparedValue ('Postgres 'Vanilla))))
                (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
            ),
          RootFieldMap (ActionId, ActionLogResponse -> Either QErr EncJSON)
        ),
        RootFieldMap (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
      ) ->
      (RootFieldAlias, IR.QueryRootField IR.UnpreparedValue) ->
      m
        ( ( RootFieldMap
              ( Either
                  (ActionId, (PGSourceConfig, EA.AsyncActionQuerySourceExecution (IR.UnpreparedValue ('Postgres 'Vanilla))))
                  (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
              ),
            RootFieldMap (ActionId, ActionLogResponse -> Either QErr EncJSON)
          ),
          RootFieldMap (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
        )
    go (accLiveQueryFields, accStreamingFields) (gName, field) = case field of
      IR.RFRemote _ -> throw400 NotSupported "subscription to remote server is not supported"
      IR.RFRaw _ -> throw400 NotSupported "Introspection not supported over subscriptions"
      IR.RFDB src e -> do
        let subscriptionType =
              case AB.unpackAnyBackend @('Postgres 'Vanilla) e of
                Just (IR.SourceConfigWith _ _ (IR.QDBR (IR.QDBStreamMultipleRows _))) -> Streaming
                _ -> LiveQuery
        newQDB <- AB.traverseBackend @EB.BackendExecute e \(IR.SourceConfigWith srcConfig queryTagsConfig (IR.QDBR qdb)) -> do
          let (newQDB, remoteJoins) = RJ.getRemoteJoinsQueryDB qdb
          unless (isNothing remoteJoins) $
            throw400 NotSupported "Remote relationships are not allowed in subscriptions"
          pure $ IR.SourceConfigWith srcConfig queryTagsConfig (IR.QDBR newQDB)
        case subscriptionType of
          Streaming -> pure (accLiveQueryFields, OMap.insert gName (src, newQDB) accStreamingFields)
          LiveQuery -> pure $ (first (OMap.insert gName (Right (src, newQDB))) accLiveQueryFields, accStreamingFields)
      IR.RFAction action -> do
        let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
        unless (isNothing remoteJoins) $
          throw400 NotSupported "Remote relationships are not allowed in subscriptions"
        case noRelsDBAST of
          IR.AQAsync q -> do
            let actionId = IR._aaaqActionId q
            case EA.resolveAsyncActionQuery userInfo q of
              EA.AAQENoRelationships respMaker ->
                pure $ (second (OMap.insert gName (actionId, respMaker)) accLiveQueryFields, accStreamingFields)
              EA.AAQEOnSourceDB srcConfig dbExecution ->
                pure $ (first (OMap.insert gName (Left (actionId, (srcConfig, dbExecution)))) accLiveQueryFields, accStreamingFields)
          IR.AQQuery _ -> throw400 NotSupported "query actions cannot be run as a subscription"

    buildAction ::
      (SourceName, AB.AnyBackend (IR.SourceConfigWith b)) ->
      RootFieldMap
        (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue))) ->
      RootFieldAlias ->
      ExceptT QErr IO (SourceName, SubscriptionQueryPlan)
    buildAction (sourceName, exists) allFields rootFieldName = do
      subscriptionPlan <- AB.dispatchAnyBackend @EB.BackendExecute
        exists
        \(IR.SourceConfigWith sourceConfig queryTagsConfig _ :: IR.SourceConfigWith db b) -> do
          qdbs <- traverse (checkField @b sourceName) allFields
          let subscriptionQueryTagsAttributes = encodeQueryTags $ QTLiveQuery $ LivequeryMetadata rootFieldName parameterizedQueryHash
          let queryTagsComment = Tagged.untag $ EB.createQueryTags @m subscriptionQueryTagsAttributes queryTagsConfig
          SubscriptionQueryPlan . AB.mkAnyBackend . MultiplexedSubscriptionQueryPlan
            <$> runReaderT (EB.mkLiveQuerySubscriptionPlan userInfo sourceName sourceConfig (_rfaNamespace rootFieldName) qdbs) queryTagsComment
      pure (sourceName, subscriptionPlan)

    checkField ::
      forall b m1.
      (Backend b, MonadError QErr m1) =>
      SourceName ->
      (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue))) ->
      m1 (IR.QueryDB b Void (IR.UnpreparedValue b))
    checkField sourceName (src, exists)
      | sourceName /= src = throw400 NotSupported "all fields of a subscription must be from the same source"
      | otherwise = case AB.unpackAnyBackend exists of
        Nothing -> throw500 "internal error: two sources share the same name but are tied to different backends"
        Just (IR.SourceConfigWith _ _ (IR.QDBR qdb)) -> pure qdb

checkQueryInAllowlist ::
  (MonadError QErr m) =>
  Bool ->
  AllowlistMode ->
  UserInfo ->
  GQLReqParsed ->
  SchemaCache ->
  m ()
checkQueryInAllowlist allowlistEnabled allowlistMode userInfo req schemaCache =
  -- only for non-admin roles
  -- check if query is in allowlist
  when (allowlistEnabled && role /= adminRoleName) do
    let query = G.ExecutableDocument . unGQLExecDoc $ _grQuery req
        allowlist = scAllowlist schemaCache
        allowed = allowlistAllowsQuery allowlist allowlistMode role query
    unless allowed $
      modifyQErr modErr $ throw400 ValidationFailed "query is not allowed"
  where
    role = _uiRole userInfo
    modErr e =
      let msg = "query is not in any of the allowlists"
       in e {qeInternal = Just $ ExtraInternal $ J.object ["message" J..= J.String msg]}

-- | Construct a 'ResolvedExecutionPlan' from a 'GQLReqParsed' and a
-- bunch of metadata.
getResolvedExecPlan ::
  forall m.
  ( MonadError QErr m,
    MonadMetadataStorage (MetadataStorageT m),
    MonadIO m,
    MonadBaseControl IO m,
    Tracing.MonadTrace m,
    EC.MonadGQLExecutionCheck m,
    EB.MonadQueryTags m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  UserInfo ->
  SQLGenCtx ->
  ReadOnlyMode ->
  SchemaCache ->
  SchemaCacheVer ->
  ET.GraphQLQueryType ->
  HTTP.Manager ->
  [HTTP.Header] ->
  (GQLReqUnparsed, GQLReqParsed) ->
  RequestId ->
  m (ParameterizedQueryHash, ResolvedExecutionPlan)
getResolvedExecPlan
  env
  logger
  userInfo
  sqlGenCtx
  readOnlyMode
  sc
  _scVer
  queryType
  httpManager
  reqHeaders
  (reqUnparsed, reqParsed)
  reqId = do
    -- 1. Construct the first step of the execution plan.
    (gCtx, queryParts) <- getExecPlanPartial userInfo sc queryType reqParsed

    let maybeOperationName = (Just <$> _unOperationName) =<< _grOperationName reqParsed

    -- 2. Construct the full 'ResolvedExecutionPlan' from the 'queryParts :: SingleOperation'.
    (parameterizedQueryHash, resolvedExecPlan) <-
      case queryParts of
        G.TypedOperationDefinition G.OperationTypeQuery _ varDefs directives inlinedSelSet -> do
          (executionPlan, queryRootFields, dirMap, parameterizedQueryHash) <-
            EQ.convertQuerySelSet
              env
              logger
              gCtx
              userInfo
              httpManager
              reqHeaders
              directives
              inlinedSelSet
              varDefs
              reqUnparsed
              (scSetGraphqlIntrospectionOptions sc)
              reqId
              maybeOperationName
          pure (parameterizedQueryHash, QueryExecutionPlan executionPlan queryRootFields dirMap)
        G.TypedOperationDefinition G.OperationTypeMutation _ varDefs directives inlinedSelSet -> do
          when (readOnlyMode == ReadOnlyModeEnabled) $
            throw400 NotSupported "Mutations are not allowed when read-only mode is enabled"
          (executionPlan, parameterizedQueryHash) <-
            EM.convertMutationSelectionSet
              env
              logger
              gCtx
              sqlGenCtx
              userInfo
              httpManager
              reqHeaders
              directives
              inlinedSelSet
              varDefs
              reqUnparsed
              (scSetGraphqlIntrospectionOptions sc)
              reqId
              maybeOperationName
          pure (parameterizedQueryHash, MutationExecutionPlan executionPlan)
        G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs directives inlinedSelSet -> do
          (normalizedDirectives, normalizedSelectionSet) <-
            ER.resolveVariables
              varDefs
              (fromMaybe mempty (_grVariables reqUnparsed))
              directives
              inlinedSelSet
          subscriptionParser <- C.gqlSubscriptionParser gCtx `onNothing` throw400 ValidationFailed "no subscriptions exist"
          unpreparedAST <- liftEither $ subscriptionParser normalizedSelectionSet
          let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet
          -- Process directives on the subscription
          dirMap <-
            runParse
              (parseDirectives customDirectives (G.DLExecutable G.EDLSUBSCRIPTION) normalizedDirectives)

          -- A subscription should have exactly one root field.
          -- However, for testing purposes, we may allow several root fields; we check for this by
          -- looking for directive "_multiple_top_level_fields" on the subscription. THIS IS NOT A
          -- SUPPORTED FEATURE. We might remove it in the future without warning. DO NOT USE THIS.
          allowMultipleRootFields <- withDirective dirMap multipleRootFields $ pure . isJust
          case inlinedSelSet of
            [] -> throw500 "empty selset for subscription"
            [_] -> pure ()
            _ ->
              unless (allowMultipleRootFields && isSingleNamespace unpreparedAST) $
                throw400 ValidationFailed "subscriptions must select one top level field"
          subscriptionPlan <- buildSubscriptionPlan userInfo unpreparedAST parameterizedQueryHash
          pure (parameterizedQueryHash, SubscriptionExecutionPlan subscriptionPlan)
    -- the parameterized query hash is calculated here because it is used in multiple
    -- places and instead of calculating it separately, this is a common place to calculate
    -- the parameterized query hash and then thread it to the required places
    pure $ (parameterizedQueryHash, resolvedExecPlan)

-- | Even when directive _multiple_top_level_fields is given, we can't allow
-- fields within differently-aliased namespaces.
-- This is because the namespace is added to the result by wrapping
-- the bytestring response we get back from the DB.
isSingleNamespace :: RootFieldMap a -> Bool
isSingleNamespace fieldMap =
  case nubOrd (_rfaNamespace <$> OMap.keys fieldMap) of
    [_] -> True
    _ -> False
