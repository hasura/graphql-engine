module Hasura.GraphQL.Execute
  ( EB.ExecutionStep (..),
    ResolvedExecutionPlan (..),
    ET.GraphQLQueryType (..),
    getResolvedExecPlan,
    makeGQLContext,
    execRemoteGQ,
    SubscriptionExecution (..),
    buildSubscriptionPlan,
    EC.MonadGQLExecutionCheck (..),
    checkQueryInAllowlist,
    MultiplexedSubscriptionQueryPlan (..),
    SubscriptionQueryPlan (..),
    SourceSubscription (..),
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.Containers.ListUtils (nubOrd)
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as HS
import Data.List (elemIndex)
import Data.Monoid (Endo (..))
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
import Hasura.GraphQL.RemoteServer (execRemoteGQ)
import Hasura.GraphQL.Schema.Parser (runParse, toQErr)
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Logging qualified as L
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.ModelInformation
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Allowlist
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.OpenTelemetry (getOtelTracesPropagator)
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Subscription
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Init qualified as Init
import Hasura.Server.Prometheus (PrometheusMetrics)
import Hasura.Server.Types (MonadGetPolicies, ReadOnlyMode (..), RequestId (..))
import Hasura.Services
import Hasura.Session (BackendOnlyFieldAccess (..), UserInfo (..))
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

-- | Construct a single step of an execution plan.
makeGQLContext ::
  UserInfo ->
  SchemaCache ->
  ET.GraphQLQueryType ->
  C.GQLContext
makeGQLContext userInfo sc queryType =
  case HashMap.lookup role contextMap of
    Nothing -> defaultContext
    Just (C.RoleContext frontend backend) ->
      case _uiBackendOnlyFieldAccess userInfo of
        BOFAAllowed -> fromMaybe frontend backend
        BOFADisallowed -> frontend
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

-- The graphql query is resolved into a sequence of execution operations
data ResolvedExecutionPlan
  = -- | query execution; remote schemas and introspection possible
    QueryExecutionPlan EB.ExecutionPlan [IR.QueryRootField IR.UnpreparedValue] DirectiveMap
  | -- | mutation execution; only __typename introspection supported
    MutationExecutionPlan EB.ExecutionPlan
  | -- | either action query or live query execution; remote schemas and introspection not supported
    SubscriptionExecutionPlan (SubscriptionExecution, Maybe (Endo JO.Value))

newtype MultiplexedSubscriptionQueryPlan (b :: BackendType)
  = MultiplexedSubscriptionQueryPlan (ES.SubscriptionQueryPlan b (EB.MultiplexedQuery b))

newtype SubscriptionQueryPlan = SubscriptionQueryPlan (AB.AnyBackend MultiplexedSubscriptionQueryPlan)

data SourceSubscription
  = SSLivequery !(HashSet ActionId) !(ActionLogResponseMap -> ExceptT QErr IO ((SourceName, SubscriptionQueryPlan), [ModelInfoPart]))
  | SSStreaming !RootFieldAlias !((SourceName, SubscriptionQueryPlan), [ModelInfoPart])

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
  (MonadError QErr m, MonadQueryTags m, MonadIO m, MonadBaseControl IO m) =>
  UserInfo ->
  RootFieldMap (IR.QueryRootField IR.UnpreparedValue) ->
  ParameterizedQueryHash ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m ((SubscriptionExecution, Maybe (Endo JO.Value)), [ModelInfoPart])
buildSubscriptionPlan userInfo rootFields parameterizedQueryHash reqHeaders operationName = do
  (((liveQueryOnSourceFields, noRelationActionFields), streamingFields), modifier) <- foldlM go (((mempty, mempty), mempty), mempty) (InsOrdHashMap.toList rootFields)

  if
    | null liveQueryOnSourceFields && null streamingFields ->
        pure $ ((SEAsyncActionsWithNoRelationships noRelationActionFields, modifier), [])
    | null noRelationActionFields -> do
        if
          | null liveQueryOnSourceFields -> do
              case InsOrdHashMap.toList streamingFields of
                [] -> throw500 "empty selset for subscription"
                [(rootFieldName, (sourceName, exists))] -> do
                  (subscriptionPlan, modelInfoList) <- AB.dispatchAnyBackend @EB.BackendExecute
                    exists
                    \(IR.SourceConfigWith sourceConfig queryTagsConfig (IR.QDBR qdb) :: IR.SourceConfigWith db b) -> do
                      let subscriptionQueryTagsAttributes = encodeQueryTags $ QTLiveQuery $ LivequeryMetadata rootFieldName parameterizedQueryHash
                          queryTagsComment = Tagged.untag $ createQueryTags @m subscriptionQueryTagsAttributes queryTagsConfig
                          mkDBStreamingSubscriptionPlanResult =
                            runReaderT
                              ( EB.mkDBStreamingSubscriptionPlan
                                  userInfo
                                  sourceName
                                  sourceConfig
                                  (rootFieldName, qdb)
                                  reqHeaders
                                  operationName
                              )
                              queryTagsComment
                      modelInfo <- snd <$> mkDBStreamingSubscriptionPlanResult
                      subscriptionPlan' <-
                        SubscriptionQueryPlan
                          . AB.mkAnyBackend
                          . MultiplexedSubscriptionQueryPlan
                          . fst
                          <$> mkDBStreamingSubscriptionPlanResult
                      pure (subscriptionPlan', modelInfo)
                  pure
                    $ (,[])
                    $ (,modifier)
                    $ SEOnSourceDB
                    $ SSStreaming rootFieldName
                    $ ((sourceName, subscriptionPlan), modelInfoList)
                _ -> throw400 NotSupported "exactly one root field is allowed for streaming subscriptions"
          | null streamingFields -> do
              let allActionIds = HS.fromList $ map fst $ lefts $ toList liveQueryOnSourceFields
              pure
                $ (,[])
                $ (,modifier)
                $ SEOnSourceDB
                $ SSLivequery allActionIds
                $ \actionLogMap -> do
                  sourceSubFields <- for liveQueryOnSourceFields $ \case
                    Right x -> pure x
                    Left (actionId, (srcConfig, dbExecution)) -> do
                      let sourceName = EA._aaqseSource dbExecution
                      actionLogResponse <-
                        HashMap.lookup actionId actionLogMap
                          `onNothing` throw500 "unexpected: cannot lookup action_id in the map"
                      let selectAST = EA._aaqseSelectBuilder dbExecution $ actionLogResponse
                          queryDB = case EA._aaqseJsonAggSelect dbExecution of
                            JASMultipleRows -> IR.QDBMultipleRows selectAST
                            JASSingleObject -> IR.QDBSingleRow selectAST
                      pure $ (sourceName, AB.mkAnyBackend $ IR.SourceConfigWith srcConfig Nothing (IR.QDBR queryDB))

                  case InsOrdHashMap.toList sourceSubFields of
                    [] -> throw500 "empty selset for subscription"
                    ((rootFieldName, sub) : _) -> buildAction sub sourceSubFields rootFieldName
          | otherwise -> throw400 NotSupported "streaming and livequery subscriptions cannot be executed in the same subscription"
    | otherwise ->
        throw400
          NotSupported
          "async action queries with no relationships aren't expected to mix with normal source database queries"
  where
    go ::
      ( ( ( RootFieldMap
              ( Either
                  (ActionId, (PGSourceConfig, EA.AsyncActionQuerySourceExecution (IR.UnpreparedValue ('Postgres 'Vanilla))))
                  (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
              ),
            RootFieldMap (ActionId, ActionLogResponse -> Either QErr EncJSON)
          ),
          RootFieldMap (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
        ),
        Maybe (Endo JO.Value)
      ) ->
      (RootFieldAlias, IR.QueryRootField IR.UnpreparedValue) ->
      m
        ( ( ( RootFieldMap
                ( Either
                    (ActionId, (PGSourceConfig, EA.AsyncActionQuerySourceExecution (IR.UnpreparedValue ('Postgres 'Vanilla))))
                    (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
                ),
              RootFieldMap (ActionId, ActionLogResponse -> Either QErr EncJSON)
            ),
            RootFieldMap (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue)))
          ),
          Maybe (Endo JO.Value)
        )
    go ((accLiveQueryFields, accStreamingFields), modifier) (gName, field) = case field of
      IR.RFRemote _ _ -> throw400 NotSupported "subscription to remote server is not supported"
      IR.RFRaw val -> do
        when (isNothing (_rfaNamespace gName)) do
          throw400 NotSupported "Introspection at root field level is not supported over subscriptions"
        let fieldIndex = fromMaybe 0 $ elemIndex gName $ InsOrdHashMap.keys rootFields
            newModifier oldResponse =
              case JO.asObject oldResponse of
                Left (_err :: String) -> oldResponse
                Right responseObj ->
                  JO.Object $ JO.insert (fieldIndex, G.unName $ _rfaAlias gName) val responseObj
        pure ((accLiveQueryFields, accStreamingFields), Just (Endo newModifier) <> modifier)
      IR.RFMulti _ -> throw400 NotSupported "not supported over subscriptions"
      IR.RFDB src e -> do
        let subscriptionType =
              case AB.unpackAnyBackend @('Postgres 'Vanilla) e of
                Just (IR.SourceConfigWith _ _ (IR.QDBR (IR.QDBStreamMultipleRows _))) -> Streaming
                _ -> case AB.unpackAnyBackend @('Postgres 'Citus) e of
                  Just (IR.SourceConfigWith _ _ (IR.QDBR (IR.QDBStreamMultipleRows _))) -> Streaming
                  _ -> case AB.unpackAnyBackend @('Postgres 'Cockroach) e of
                    Just (IR.SourceConfigWith _ _ (IR.QDBR (IR.QDBStreamMultipleRows _))) -> Streaming
                    _ -> LiveQuery
        newQDB <- AB.traverseBackend @EB.BackendExecute e \(IR.SourceConfigWith srcConfig queryTagsConfig (IR.QDBR qdb)) -> do
          let (newQDB, remoteJoins) = RJ.getRemoteJoinsQueryDB qdb
          unless (isNothing remoteJoins)
            $ throw400 NotSupported "Remote relationships are not allowed in subscriptions"
          pure $ IR.SourceConfigWith srcConfig queryTagsConfig (IR.QDBR newQDB)
        case subscriptionType of
          Streaming -> pure ((accLiveQueryFields, InsOrdHashMap.insert gName (src, newQDB) accStreamingFields), modifier)
          LiveQuery -> pure ((first (InsOrdHashMap.insert gName (Right (src, newQDB))) accLiveQueryFields, accStreamingFields), modifier)
      IR.RFAction action -> do
        let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
        unless (isNothing remoteJoins)
          $ throw400 NotSupported "Remote relationships are not allowed in subscriptions"
        case noRelsDBAST of
          IR.AQAsync q -> do
            let actionId = IR._aaaqActionId q
            case EA.resolveAsyncActionQuery userInfo q of
              EA.AAQENoRelationships respMaker ->
                pure $ ((second (InsOrdHashMap.insert gName (actionId, respMaker)) accLiveQueryFields, accStreamingFields), modifier)
              EA.AAQEOnSourceDB srcConfig dbExecution ->
                pure $ ((first (InsOrdHashMap.insert gName (Left (actionId, (srcConfig, dbExecution)))) accLiveQueryFields, accStreamingFields), modifier)
          IR.AQQuery _ -> throw400 NotSupported "query actions cannot be run as a subscription"

    buildAction ::
      (SourceName, AB.AnyBackend (IR.SourceConfigWith b)) ->
      RootFieldMap
        (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot Void IR.UnpreparedValue))) ->
      RootFieldAlias ->
      ExceptT QErr IO ((SourceName, SubscriptionQueryPlan), [ModelInfoPart])
    buildAction (sourceName, exists) allFields rootFieldName = do
      (subscriptionPlan, modelInfo) <- AB.dispatchAnyBackend @EB.BackendExecute
        exists
        \(IR.SourceConfigWith sourceConfig queryTagsConfig _ :: IR.SourceConfigWith db b) -> do
          qdbs <- traverse (checkField @b sourceName) allFields
          let subscriptionQueryTagsAttributes = encodeQueryTags $ QTLiveQuery $ LivequeryMetadata rootFieldName parameterizedQueryHash
          let queryTagsComment = Tagged.untag $ createQueryTags @m subscriptionQueryTagsAttributes queryTagsConfig
          first
            ( SubscriptionQueryPlan
                . AB.mkAnyBackend
                . MultiplexedSubscriptionQueryPlan
            )
            <$> runReaderT (EB.mkLiveQuerySubscriptionPlan userInfo sourceName sourceConfig (_rfaNamespace rootFieldName) qdbs reqHeaders operationName) queryTagsComment
      pure ((sourceName, subscriptionPlan), modelInfo)

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
  Init.AllowListStatus ->
  AllowlistMode ->
  UserInfo ->
  GQLReqParsed ->
  SchemaCache ->
  m ()
checkQueryInAllowlist allowListStatus allowlistMode userInfo req schemaCache =
  -- only for non-admin roles
  -- check if query is in allowlist
  when (Init.isAllowListEnabled allowListStatus && role /= adminRoleName) do
    let query = G.ExecutableDocument . unGQLExecDoc $ _grQuery req
        allowlist = scAllowlist schemaCache
        allowed = allowlistAllowsQuery allowlist allowlistMode role query
    unless allowed
      $ modifyQErr modErr
      $ throw400 ValidationFailed "query is not allowed"
  where
    role = _uiRole userInfo
    modErr e =
      let msg = "query is not in any of the allowlists"
       in e {qeInternal = Just $ ExtraInternal $ J.object ["message" J..= J.String msg]}

-- | Construct a 'ResolvedExecutionPlan' from a 'GQLReqParsed' and a
-- bunch of metadata.
--
-- Labelling it as inlineable fixed a performance regression on GHC 8.10.7.
{-# INLINEABLE getResolvedExecPlan #-}
getResolvedExecPlan ::
  forall m.
  ( MonadError QErr m,
    MonadMetadataStorage m,
    MonadIO m,
    MonadBaseControl IO m,
    Tracing.MonadTrace m,
    EC.MonadGQLExecutionCheck m,
    MonadQueryTags m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  PrometheusMetrics ->
  UserInfo ->
  SQLGenCtx ->
  ReadOnlyMode ->
  SchemaCache ->
  ET.GraphQLQueryType ->
  [HTTP.Header] ->
  GQLReqUnparsed ->
  SingleOperation -> -- the first step of the execution plan
  Maybe G.Name ->
  RequestId ->
  m (ParameterizedQueryHash, ResolvedExecutionPlan, [ModelInfoPart])
getResolvedExecPlan
  env
  logger
  prometheusMetrics
  userInfo
  sqlGenCtx
  readOnlyMode
  sc
  queryType
  reqHeaders
  reqUnparsed
  queryParts -- the first step of the execution plan
  maybeOperationName
  reqId = do
    let gCtx = makeGQLContext userInfo sc queryType
        tracesPropagator = getOtelTracesPropagator $ scOpenTelemetryConfig sc

    -- Construct the full 'ResolvedExecutionPlan' from the 'queryParts :: SingleOperation'.
    (parameterizedQueryHash, resolvedExecPlan, modelInfoList') <-
      case queryParts of
        G.TypedOperationDefinition G.OperationTypeQuery _ varDefs directives inlinedSelSet -> Tracing.newSpan "Resolve query execution plan" $ do
          (executionPlan, queryRootFields, dirMap, parameterizedQueryHash, modelInfoList) <-
            EQ.convertQuerySelSet
              env
              logger
              tracesPropagator
              prometheusMetrics
              gCtx
              userInfo
              reqHeaders
              directives
              inlinedSelSet
              varDefs
              reqUnparsed
              (scSetGraphqlIntrospectionOptions sc)
              reqId
              maybeOperationName
          Tracing.attachMetadata [("graphql.operation.type", "query"), ("parameterized_query_hash", bsToTxt $ unParamQueryHash parameterizedQueryHash)]
          pure (parameterizedQueryHash, QueryExecutionPlan executionPlan queryRootFields dirMap, modelInfoList)
        G.TypedOperationDefinition G.OperationTypeMutation _ varDefs directives inlinedSelSet -> Tracing.newSpan "Resolve mutation execution plan" $ do
          when (readOnlyMode == ReadOnlyModeEnabled)
            $ throw400 NotSupported "Mutations are not allowed when read-only mode is enabled"
          (executionPlan, parameterizedQueryHash, modelInfoList) <-
            EM.convertMutationSelectionSet
              env
              logger
              tracesPropagator
              prometheusMetrics
              gCtx
              sqlGenCtx
              userInfo
              reqHeaders
              directives
              inlinedSelSet
              varDefs
              reqUnparsed
              (scSetGraphqlIntrospectionOptions sc)
              reqId
              maybeOperationName
          Tracing.attachMetadata [("graphql.operation.type", "mutation")]
          pure (parameterizedQueryHash, MutationExecutionPlan executionPlan, modelInfoList)
        G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs directives inlinedSelSet -> Tracing.newSpan "Resolve subscription execution plan" $ do
          (normalizedDirectives, normalizedSelectionSet) <-
            ER.resolveVariables
              varDefs
              (fromMaybe mempty (_grVariables reqUnparsed))
              directives
              inlinedSelSet
          subscriptionParser <- C.gqlSubscriptionParser gCtx `onNothing` throw400 ValidationFailed "no subscriptions exist"
          unpreparedAST <- Tracing.newSpan "Parse subscription IR" $ liftEither $ subscriptionParser normalizedSelectionSet
          let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet
          -- Process directives on the subscription
          dirMap <-
            toQErr $ runParse (parseDirectives customDirectives (G.DLExecutable G.EDLSUBSCRIPTION) normalizedDirectives)

          -- A subscription should have exactly one root field.
          -- However, for testing purposes, we may allow several root fields; we check for this by
          -- looking for directive "_multiple_top_level_fields" on the subscription. THIS IS NOT A
          -- SUPPORTED FEATURE. We might remove it in the future without warning. DO NOT USE THIS.
          allowMultipleRootFields <- withDirective dirMap multipleRootFields $ pure . isJust
          case inlinedSelSet of
            [] -> throw500 "empty selset for subscription"
            [_] -> pure ()
            _ ->
              unless (allowMultipleRootFields && isSingleNamespace unpreparedAST)
                $ throw400 ValidationFailed "subscriptions must select one top level field"
          (subscriptionPlan, modelInfoList) <- buildSubscriptionPlan userInfo unpreparedAST parameterizedQueryHash reqHeaders maybeOperationName
          Tracing.attachMetadata [("graphql.operation.type", "subscription")]
          pure (parameterizedQueryHash, SubscriptionExecutionPlan subscriptionPlan, modelInfoList)
    -- the parameterized query hash is calculated here because it is used in multiple
    -- places and instead of calculating it separately, this is a common place to calculate
    -- the parameterized query hash and then thread it to the required places
    pure $ (parameterizedQueryHash, resolvedExecPlan, modelInfoList')

-- | Even when directive _multiple_top_level_fields is given, we can't allow
-- fields within differently-aliased namespaces.
-- This is because the namespace is added to the result by wrapping
-- the bytestring response we get back from the DB.
isSingleNamespace :: RootFieldMap a -> Bool
isSingleNamespace fieldMap =
  case nubOrd (_rfaNamespace <$> InsOrdHashMap.keys fieldMap) of
    [_] -> True
    _ -> False
