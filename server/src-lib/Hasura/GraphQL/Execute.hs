module Hasura.GraphQL.Execute
  ( EB.ExecutionStep(..)
  , ResolvedExecutionPlan(..)
  , ET.GraphQLQueryType(..)
  , getResolvedExecPlan
  , getExecPlanPartial
  , execRemoteGQ
  , SubscriptionExecution(..)
  , buildSubscriptionPlan
  , ExecutionCtx(..)
  , EC.MonadGQLExecutionCheck(..)
  , checkQueryInAllowlist
  , MultiplexedLiveQueryPlan(..)
  , LiveQueryPlan (..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as HS
import qualified Data.Tagged                            as Tagged
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.GraphQL.Context                 as C
import qualified Hasura.GraphQL.Execute.Action          as EA
import qualified Hasura.GraphQL.Execute.Backend         as EB
import qualified Hasura.GraphQL.Execute.Common          as EC
import qualified Hasura.GraphQL.Execute.LiveQuery.Plan  as EL
import qualified Hasura.GraphQL.Execute.Mutation        as EM
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Execute.RemoteJoin      as RJ
import qualified Hasura.GraphQL.Execute.Types           as ET
import qualified Hasura.Logging                         as L
import qualified Hasura.RQL.IR                          as IR
import qualified Hasura.SQL.AnyBackend                  as AB
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.GraphQL.Parser.Column           (UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Directives
import           Hasura.GraphQL.Parser.Monad
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

import           Hasura.QueryTags
import           Hasura.Server.Types                    (RequestId (..))


-- | Execution context
data ExecutionCtx
  = ExecutionCtx
  { _ecxLogger          :: !(L.Logger L.Hasura)
  , _ecxSqlGenCtx       :: !SQLGenCtx
  , _ecxSchemaCache     :: !SchemaCache
  , _ecxSchemaCacheVer  :: !SchemaCacheVer
  , _ecxHttpManager     :: !HTTP.Manager
  , _ecxEnableAllowList :: !Bool
  }

getExecPlanPartial
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> ET.GraphQLQueryType
  -> GQLReqParsed
  -> m (C.GQLContext, SingleOperation)
getExecPlanPartial userInfo sc queryType req =
  (getGCtx ,) <$> getSingleOperation req
  where
    role = _uiRole userInfo

    contextMap =
      case queryType of
        ET.QueryHasura -> scGQLContext sc
        ET.QueryRelay  -> scRelayContext sc

    defaultContext =
      case queryType of
        ET.QueryHasura -> scUnauthenticatedGQLContext sc
        ET.QueryRelay  -> scUnauthenticatedRelayContext sc

    getGCtx :: C.GQLContext
    getGCtx =
      case Map.lookup role contextMap of
        Nothing  -> defaultContext
        Just (C.RoleContext frontend backend) ->
          case _uiBackendOnlyFieldAccess userInfo of
            BOFAAllowed    -> fromMaybe frontend backend
            BOFADisallowed -> frontend

-- The graphql query is resolved into a sequence of execution operations
data ResolvedExecutionPlan
  = QueryExecutionPlan EB.ExecutionPlan [IR.QueryRootField UnpreparedValue] DirectiveMap
  -- ^ query execution; remote schemas and introspection possible
  | MutationExecutionPlan EB.ExecutionPlan
  -- ^ mutation execution; only __typename introspection supported
  | SubscriptionExecutionPlan SubscriptionExecution
  -- ^ either action query or live query execution; remote schemas and introspection not supported

newtype MultiplexedLiveQueryPlan (b :: BackendType) =
  MultiplexedLiveQueryPlan (EL.LiveQueryPlan b (EB.MultiplexedQuery b))

newtype LiveQueryPlan = LQP (AB.AnyBackend MultiplexedLiveQueryPlan)

-- | The comprehensive subscription plan. We only support either
-- 1. Fields with only async action queries with no associated relationships
--    or
-- 2. Source database query fields from same source and also can be mixed with async
--    action query fields whose relationships are defined to tables in the source
data SubscriptionExecution
  = SEAsyncActionsWithNoRelationships !(InsOrdHashMap G.Name (ActionId, ActionLogResponse -> Either QErr EncJSON))
  | SEOnSourceDB
    !(HashSet ActionId)
    !(ActionLogResponseMap -> ExceptT QErr IO (SourceName, LiveQueryPlan))

buildSubscriptionPlan
  :: forall m
   . (MonadError QErr m, EB.MonadQueryTags m)
  => UserInfo
  -> InsOrdHashMap G.Name (IR.QueryRootField UnpreparedValue)
  -> ParameterizedQueryHash
  -> QueryTagsConfig
  -> m SubscriptionExecution
buildSubscriptionPlan userInfo rootFields parameterizedQueryHash queryTagsConfig = do
  (onSourceFields, noRelationActionFields) <- foldlM go (mempty, mempty) (OMap.toList rootFields)

  if | null onSourceFields -> do
       pure $ SEAsyncActionsWithNoRelationships noRelationActionFields

     | null noRelationActionFields -> do
       let allActionIds = HS.fromList $ map fst $ lefts $ toList onSourceFields
       pure $ SEOnSourceDB allActionIds $ \actionLogMap -> do
         sourceSubFields <- for onSourceFields $ \case
           Right x -> pure x
           Left (actionId, (srcConfig, dbExecution)) -> do
             let sourceName = EA._aaqseSource dbExecution
             actionLogResponse <- Map.lookup actionId actionLogMap
               `onNothing` throw500 "unexpected: cannot lookup action_id in the map"
             let selectAST = EA._aaqseSelectBuilder dbExecution $ actionLogResponse
                 queryDB   = case EA._aaqseJsonAggSelect dbExecution of
                   JASMultipleRows -> IR.QDBMultipleRows selectAST
                   JASSingleObject -> IR.QDBSingleRow    selectAST
             pure $ (sourceName, AB.mkAnyBackend $ IR.SourceConfigWith srcConfig $ IR.QDBR queryDB)

         case OMap.toList sourceSubFields of
           []                      -> throw500 "empty selset for subscription"
           ((rootFieldName,sub):_) -> buildAction sub sourceSubFields rootFieldName

     | otherwise ->
       throw400 NotSupported
       "async action queries with no relationships aren't expected to mix with normal source database queries"
  where
    go accFields (gName, field) = case field of
      IR.RFRemote _      -> throw400 NotSupported "subscription to remote server is not supported"
      IR.RFRaw _         -> throw400 NotSupported "Introspection not supported over subscriptions"
      IR.RFDB src e      -> do
        newQDB <- AB.traverseBackend @EB.BackendExecute e \(IR.SourceConfigWith sc (IR.QDBR qdb)) -> do
          let (newQDB, remoteJoins) = RJ.getRemoteJoins qdb
          unless (isNothing remoteJoins) $
            throw400 NotSupported "Remote relationships are not allowed in subscriptions"
          pure $ IR.SourceConfigWith sc (IR.QDBR newQDB)
        pure $ first (OMap.insert gName (Right (src, newQDB))) accFields
      IR.RFAction action -> do
        let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
        unless (isNothing remoteJoins) $
          throw400 NotSupported "Remote relationships are not allowed in subscriptions"
        case noRelsDBAST of
          IR.AQAsync q -> do
            let actionId = _aaaqActionId q
            case EA.resolveAsyncActionQuery userInfo q of
              EA.AAQENoRelationships respMaker ->
                pure $ second (OMap.insert gName (actionId, respMaker)) accFields
              EA.AAQEOnSourceDB srcConfig dbExecution ->
                pure $ first (OMap.insert gName (Left (actionId, (srcConfig, dbExecution)))) accFields
          IR.AQQuery _ -> throw400 NotSupported "query actions cannot be run as a subscription"

    buildAction (sourceName, exists) allFields rootFieldName = do
      lqp <- AB.dispatchAnyBackend @EB.BackendExecute exists
        \(IR.SourceConfigWith sourceConfig _ :: IR.SourceConfigWith db b) -> do
           qdbs <- traverse (checkField @b sourceName) allFields
           let subscriptionQueryTagsAttributes = encodeQueryTags $  QTLiveQuery $ LivequeryMetadata rootFieldName parameterizedQueryHash
           let qtSourceConfig = getQueryTagsSourceConfig queryTagsConfig sourceName
           let queryTagsComment = QueryTagsComment $ Tagged.untag $ EB.createQueryTags @m (Just qtSourceConfig) subscriptionQueryTagsAttributes
           LQP . AB.mkAnyBackend . MultiplexedLiveQueryPlan
             <$> EB.mkDBSubscriptionPlan userInfo sourceName sourceConfig qdbs queryTagsComment
      pure (sourceName, lqp)

    checkField
      :: forall b m1 . (Backend b, MonadError QErr m1)
      => SourceName
      -> (SourceName, AB.AnyBackend (IR.SourceConfigWith (IR.QueryDBRoot (Const Void) UnpreparedValue)))
      -> m1 (IR.QueryDB b (Const Void) (UnpreparedValue b))
    checkField sourceName (src, exists)
      | sourceName /= src = throw400 NotSupported "all fields of a subscription must be from the same source"
      | otherwise         = case AB.unpackAnyBackend exists of
          Nothing -> throw500 "internal error: two sources share the same name but are tied to different backends"
          Just (IR.SourceConfigWith _ (IR.QDBR qdb)) -> pure qdb

checkQueryInAllowlist
  :: (MonadError QErr m) => Bool -> UserInfo -> GQLReqParsed -> SchemaCache -> m ()
checkQueryInAllowlist enableAL userInfo req sc =
  -- only for non-admin roles
  -- check if query is in allowlist
  when (enableAL && (_uiRole userInfo /= adminRoleName)) $ do
    let notInAllowlist =
          not $ isQueryInAllowlist (_grQuery req) (scAllowlist sc)
    when notInAllowlist $ modifyQErr modErr $ throw400 ValidationFailed "query is not allowed"

  where
    modErr e =
      let msg = "query is not in any of the allowlists"
      in e{qeInternal = Just $ J.object [ "message" J..= J.String msg]}

    isQueryInAllowlist q = HS.member gqlQuery
      where
        gqlQuery = GQLQuery $ G.ExecutableDocument $ stripTypenames $
                   unGQLExecDoc q

getResolvedExecPlan
  :: forall m
   . ( HasVersion
     , MonadError QErr m
     , MonadMetadataStorage (MetadataStorageT m)
     , MonadIO m
     , Tracing.MonadTrace m
     , EC.MonadGQLExecutionCheck m
     , EB.MonadQueryTags m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> SchemaCacheVer
  -> ET.GraphQLQueryType
  -> HTTP.Manager
  -> [HTTP.Header]
  -> (GQLReqUnparsed, GQLReqParsed)
  -> RequestId
  -> m (ParameterizedQueryHash, ResolvedExecutionPlan)
getResolvedExecPlan env logger userInfo sqlGenCtx
  sc _scVer queryType httpManager reqHeaders (reqUnparsed, reqParsed) reqId = do
  (gCtx, queryParts) <- getExecPlanPartial userInfo sc queryType reqParsed

  let maybeOperationName = (Just <$> _unOperationName) =<< _grOperationName reqParsed
  let queryTagsConfig = scQueryTagsConfig sc

  (parameterizedQueryHash, resolvedExecPlan) <-
    case queryParts of
      G.TypedOperationDefinition G.OperationTypeQuery _ varDefs directives inlinedSelSet -> do
        (executionPlan, queryRootFields, dirMap, parameterizedQueryHash) <-
          EQ.convertQuerySelSet env logger gCtx userInfo httpManager reqHeaders directives inlinedSelSet
            varDefs (_grVariables reqUnparsed) (scSetGraphqlIntrospectionOptions sc)
            reqId maybeOperationName queryTagsConfig
        pure $ (parameterizedQueryHash, QueryExecutionPlan executionPlan queryRootFields dirMap)
      G.TypedOperationDefinition G.OperationTypeMutation _ varDefs directives inlinedSelSet -> do
        (executionPlan, parameterizedQueryHash) <-
          EM.convertMutationSelectionSet env logger gCtx sqlGenCtx userInfo httpManager reqHeaders
            directives inlinedSelSet varDefs (_grVariables reqUnparsed) (scSetGraphqlIntrospectionOptions sc)
            reqId maybeOperationName queryTagsConfig
        pure $ (parameterizedQueryHash, MutationExecutionPlan executionPlan)
      G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs directives inlinedSelSet -> do
        -- Parse as query to check correctness
        (unpreparedAST, normalizedDirectives, normalizedSelectionSet) <-
          EQ.parseGraphQLQuery gCtx varDefs (_grVariables reqUnparsed) directives inlinedSelSet
        let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet
        -- Process directives on the subscription
        dirMap <-  (`onLeft` reportParseErrors) =<<
          runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLSUBSCRIPTION) normalizedDirectives)
        -- A subscription should have exactly one root field.
        -- However, for testing purposes, we may allow several root fields; we check for this by
        -- looking for directive "_multiple_top_level_fields" on the subscription. THIS IS NOT A
        -- SUPPORTED FEATURE. We might remove it in the future without warning. DO NOT USE THIS.
        allowMultipleRootFields <- withDirective dirMap multipleRootFields $ pure . isJust
        case inlinedSelSet of
          [_] -> pure ()
          []  -> throw500 "empty selset for subscription"
          _   -> unless allowMultipleRootFields $
            throw400 ValidationFailed "subscriptions must select one top level field"
        subscriptionPlan <- buildSubscriptionPlan userInfo unpreparedAST parameterizedQueryHash queryTagsConfig
        pure (parameterizedQueryHash, SubscriptionExecutionPlan subscriptionPlan)
  -- the parameterized query hash is calculated here because it is used in multiple
  -- places and instead of calculating it separately, this is a common place to calculate
  -- the parameterized query hash and then thread it to the required places
  pure $ (parameterizedQueryHash, resolvedExecPlan)
