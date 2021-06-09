module Hasura.GraphQL.Execute
  ( EPr.ExecutionStep(..)
  , ResolvedExecutionPlan(..)
  , ET.GraphQLQueryType(..)
  , getResolvedExecPlan
  , getExecPlanPartial
  , execRemoteGQ
  , SubscriptionExecution(..)
  , buildSubscriptionPlan
  , EQ.PreparedSql(..)
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
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

import qualified Hasura.GraphQL.Context                 as C
import qualified Hasura.GraphQL.Execute.Action          as EA
import qualified Hasura.GraphQL.Execute.Backend         as EB
import qualified Hasura.GraphQL.Execute.Common          as EC
import qualified Hasura.GraphQL.Execute.LiveQuery.Plan  as EL
import qualified Hasura.GraphQL.Execute.Mutation        as EM
import qualified Hasura.GraphQL.Execute.Prepare         as EPr
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Execute.RemoteJoin      as RJ
import qualified Hasura.GraphQL.Execute.Types           as ET
import qualified Hasura.Logging                         as L
import qualified Hasura.SQL.AnyBackend                  as AB
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Parser.Column           (UnpreparedValue)
import           Hasura.GraphQL.Parser.Directives
import           Hasura.GraphQL.Parser.Monad
import           Hasura.GraphQL.Parser.Schema           (Variable)
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


-- | Execution context
data ExecutionCtx
  = ExecutionCtx
  { _ecxLogger          :: !(L.Logger L.Hasura)
  , _ecxSqlGenCtx       :: !SQLGenCtx
  -- , _ecxPlanCache       :: !EP.PlanCache
  , _ecxSchemaCache     :: !SchemaCache
  , _ecxSchemaCacheVer  :: !SchemaCacheVer
  , _ecxHttpManager     :: !HTTP.Manager
  , _ecxEnableAllowList :: !Bool
  }

-- | Typeclass representing safety checks (if any) that need to be performed
-- before a GraphQL query should be allowed to be executed. In OSS, the safety
-- check is to check in the query is in the allow list.

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

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (ExceptT e m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (ReaderT r m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (Tracing.TraceT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (MetadataStorageT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

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
  = QueryExecutionPlan EB.ExecutionPlan [C.QueryRootField UnpreparedValue]
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
  :: (MonadError QErr m)
  => UserInfo
  -> InsOrdHashMap G.Name (C.QueryRootField UnpreparedValue)
  -> m SubscriptionExecution
buildSubscriptionPlan userInfo rootFields = do
  (onSourceFields, noRelationActionFields) <- foldlM go (mempty, mempty) (OMap.toList rootFields)

  if | null onSourceFields -> pure $ SEAsyncActionsWithNoRelationships noRelationActionFields

     | null noRelationActionFields ->
         let allActionIds = HS.fromList $ map fst $ lefts $ toList onSourceFields
         in pure $ SEOnSourceDB allActionIds $ \actionLogMap -> do
           sourceSubFields <- for onSourceFields $ \case
             Right x -> pure x
             Left (actionId, (srcConfig, dbExecution)) -> do
               let sourceName = EA._aaqseSource dbExecution
               actionLogResponse <- Map.lookup actionId actionLogMap
                 `onNothing` throw500 "unexpected: cannot lookup action_id in the map"
               let selectAST = EA._aaqseSelectBuilder dbExecution $ actionLogResponse
                   queryDB = case EA._aaqseJsonAggSelect dbExecution of
                     JASMultipleRows -> C.QDBMultipleRows selectAST
                     JASSingleObject -> C.QDBSingleRow selectAST
               pure $ C.RFDB sourceName $ AB.mkAnyBackend $ C.SourceConfigWith srcConfig $ C.QDBR queryDB

           for_ sourceSubFields \(C.RFDB _ exists) -> do
             AB.dispatchAnyBackend @EB.BackendExecute exists \(C.SourceConfigWith _ (C.QDBR qdb)) ->
               unless (isNothing $ RJ.getRemoteJoins qdb) $
                 throw400 NotSupported "Remote relationships are not allowed in subscriptions"

           case toList sourceSubFields of
             []      -> throw500 "empty selset for subscription"
             (sub:_) -> buildAction sub sourceSubFields

     | otherwise -> throw400 NotSupported
                    "async action queries with no relationships aren't expected to mix with normal source database queries"
  where
    go accFields (gName, field) = case field of
      C.RFDB src e                 -> pure $ first (OMap.insert gName (Right (C.RFDB src e))) accFields
      C.RFAction (C.AQAsync q) -> do
        let actionId = _aaaqActionId q
        case EA.resolveAsyncActionQuery userInfo q of
          EA.AAQENoRelationships respMaker ->
            pure $ second (OMap.insert gName (actionId, respMaker)) accFields
          EA.AAQEOnSourceDB srcConfig dbExecution ->
            pure $ first (OMap.insert gName (Left (actionId, (srcConfig, dbExecution)))) accFields
      C.RFAction (C.AQQuery _) -> throw400 NotSupported "query actions cannot be run as a subscription"
      C.RFRemote _             -> throw400 NotSupported "subscription to remote server is not supported"
      C.RFRaw _                -> throw400 NotSupported "Introspection not supported over subscriptions"

    buildAction (C.RFDB sourceName exists) allFields = do
      lqp <- AB.dispatchAnyBackend @EB.BackendExecute exists
        \(C.SourceConfigWith sourceConfig _ :: C.SourceConfigWith db b) -> do
           qdbs <- traverse (checkField @b sourceName) allFields
           LQP . AB.mkAnyBackend . MultiplexedLiveQueryPlan
             <$> EB.mkDBSubscriptionPlan userInfo sourceName sourceConfig qdbs
      pure (sourceName, lqp)

    checkField
      :: forall b m. (Backend b, MonadError QErr m)
      => SourceName
      -> C.SubscriptionRootField UnpreparedValue
      -> m (C.QueryDB b (UnpreparedValue b))
    checkField sourceName (C.RFDB src exists)
      | sourceName /= src = throw400 NotSupported "all fields of a subscription must be from the same source"
      | otherwise         = case AB.unpackAnyBackend exists of
          Nothing -> throw500 "internal error: two sources share the same name but are tied to different backends"
          Just (C.SourceConfigWith _ (C.QDBR qdb)) -> pure qdb

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
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -- -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> SchemaCacheVer
  -> ET.GraphQLQueryType
  -> HTTP.Manager
  -> [HTTP.Header]
  -> (GQLReqUnparsed, GQLReqParsed)
  -> m (Telem.CacheHit, (G.SelectionSet G.NoFragments Variable, ResolvedExecutionPlan))
getResolvedExecPlan env logger {- planCache-} userInfo sqlGenCtx
  sc _scVer queryType httpManager reqHeaders (reqUnparsed, reqParsed) = -- do

  -- See Note [Temporarily disabling query plan caching]
  -- planM <- liftIO $ EP.getPlan scVer (_uiRole userInfo) opNameM queryStr
  --          queryType planCache
--   case planM of
--     -- plans are only for queries and subscriptions
--     Just plan -> (Telem.Hit,) <$> case plan of
--       EP.RPQuery queryPlan -> do
-- --        (tx, genSql) <- EQ.queryOpFromPlan env httpManager reqHeaders userInfo queryVars queryPlan
--         return $ QueryExecutionPlan _ -- tx (Just genSql)
--       EP.RPSubs subsPlan ->
--         return $ SubscriptionExecutionPlan _ -- <$> EL.reuseLiveQueryPlan pgExecCtx usrVars queryVars subsPlan
--     Nothing -> (Telem.Miss,) <$> noExistingPlan
  (Telem.Miss,) <$> noExistingPlan
  where
    -- GQLReq opNameM queryStr queryVars = reqUnparsed
    -- addPlanToCache plan =
    --   liftIO $ EP.addPlan scVer (userRole userInfo)
    --   opNameM queryStr plan planCache
    noExistingPlan :: m (G.SelectionSet G.NoFragments Variable, ResolvedExecutionPlan)
    noExistingPlan = do
      (gCtx, queryParts) <- getExecPlanPartial userInfo sc queryType reqParsed

      case queryParts of
        G.TypedOperationDefinition G.OperationTypeQuery _ varDefs directives inlinedSelSet -> do
          (executionPlan, queryRootFields, normalizedSelectionSet) <-
            EQ.convertQuerySelSet env logger gCtx userInfo httpManager reqHeaders directives inlinedSelSet varDefs (_grVariables reqUnparsed) (scSetGraphqlIntrospectionOptions sc)
          pure $ (normalizedSelectionSet, QueryExecutionPlan executionPlan queryRootFields)

          -- See Note [Temporarily disabling query plan caching]
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
        G.TypedOperationDefinition G.OperationTypeMutation _ varDefs directives inlinedSelSet -> do
          (executionPlan, normalizedSelectionSet) <-
            EM.convertMutationSelectionSet env logger gCtx sqlGenCtx userInfo httpManager reqHeaders directives inlinedSelSet varDefs (_grVariables reqUnparsed) (scSetGraphqlIntrospectionOptions sc)
          pure $ (normalizedSelectionSet, MutationExecutionPlan executionPlan)
          -- See Note [Temporarily disabling query plan caching]
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
        G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs directives inlinedSelSet -> do
          -- Parse as query to check correctness
          (unpreparedAST, _reusability, normalizedDirectives, normalizedSelectionSet) <-
            EQ.parseGraphQLQuery gCtx varDefs (_grVariables reqUnparsed) directives inlinedSelSet
          -- Process directives on the subscription
          (dirMap, _) <-  (`onLeft` reportParseErrors) =<<
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
          subscriptionPlan <- buildSubscriptionPlan userInfo unpreparedAST
          pure (normalizedSelectionSet, SubscriptionExecutionPlan subscriptionPlan)
