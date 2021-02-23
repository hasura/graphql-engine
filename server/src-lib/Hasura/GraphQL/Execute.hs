module Hasura.GraphQL.Execute
  ( EPr.ExecutionStep(..)
  , ResolvedExecutionPlan(..)
  , ET.GraphQLQueryType(..)
  , getResolvedExecPlan
  , getExecPlanPartial
  , execRemoteGQ
  , EQ.PreparedSql(..)
  , ExecutionCtx(..)
  , MonadGQLExecutionCheck(..)
  , checkQueryInAllowlist
  , LiveQueryPlan (..)
  , createSubscriptionPlan
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as HS
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

import           Data.Text.Extended
import           Data.Typeable

import qualified Hasura.GraphQL.Context                 as C
import qualified Hasura.GraphQL.Execute.Backend         as EB
import qualified Hasura.GraphQL.Execute.Inline          as EI
import qualified Hasura.GraphQL.Execute.LiveQuery.Plan  as EL
import qualified Hasura.GraphQL.Execute.Mutation        as EM
import qualified Hasura.GraphQL.Execute.Prepare         as EPr
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Execute.RemoteJoin      as RJ
import qualified Hasura.GraphQL.Execute.Types           as ET
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing

import           Hasura.GraphQL.Parser.Column           (UnpreparedValue)
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


type QueryParts = G.TypedOperationDefinition G.FragmentSpread G.Name

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
  -> m (C.GQLContext, QueryParts)
getExecPlanPartial userInfo sc queryType req =
  (getGCtx ,) <$> getQueryParts req
  where
    roleName = _uiRole userInfo

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
      case Map.lookup roleName contextMap of
        Nothing  -> defaultContext
        Just (C.RoleContext frontend backend) ->
          case _uiBackendOnlyFieldAccess userInfo of
            BOFAAllowed    -> fromMaybe frontend backend
            BOFADisallowed -> frontend

    -- | Depending on the request parameters, fetch the correct typed operation
    -- definition from the GraphQL query
    getQueryParts
      :: MonadError QErr m
      => GQLReqParsed
      -> m QueryParts
    getQueryParts (GQLReq opNameM q _varValsM) = do
      let (selSets, opDefs, _fragDefsL) = G.partitionExDefs $ unGQLExecDoc q
      case (opNameM, selSets, opDefs) of
        (Just opName, [], _) -> do
          let n = _unOperationName opName
              opDefM = find (\opDef -> G._todName opDef == Just n) opDefs
          onNothing opDefM $ throw400 ValidationFailed $
            "no such operation found in the document: " <> dquote n
        (Just _, _, _)  ->
          throw400 ValidationFailed $ "operationName cannot be used when " <>
          "an anonymous operation exists in the document"
        (Nothing, [selSet], []) ->
          return $ G.TypedOperationDefinition G.OperationTypeQuery Nothing [] [] selSet
        (Nothing, [], [opDef])  ->
          return opDef
        (Nothing, _, _) ->
          throw400 ValidationFailed $ "exactly one operation has to be present " <>
          "in the document when operationName is not specified"

-- The graphql query is resolved into a sequence of execution operations
data ResolvedExecutionPlan
  = QueryExecutionPlan EB.ExecutionPlan [C.QueryRootField UnpreparedValue]
  -- ^ query execution; remote schemas and introspection possible
  | MutationExecutionPlan EB.ExecutionPlan
  -- ^ mutation execution; only __typename introspection supported
  | SubscriptionExecutionPlan SourceName LiveQueryPlan
  -- ^ live query execution; remote schemas and introspection not supported

data LiveQueryPlan where
  LQP :: forall b. EB.BackendExecute b => EL.LiveQueryPlan b (EB.MultiplexedQuery b) -> LiveQueryPlan


createSubscriptionPlan
  :: forall m
   . ( MonadError QErr m
     , MonadIO m
     )
  => UserInfo
  -> InsOrdHashMap G.Name (C.QueryRootField UnpreparedValue)
  -> m (SourceName, LiveQueryPlan)
createSubscriptionPlan userInfo rootFields = do
  subscriptions <- for rootFields \case
    C.RFDB src e x           -> pure $ C.RFDB src e x
    C.RFAction (C.AQAsync _) -> throw400 NotSupported "async action queries are temporarily not supported in subscription"
    C.RFAction (C.AQQuery _) -> throw400 NotSupported "query actions cannot be run as a subscription"
    C.RFRemote _             -> throw400 NotSupported "subscription to remote server is not supported"
    C.RFRaw _                -> throw400 NotSupported "Introspection not supported over subscriptions"
  for_ subscriptions \(C.RFDB _ _ (C.QDBR qdb)) -> do
    unless (isNothing $ RJ.getRemoteJoins qdb) $
      throw400 NotSupported "Remote relationships are not allowed in subscriptions"
  case toList subscriptions of
    []      -> throw500 "empty selset for subscription"
    (sub:_) -> buildAction sub subscriptions
  where
    buildAction (C.RFDB sourceName (sourceConfig :: SourceConfig b) _) allFields = do
      qdbs <- traverse (checkField @b sourceName) allFields
      lqp  <- case backendTag @b of
        PostgresTag -> LQP <$> EB.mkDBSubscriptionPlan userInfo sourceConfig qdbs
        MSSQLTag    -> LQP <$> EB.mkDBSubscriptionPlan userInfo sourceConfig qdbs
      pure (sourceName, lqp)
    checkField
      :: forall b. Backend b
      => SourceName
      -> C.SubscriptionRootField UnpreparedValue
      -> m (C.QueryDB b (UnpreparedValue b))
    checkField sourceName (C.RFDB src _ (C.QDBR qdb))
      | sourceName /= src = throw400 NotSupported "all fields of a subscription must be from the same source"
      | otherwise         = cast qdb `onNothing`
        throw500 "internal error: two sources share the same name but are tied to different backends"


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
  -> m (Telem.CacheHit, ResolvedExecutionPlan)
getResolvedExecPlan env logger {- planCache-} userInfo sqlGenCtx
  sc scVer queryType httpManager reqHeaders (reqUnparsed, reqParsed) = -- do

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
    GQLReq opNameM queryStr queryVars = reqUnparsed
    -- addPlanToCache plan =
    --   liftIO $ EP.addPlan scVer (userRole userInfo)
    --   opNameM queryStr plan planCache
    noExistingPlan :: m ResolvedExecutionPlan
    noExistingPlan = do
      -- GraphQL requests may incorporate fragments which insert a pre-defined
      -- part of a GraphQL query. Here we make sure to remember those
      -- pre-defined sections, so that when we encounter a fragment spread
      -- later, we can inline it instead.
      let takeFragment = \case G.ExecutableDefinitionFragment f -> Just f; _ -> Nothing
          fragments =
            mapMaybe takeFragment $ unGQLExecDoc $ _grQuery reqParsed
      (gCtx, queryParts) <- getExecPlanPartial userInfo sc queryType reqParsed

      case queryParts of
        G.TypedOperationDefinition G.OperationTypeQuery _ varDefs dirs selSet -> do
          -- (Here the above fragment inlining is actually executed.)
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          uncurry QueryExecutionPlan <$>
            EQ.convertQuerySelSet env logger gCtx userInfo httpManager reqHeaders dirs inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- See Note [Temporarily disabling query plan caching]
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
        G.TypedOperationDefinition G.OperationTypeMutation _ varDefs _ selSet -> do
          -- (Here the above fragment inlining is actually executed.)
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          MutationExecutionPlan <$>
            EM.convertMutationSelectionSet env logger gCtx sqlGenCtx userInfo httpManager reqHeaders
            inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- See Note [Temporarily disabling query plan caching]
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
        G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs directives selSet -> do
          -- (Here the above fragment inlining is actually executed.)
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          -- Parse as query to check correctness
          (unpreparedAST, _reusability) <-
            EQ.parseGraphQLQuery gCtx varDefs (_grVariables reqUnparsed) inlinedSelSet
          -- A subscription should have exactly one root field
          -- As an internal testing feature, we support subscribing to multiple
          -- root fields in a subcription. First, we check if the corresponding directive
          -- (@_multiple_top_level_fields) is set.
          case inlinedSelSet of
            [] -> throw500 "empty selset for subscription"
            [_] -> pure ()
            (_:rst) ->
              let multipleAllowed =
                    -- TODO!!!
                    -- We support directives we don't expose in the schema?!
                    G.Directive $$(G.litName "_multiple_top_level_fields") mempty `elem` directives
              in
              unless (multipleAllowed || null rst) $
                throw400 ValidationFailed "subscriptions must select one top level field"
          uncurry SubscriptionExecutionPlan <$> createSubscriptionPlan userInfo unpreparedAST
