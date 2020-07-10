module Hasura.GraphQL.Execute
  ( EPr.ExecutionStep(..)
  , ResolvedExecutionPlan(..)
  , ET.GraphQLQueryType(..)
  , getResolvedExecPlan
  , execRemoteGQ
  -- , getSubsOp

  , EP.PlanCache
  , EP.mkPlanCacheOptions
  , EP.PlanCacheOptions(..)
  , EP.initPlanCache
  , EP.clearPlanCache
  , EP.dumpPlanCache

  , ExecutionCtx(..)
  , MonadGQLExecutionCheck(..)
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Data.Has

import qualified Data.Aeson                             as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.Sequence.NonEmpty                 as NESeq
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai
import qualified Network.Wreq                           as Wreq

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Utils                    (RequestId, mkClientHeadersForward,
                                                         mkSetCookieHeaders, userRoleHeader)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

import qualified Hasura.GraphQL.Context                 as C
import qualified Hasura.GraphQL.Execute.Inline          as EI
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Mutation        as EM
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Prepare         as EPr
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Execute.Types           as ET
import qualified Hasura.GraphQL.Parser.Schema           as PS
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem

type QueryParts = G.TypedOperationDefinition G.FragmentSpread G.Name

-- | Execution context
data ExecutionCtx
  = ExecutionCtx
  { _ecxLogger          :: !(L.Logger L.Hasura)
  , _ecxSqlGenCtx       :: !SQLGenCtx
  , _ecxPgExecCtx       :: !PGExecCtx
  , _ecxPlanCache       :: !EP.PlanCache
  , _ecxSchemaCache     :: !SchemaCache
  , _ecxSchemaCacheVer  :: !SchemaCacheVer
  , _ecxHttpManager     :: !HTTP.Manager
  , _ecxEnableAllowList :: !Bool
  }

-- | Typeclass representing safety checks (if any) that need to be performed
-- before a GraphQL query should be allowed to be executed. In OSS, the safety
-- check is to check in the query is in the allow list.

-- | TODO: Limitation: This parses the query, which is not ideal if we already
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

getExecPlanPartial
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> Bool
  -> ET.GraphQLQueryType
  -> GQLReqParsed
  -> m (C.GQLContext, QueryParts)
getExecPlanPartial userInfo sc enableAL queryType req = do
  -- check if query is in allowlist
  when enableAL checkQueryInAllowlist

  (getGCtx ,) <$> getQueryParts req
  where
    roleName = _uiRole userInfo

    checkQueryInAllowlist =
      -- only for non-admin roles
      when (roleName /= adminRoleName) $ do
        let notInAllowlist =
              not $ _isQueryInAllowlist (_grQuery req) (scAllowlist sc)
        when notInAllowlist $ modifyQErr modErr $ throw400 ValidationFailed "query is not allowed"

    modErr e =
      let msg = "query is not in any of the allowlists"
      in e{qeInternal = Just $ J.object [ "message" J..= J.String msg]}

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
        -- TODO FIXME implement backend-only field access
        {-
        Just (RoleContext defaultGCtx maybeBackendGCtx)   ->
          case backendOnlyFieldAccess of
            BOFAAllowed    ->
              -- When backend field access is allowed and if there's no 'backend_only'
              -- permissions defined, we should allow access to non backend only fields
              fromMaybe defaultGCtx maybeBackendGCtx
            BOFADisallowed -> defaultGCtx
        -}

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
            "no such operation found in the document: " <> _showName n
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
  = QueryExecutionPlan (EPr.ExecutionPlan (LazyRespTx, EQ.GeneratedSqlMap) EPr.RemoteCall (G.Name, J.Value))
  -- ^ query execution; remote schemas and introspection possible (TODO implement remote)
  | MutationExecutionPlan (EPr.ExecutionPlan (LazyRespTx, HTTP.ResponseHeaders) EPr.RemoteCall (G.Name, J.Value))
  -- ^ mutation execution; only __typename introspection supported (TODO implement remote)
  | SubscriptionExecutionPlan (EPr.ExecutionPlan EL.LiveQueryPlan Void Void)
  -- ^ live query execution; remote schemas and introspection not supported

getResolvedExecPlan
  :: forall m . (HasVersion, MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> SchemaCache
  -> SchemaCacheVer
  -> ET.GraphQLQueryType
  -> HTTP.Manager
  -> [HTTP.Header]
  -> GQLReqUnparsed
  -> m (Telem.CacheHit, ResolvedExecutionPlan)
getResolvedExecPlan pgExecCtx planCache userInfo sqlGenCtx
  enableAL sc scVer queryType httpManager reqHeaders reqUnparsed = do
  planM <- liftIO $ EP.getPlan scVer (_uiRole userInfo)
           opNameM queryStr queryType planCache
  let usrVars = _uiSession userInfo
  case planM of
    -- plans are only for queries and subscriptions
    Just plan -> (Telem.Hit,) <$> case plan of
      EP.RPQuery queryPlan -> do
        (tx, genSql) <- EQ.queryOpFromPlan httpManager reqHeaders userInfo queryVars queryPlan
        return $ QueryExecutionPlan _ -- tx (Just genSql)
      EP.RPSubs subsPlan ->
        return $ SubscriptionExecutionPlan _ -- <$> EL.reuseLiveQueryPlan pgExecCtx usrVars queryVars subsPlan
    Nothing -> (Telem.Miss,) <$> noExistingPlan
  where
    GQLReq opNameM queryStr queryVars = reqUnparsed
    -- addPlanToCache plan =
    --   liftIO $ EP.addPlan scVer (userRole userInfo)
    --   opNameM queryStr plan planCache
    noExistingPlan :: m ResolvedExecutionPlan
    noExistingPlan = do
      req <- toParsed reqUnparsed
      -- GraphQL requests may incorporate fragments which insert a pre-defined
      -- part of a GraphQL query. Here we make sure to remember those
      -- pre-defined sections, so that when we encounter a fragment spread
      -- later, we can inline it instead.
      let takeFragment = \case G.ExecutableDefinitionFragment f -> Just f; _ -> Nothing
          fragments =
            mapMaybe takeFragment $ unGQLExecDoc $ _grQuery req
        -- TODO check that all defined fragments are used, see:
        -- http://spec.graphql.org/June2018/#sec-Fragments-Must-Be-Used
      (gCtx, queryParts) <- getExecPlanPartial userInfo sc enableAL queryType req
      case queryParts of
        G.TypedOperationDefinition G.OperationTypeQuery _ varDefs _ selSet -> do
          -- (Here the above fragment inlining is actually executed.)
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          (execPlan, plan, _unprepared) <-
            EQ.convertQuerySelSet gCtx userInfo httpManager reqHeaders inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
          return $ QueryExecutionPlan $ execPlan
        G.TypedOperationDefinition G.OperationTypeMutation _ varDefs _ selSet -> do
          -- (Here the above fragment inlining is actually executed.)
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          queryTx <- EM.convertMutationSelectionSet gCtx userInfo httpManager reqHeaders
                     inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
          return $ MutationExecutionPlan $ queryTx
        G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs _ selSet -> do
          -- (Here the above fragment inlining is actually executed.)
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          -- Parse as query to check correctness
          (_execPlan, _plan, unpreparedAST) <-
            EQ.convertQuerySelSet gCtx userInfo httpManager reqHeaders inlinedSelSet varDefs (_grVariables reqUnparsed)
          validSubscriptionAST <- for unpreparedAST $ \case
            C.RFDB x -> pure $ C.RFDB x
            C.RFAction (C.AQAsync s) -> pure $ C.RFAction s
            C.RFAction (C.AQQuery _) -> throw400 NotSupported "action queries are not supported over subscriptions"
            C.RFRemote _ -> throw400 NotSupported "subscription to remote server is not supported"
            C.RFRaw _ -> throw400 NotSupported "Introspection not supported over subscriptions"
          -- TODO we should check that there's only one root field (unless the appropriate directive is set)
          (lqOp, plan) <- EL.buildLiveQueryPlan pgExecCtx userInfo validSubscriptionAST
          -- getSubsOpM pgExecCtx userInfo inlinedSelSet
          return $ SubscriptionExecutionPlan $ EPr.ExecStepDB lqOp

      -- forM partialExecPlan $ \(gCtx, rootSelSet) ->
      --   case rootSelSet of
      --     VQ.RMutation selSet -> do
      --       (tx, respHeaders) <- getMutOp gCtx sqlGenCtx userInfo httpManager reqHeaders selSet
      --       pure $ ExOpMutation respHeaders tx
      --     VQ.RQuery selSet -> do
      --       (queryTx, plan, genSql) <- getQueryOp gCtx sqlGenCtx userInfo queryReusability (allowQueryActionExecuter httpManager reqHeaders) selSet
      --       traverse_ (addPlanToCache . EP.RPQuery) plan
      --       return $ ExOpQuery queryTx (Just genSql)
      --     VQ.RSubscription fld -> do
      --       (lqOp, plan) <- getSubsOp pgExecCtx gCtx sqlGenCtx userInfo queryReusability (restrictActionExecuter "query actions cannot be run as a subscription") fld
      --       traverse_ (addPlanToCache . EP.RPSubs) plan
      --       return $ ExOpSubs lqOp


execRemoteGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [HTTP.Header]
  -> GQLReqUnparsed
  -> RemoteSchemaInfo
  -> G.TypedOperationDefinition G.NoFragments G.Name
  -> m (DiffTime, HttpResponse EncJSON)
  -- ^ Also returns time spent in http request, for telemetry.
execRemoteGQ reqId userInfo reqHdrs q rsi opDef = do
  execCtx <- ask
  let logger  = _ecxLogger execCtx
      manager = _ecxHttpManager execCtx
      opTy    = G._todType opDef
  when (opTy == G.OperationTypeSubscription) $
    throw400 NotSupported "subscription to remote server is not supported"
  confHdrs <- makeHeadersFromConf hdrConf
  let clientHdrs = bool [] (mkClientHeadersForward reqHdrs) fwdClientHdrs
      -- filter out duplicate headers
      -- priority: conf headers > resolved userinfo vars > client headers
      hdrMaps    = [ Map.fromList confHdrs
                   , Map.fromList userInfoToHdrs
                   , Map.fromList clientHdrs
                   ]
      headers  = Map.toList $ foldr Map.union Map.empty hdrMaps
      finalHeaders = addDefaultHeaders headers
  initReqE <- liftIO $ try $ HTTP.parseRequest (show url)
  initReq <- either httpThrow pure initReqE
  let req = initReq
           { HTTP.method = "POST"
           , HTTP.requestHeaders = finalHeaders
           , HTTP.requestBody = HTTP.RequestBodyLBS (J.encode q)
           , HTTP.responseTimeout = HTTP.responseTimeoutMicro (timeout * 1000000)
           }

  L.unLogger logger $ QueryLog q Nothing reqId
  (time, res)  <- withElapsedTime $ liftIO $ try $ HTTP.httpLbs req manager
  resp <- either httpThrow return res
  let !httpResp = HttpResponse (encJFromLBS $ resp ^. Wreq.responseBody) $ mkSetCookieHeaders resp
  return (time, httpResp)

  where
    RemoteSchemaInfo url hdrConf fwdClientHdrs timeout = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow = \case
      HTTP.HttpExceptionRequest _req content -> throw500 $ T.pack . show $ content
      HTTP.InvalidUrlException _url reason -> throw500 $ T.pack . show $ reason

    userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
                     userInfoToList userInfo
