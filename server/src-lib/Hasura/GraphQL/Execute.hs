module Hasura.GraphQL.Execute
  ( EPr.ExecutionStep(..)
  , ResolvedExecutionPlan(..)
  , EQ.GraphQLQueryType(..)
  , getResolvedExecPlan
  , execRemoteGQ
  -- , getSubsOp

  , EP.PlanCache
  , EP.mkPlanCacheOptions
  , EP.PlanCacheOptions
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
                                                         mkSetCookieHeaders)
import           Hasura.Server.Version                  (HasVersion)

import qualified Hasura.GraphQL.Context                 as C
import qualified Hasura.GraphQL.Execute.Inline          as EI
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Mutation        as EM
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Prepare         as EPr
import qualified Hasura.GraphQL.Execute.Query           as EQ
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
  -> GQLReqParsed
  -> m (C.GQLContext, QueryParts)
getExecPlanPartial userInfo sc enableAL req = do
  -- check if query is in allowlist
  when enableAL checkQueryInAllowlist

  let gCtx = getGCtx sc role

  (gCtx ,) <$> getQueryParts req
  where
    role = userRole userInfo

    checkQueryInAllowlist =
      -- only for non-admin roles
      when (role /= adminRole) $ do
        let notInAllowlist =
              not $ _isQueryInAllowlist (_grQuery req) (scAllowlist sc)
        when notInAllowlist $ modifyQErr modErr $ _throwVE "query is not allowed"

    modErr e =
      let msg = "query is not in any of the allowlists"
      in e{qeInternal = Just $ J.object [ "message" J..= J.String msg]}

    getGCtx :: SchemaCache -> RoleName -> C.GQLContext
    getGCtx sc rn =
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      {- TODO TODO FIXME of course this is wrong -}
      fromMaybe (head $ Map.elems $ scGQLContext sc) $ Map.lookup rn (scGQLContext sc)

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
          onNothing opDefM $ _throwVE $
            "no such operation found in the document: " <> _showName n
        (Just _, _, _)  ->
          _throwVE $ "operationName cannot be used when " <>
          "an anonymous operation exists in the document"
        (Nothing, [selSet], []) ->
          return $ G.TypedOperationDefinition G.OperationTypeQuery Nothing [] [] selSet
        (Nothing, [], [opDef])  ->
          return opDef
        (Nothing, _, _) ->
          _throwVE $ "exactly one operation has to be present " <>
          "in the document when operationName is not specified"

-- The graphql query is resolved into a sequence of execution operations
data ResolvedExecutionPlan
  = QueryExecutionPlan (EPr.ExecutionPlan (LazyRespTx, EQ.GeneratedSqlMap) EPr.RemoteCall (G.Name, J.Value))
  -- ^ query execution; remote schemata and introspection possible (TODO implement remote)
  | MutationExecutionPlan (EPr.ExecutionPlan (LazyRespTx, HTTP.ResponseHeaders) EPr.RemoteCall (G.Name, J.Value))
  -- ^ mutation execution; only __typename introspection supported (TODO implement remote)
  | SubscriptionExecutionPlan (EPr.ExecutionPlan EL.LiveQueryPlan Void Void)
  -- ^ live query execution; remote schemata and introspection not supported

-- An execution operation, in case of
-- queries and mutations it is just a transaction
-- to be executed
-- data ExecOp
--   = ExOpQuery !LazyRespTx !(Maybe EQ.GeneratedSqlMap)
--   | ExOpMutation !N.ResponseHeaders !LazyRespTx
--   | ExOpSubs !EL.LiveQueryPlan

-- The graphql query is resolved into an execution operation
-- type ExecPlanResolved = GQExecPlan ExecOp

getResolvedExecPlan
  :: forall m . (HasVersion, MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> SchemaCache
  -> SchemaCacheVer
  -> EQ.GraphQLQueryType
  -> HTTP.Manager
  -> [HTTP.Header]
  -> GQLReqUnparsed
  -> m (Telem.CacheHit, ResolvedExecutionPlan)
getResolvedExecPlan pgExecCtx planCache userInfo sqlGenCtx
  enableAL sc scVer queryType httpManager reqHeaders reqUnparsed = do
  planM <- liftIO $ EP.getPlan scVer (userRole userInfo)
           opNameM queryStr planCache
  let usrVars = userVars userInfo
  case planM of
    -- plans are only for queries and subscriptions
    Just plan -> (Telem.Hit,) <$> case plan of
      EP.RPQuery queryPlan -> do
        (tx, genSql) <- EQ.queryOpFromPlan usrVars queryVars queryPlan
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
      let
        takeFragment = \case G.ExecutableDefinitionFragment f -> Just f; _ -> Nothing
        fragments =
            mapMaybe takeFragment $ unGQLExecDoc $ _grQuery req
      (gCtx, queryParts) <- getExecPlanPartial userInfo sc enableAL req
      case queryParts of
        G.TypedOperationDefinition G.OperationTypeQuery _ varDefs _ selSet -> do
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          (execPlan, plan, _unprepared) <-
            EQ.convertQuerySelSet gCtx (userVars userInfo) inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
          return $ QueryExecutionPlan $ execPlan
        G.TypedOperationDefinition G.OperationTypeMutation _ varDefs _ selSet -> do
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          queryTx <- EM.convertMutationSelectionSet gCtx (userVars userInfo) httpManager reqHeaders
                     inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
          return $ MutationExecutionPlan $ EPr.ExecStepDB queryTx
        G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs _ selSet -> do
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          -- Parse as query to check correctness
          (_execPlan, _plan, unpreparedAST) <-
            EQ.convertQuerySelSet gCtx (userVars userInfo) inlinedSelSet varDefs (_grVariables reqUnparsed)
          validSubscriptionAST <- for unpreparedAST $ \case
            C.RFDB x -> pure $ C.RFDB x
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

{-
-- Monad for resolving a hasura query/mutation
type E m =
  ReaderT ( UserInfo
          , QueryCtxMap
          , MutationCtxMap
          , TypeMap
          , FieldMap
          , OrdByCtx
          , InsCtxMap
          , SQLGenCtx
          ) (ExceptT QErr m)

runE
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> E m a
  -> m a
runE ctx sqlGenCtx userInfo action = do
  res <- runExceptT $ runReaderT action
    (userInfo, queryCtxMap, mutationCtxMap, typeMap, fldMap, ordByCtx, insCtxMap, sqlGenCtx)
  either throwError return res
  where
    queryCtxMap = _gQueryCtxMap ctx
    mutationCtxMap = _gMutationCtxMap ctx
    typeMap = _gTypes ctx
    fldMap = _gFields ctx
    ordByCtx = _gOrdByCtx ctx
    insCtxMap = _gInsCtxMap ctx

getQueryOp
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> QueryReusability
  -> QueryActionExecuter
  -> VQ.SelSet
  -> m (LazyRespTx, Maybe EQ.ReusableQueryPlan, EQ.GeneratedSqlMap)
getQueryOp gCtx sqlGenCtx userInfo queryReusability actionExecuter selSet =
  runE gCtx sqlGenCtx userInfo $ EQ.convertQuerySelSet queryReusability selSet actionExecuter

mutationRootName :: Text
mutationRootName = "mutation_root"

resolveMutSelSet
  :: ( HasVersion
     , MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has MutationCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     , Has HTTP.Manager r
     , Has [N.Header] r
     , MonadIO m
     )
  => VQ.SelSet
  -> m (LazyRespTx, N.ResponseHeaders)
resolveMutSelSet fields = do
  aliasedTxs <- forM (toList fields) $ \fld -> do
    fldRespTx <- case VQ._fName fld of
      "__typename" -> return (return $ encJFromJValue mutationRootName, [])
      _            -> evalReusabilityT $ GR.mutFldToTx fld
    return (G.unName $ G.unAlias $ VQ._fAlias fld, fldRespTx)

  -- combines all transactions into a single transaction
  return (liftTx $ toSingleTx aliasedTxs, concatMap (snd . snd) aliasedTxs)
  where
    -- A list of aliased transactions for eg
    -- [("f1", Tx r1), ("f2", Tx r2)]
    -- are converted into a single transaction as follows
    -- Tx {"f1": r1, "f2": r2}
    -- toSingleTx :: [(Text, LazyRespTx)] -> LazyRespTx
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $
      forM aliasedTxs $ \(al, (tx, _)) -> (,) al <$> tx

getMutOp
  :: (HasVersion, MonadError QErr m, MonadIO m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> HTTP.Manager
  -> [N.Header]
  -> VQ.SelSet
  -> m (LazyRespTx, N.ResponseHeaders)
getMutOp ctx sqlGenCtx userInfo manager reqHeaders selSet =
  peelReaderT $ resolveMutSelSet selSet
  where
    peelReaderT action =
      runReaderT action
        ( userInfo, queryCtxMap, mutationCtxMap
        , typeMap, fldMap, ordByCtx, insCtxMap, sqlGenCtx
        , manager, reqHeaders
        )
      where
        queryCtxMap = _gQueryCtxMap ctx
        mutationCtxMap = _gMutationCtxMap ctx
        typeMap = _gTypes ctx
        fldMap = _gFields ctx
        ordByCtx = _gOrdByCtx ctx
        insCtxMap = _gInsCtxMap ctx

getSubsOpM
  :: ( MonadError QErr m
     , MonadIO m
     , HasVersion
     )
  => PGExecCtx
  -> QueryReusability
  -> VQ.Field
  -> QueryActionExecuter
  -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
getSubsOpM pgExecCtx initialReusability fld actionExecuter =
  case VQ._fName fld of
    "__typename" ->
      _throwVE "you cannot create a subscription on '__typename' field"
    _            -> do
      (astUnresolved, finalReusability) <- runReusabilityTWith initialReusability $
        GR.queryFldToPGAST fld actionExecuter
      let varTypes = finalReusability ^? _Reusable
      EL.buildLiveQueryPlan pgExecCtx (VQ._fAlias fld) astUnresolved varTypes
getSubsOp
  :: ( MonadError QErr m
     , MonadIO m
     , HasVersion
     )
  => PGExecCtx
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> QueryReusability
  -> QueryActionExecuter
  -> VQ.Field
  -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
getSubsOp pgExecCtx gCtx sqlGenCtx userInfo queryReusability actionExecuter fld =
  runE gCtx sqlGenCtx userInfo $ getSubsOpM pgExecCtx queryReusability fld actionExecuter
-}

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
