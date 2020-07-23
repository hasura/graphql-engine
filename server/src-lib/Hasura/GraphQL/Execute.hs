module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , EQ.GraphQLQueryType(..)

  , ExecPlanPartial
  , getExecPlanPartial

  , ExecOp(..)
  , GQExecPlanResolved
  , getResolvedExecPlan
  , execRemoteGQ
  , getSubsOp

  , EP.PlanCache
  , EP.PlanCacheOptions(..)
  , EP.initPlanCache
  , EP.clearPlanCache
  , EP.dumpPlanCache
  , EQ.PreparedSql(..)
  , ExecutionCtx(..)

  , MonadGQLExecutionCheck(..)
  , checkQueryInAllowlist
  ) where

import           Control.Lens
import           Data.Has

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP
import qualified Network.Wai.Extended                   as Wai

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.RemoteServer            (execRemoteGQ')
import           Hasura.GraphQL.Resolve.Action
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Types
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

import qualified Hasura.GraphQL.Context                 as GC
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Resolve                 as GR
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.SelectionSet   as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Hasura.Tracing                         as Tracing

-- The current execution plan of a graphql operation, it is
-- currently, either local pg execution or a remote execution
--
-- The 'a' is parameterised so this AST can represent
-- intermediate passes
data GQExecPlan a
  = GExPHasura !a
  | GExPRemote !RemoteSchemaInfo !G.TypedOperationDefinition
  deriving (Functor, Foldable, Traversable)

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

instance MonadGQLExecutionCheck m => MonadGQLExecutionCheck (Tracing.TraceT m) where
  checkGQLExecution ui det enableAL sc req =
    lift $ checkGQLExecution ui det enableAL sc req

-- Enforces the current limitation
assertSameLocationNodes
  :: (MonadError QErr m) => [VT.TypeLoc] -> m VT.TypeLoc
assertSameLocationNodes typeLocs =
  case Set.toList (Set.fromList typeLocs) of
    -- this shouldn't happen
    []    -> return VT.TLHasuraType
    [loc] -> return loc
    _     -> throw400 NotSupported msg
  where
    msg = "cannot mix top level fields from two different graphql servers"

-- TODO: we should fix this function asap
-- as this will fail when there is a fragment at the top level
getTopLevelNodes :: G.TypedOperationDefinition -> [G.Name]
getTopLevelNodes opDef =
  mapMaybe f $ G._todSelectionSet opDef
  where
    f = \case
      G.SelectionField fld        -> Just $ G._fName fld
      G.SelectionFragmentSpread _ -> Nothing
      G.SelectionInlineFragment _ -> Nothing

gatherTypeLocs :: GCtx -> [G.Name] -> [VT.TypeLoc]
gatherTypeLocs gCtx nodes =
  catMaybes $ flip map nodes $ \node ->
    VT._fiLoc <$> Map.lookup node schemaNodes
  where
    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
      in maybe qr (Map.union qr) mr

-- This is for when the graphql query is validated
type ExecPlanPartial = GQExecPlan (GCtx, VQ.RootSelectionSet)

getExecPlanPartial
  :: (MonadReusability m, MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> EQ.GraphQLQueryType
  -> GQLReqParsed
  -> m ExecPlanPartial
getExecPlanPartial userInfo sc queryType req = do
  let gCtx = case queryType of
        EQ.QueryHasura -> getGCtx (_uiBackendOnlyFieldAccess userInfo) sc roleName
        EQ.QueryRelay  -> fromMaybe GC.emptyGCtx $ Map.lookup roleName $ scRelayGCtxMap sc

  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  let opDef = VQ.qpOpDef queryParts
      topLevelNodes = getTopLevelNodes opDef
      -- gather TypeLoc of topLevelNodes
      typeLocs = gatherTypeLocs gCtx topLevelNodes

  -- see if they are all the same
  typeLoc <- assertSameLocationNodes typeLocs

  case typeLoc of
    VT.TLHasuraType -> do
      rootSelSet <- runReaderT (VQ.validateGQ queryParts) gCtx
      pure $ GExPHasura (gCtx, rootSelSet)
    VT.TLRemoteType _ rsi ->
      pure $ GExPRemote rsi opDef
    VT.TLCustom ->
      throw500 "unexpected custom type for top level field"
  where
    roleName = _uiRole userInfo

checkQueryInAllowlist
  :: (MonadError QErr m) => Bool -> UserInfo -> GQLReqParsed -> SchemaCache -> m ()
checkQueryInAllowlist enableAL userInfo req sc =
  -- only for non-admin roles
  -- check if query is in allowlist
  when (enableAL && (_uiRole userInfo /= adminRoleName)) $ do
    let notInAllowlist =
          not $ VQ.isQueryInAllowlist (_grQuery req) (scAllowlist sc)
    when notInAllowlist $ modifyQErr modErr $ throwVE "query is not allowed"

  where
    modErr e =
      let msg = "query is not in any of the allowlists"
      in e{qeInternal = Just $ J.object [ "message" J..= J.String msg]}


-- An execution operation, in case of queries and mutations it is just a
-- transaction to be executed
data ExecOp m
  = ExOpQuery !(m EncJSON) !(Maybe EQ.GeneratedSqlMap) ![GR.QueryRootFldUnresolved]
  | ExOpMutation !HTTP.ResponseHeaders !(m EncJSON)
  | ExOpSubs !EL.LiveQueryPlan

-- The graphql query is resolved into an execution operation
type GQExecPlanResolved m = GQExecPlan (ExecOp m)

getResolvedExecPlan
  :: forall m tx
   . ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> PGExecCtx
  -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> SchemaCacheVer
  -> EQ.GraphQLQueryType
  -> HTTP.Manager
  -> [HTTP.Header]
  -> (GQLReqUnparsed, GQLReqParsed)
  -> m (Telem.CacheHit, GQExecPlanResolved tx)
getResolvedExecPlan env pgExecCtx planCache userInfo sqlGenCtx
  sc scVer queryType httpManager reqHeaders (reqUnparsed, reqParsed) = do

  planM <- liftIO $ EP.getPlan scVer (_uiRole userInfo) operationNameM queryStr
           queryType planCache
  let usrVars = _uiSession userInfo
  case planM of
    -- plans are only for queries and subscriptions
    Just plan -> (Telem.Hit,) . GExPHasura <$> case plan of
      EP.RPQuery queryPlan asts -> do
        (tx, genSql) <- EQ.queryOpFromPlan env httpManager reqHeaders userInfo queryVars queryPlan
        pure $ ExOpQuery tx (Just genSql) asts
      EP.RPSubs subsPlan ->
        ExOpSubs <$> EL.reuseLiveQueryPlan pgExecCtx usrVars queryVars subsPlan
    Nothing -> (Telem.Miss,) <$> noExistingPlan
  where
    GQLReq operationNameM queryStr queryVars = reqUnparsed
    addPlanToCache plan =
      liftIO $ EP.addPlan scVer (_uiRole userInfo)
      operationNameM queryStr plan queryType planCache

    noExistingPlan :: m (GQExecPlanResolved tx)
    noExistingPlan = do
      -- req <- toParsed reqUnparsed
      (partialExecPlan, queryReusability) <- runReusabilityT $
        getExecPlanPartial userInfo sc queryType reqParsed
      forM partialExecPlan $ \(gCtx, rootSelSet) ->
        case rootSelSet of
          VQ.RMutation selSet -> do
            (tx, respHeaders) <- getMutOp env gCtx sqlGenCtx userInfo httpManager reqHeaders selSet
            pure $ ExOpMutation respHeaders tx
          VQ.RQuery selSet -> do
            (queryTx, plan, genSql, asts) <- getQueryOp env gCtx sqlGenCtx httpManager reqHeaders userInfo
                                       queryReusability (allowQueryActionExecuter httpManager reqHeaders) selSet
            traverse_ (addPlanToCache . flip EP.RPQuery asts) plan
            return $ ExOpQuery queryTx (Just genSql) asts
          VQ.RSubscription fields -> do
            (lqOp, plan) <- getSubsOp env pgExecCtx gCtx sqlGenCtx userInfo queryReusability
              (restrictActionExecuter "query actions cannot be run as a subscription") fields
            traverse_ (addPlanToCache . EP.RPSubs) plan
            return $ ExOpSubs lqOp

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
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> GCtx
  -> SQLGenCtx
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> QueryReusability
  -> QueryActionExecuter
  -> VQ.ObjectSelectionSet
  -> m (tx EncJSON, Maybe EQ.ReusableQueryPlan, EQ.GeneratedSqlMap, [GR.QueryRootFldUnresolved])
getQueryOp env gCtx sqlGenCtx manager reqHdrs userInfo queryReusability actionExecuter selSet =
  runE gCtx sqlGenCtx userInfo $ EQ.convertQuerySelSet env manager reqHdrs queryReusability selSet actionExecuter

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
     , Has [HTTP.Header] r
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> VQ.ObjectSelectionSet
  -> m (tx EncJSON, HTTP.ResponseHeaders)
resolveMutSelSet env fields = do
  aliasedTxs <- traverseObjectSelectionSet fields $ \fld ->
    case VQ._fName fld of
      "__typename" -> return (return $ encJFromJValue mutationRootNamedType, [])
      _            -> evalReusabilityT $ GR.mutFldToTx env fld

  -- combines all transactions into a single transaction
  return (toSingleTx aliasedTxs, concatMap (snd . snd) aliasedTxs)
  where
    -- A list of aliased transactions for eg
    -- [("f1", Tx r1), ("f2", Tx r2)]
    -- are converted into a single transaction as follows
    -- Tx {"f1": r1, "f2": r2}
    -- toSingleTx :: [(Text, LazyRespTx)] -> LazyRespTx
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $ forM aliasedTxs $ \(al, (tx, _)) -> (,) al <$> tx

getMutOp
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> HTTP.Manager
  -> [HTTP.Header]
  -> VQ.ObjectSelectionSet
  -> m (tx EncJSON, HTTP.ResponseHeaders)
getMutOp env ctx sqlGenCtx userInfo manager reqHeaders selSet =
  peelReaderT $ resolveMutSelSet env selSet
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

getSubsOp
  :: ( MonadError QErr m
     , MonadIO m
     , HasVersion
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> PGExecCtx
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> QueryReusability
  -> QueryActionExecuter
  -> VQ.ObjectSelectionSet
  -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
getSubsOp env pgExecCtx gCtx sqlGenCtx userInfo queryReusability actionExecuter =
  runE gCtx sqlGenCtx userInfo .
  EL.buildLiveQueryPlan env pgExecCtx queryReusability actionExecuter

execRemoteGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader ExecutionCtx m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> RequestId
  -> UserInfo
  -> [HTTP.Header]
  -> GQLReqUnparsed
  -> RemoteSchemaInfo
  -> G.OperationType
  -> m (DiffTime, HttpResponse EncJSON)
  -- ^ Also returns time spent in http request, for telemetry.
execRemoteGQ env reqId userInfo reqHdrs q rsi opType = do
  execCtx <- ask
  let logger  = _ecxLogger execCtx
      manager = _ecxHttpManager execCtx
  logQueryLog logger q Nothing reqId
  (time, respHdrs, resp) <- execRemoteGQ' env manager userInfo reqHdrs q rsi opType
  let !httpResp = HttpResponse (encJFromLBS resp) respHdrs
  return (time, httpResp)
