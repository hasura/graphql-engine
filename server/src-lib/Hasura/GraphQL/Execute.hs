module Hasura.GraphQL.Execute
  ( GQExecPlan(..)

  -- , ExecPlanPartial
  -- , getExecPlanPartial

  , ExecOp(..)
  , ExecPlanResolved
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
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Data.Has

import qualified Data.Aeson                             as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId, mkClientHeadersForward)
import           Hasura.Server.Version                  (HasVersion)

import qualified Hasura.GraphQL.Context                 as C
import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Execute.Mutation        as EM
import qualified Hasura.GraphQL.Execute.Inline          as EI
import qualified Hasura.GraphQL.Parser.Schema           as PS
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem

-- The current execution plan of a graphql operation, it is
-- currently, either local pg execution or a remote execution
--
-- The 'a' is parameterised so this AST can represent
-- intermediate passes
data GQExecPlan a
  = GExPHasura !a
  | GExPRemote !RemoteSchemaInfo !(G.TypedOperationDefinition G.FragmentSpread G.Name)
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

-- -- Enforces the current limitation
-- assertSameLocationNodes
--   :: (MonadError QErr m) => [VT.TypeLoc] -> m VT.TypeLoc
-- assertSameLocationNodes typeLocs =
--   case Set.toList (Set.fromList typeLocs) of
--     -- this shouldn't happen
--     []    -> return VT.TLHasuraType
--     [loc] -> return loc
--     _     -> throw400 NotSupported msg
--   where
--     msg = "cannot mix top level fields from two different graphql servers"

-- TODO: we should fix this function asap
-- as this will fail when there is a fragment at the top level
getTopLevelNodes :: G.TypedOperationDefinition G.FragmentSpread G.Name -> [G.Name]
getTopLevelNodes opDef =
  mapMaybe f $ G._todSelectionSet opDef
  where
    f = \case
      G.SelectionField fld        -> Just $ G._fName fld
      G.SelectionFragmentSpread _ -> Nothing
      G.SelectionInlineFragment _ -> Nothing

-- gatherTypeLocs :: GCtx -> [G.Name] -> [VT.TypeLoc]
-- gatherTypeLocs gCtx nodes =
--   catMaybes $ flip map nodes $ \node ->
--     VT._fiLoc <$> Map.lookup node schemaNodes
--   where
--     schemaNodes =
--       let qr = VT._otiFields $ _gQueryRoot gCtx
--           mr = VT._otiFields <$> _gMutRoot gCtx
--       in maybe qr (Map.union qr) mr

-- This is for when the graphql query is validated
type ExecPlanPartial = GQExecPlan (C.GQLContext, G.TypedOperationDefinition G.FragmentSpread G.Name)

getTypedOp
  :: (MonadError QErr m)
  => Maybe OperationName
  -> [G.SelectionSet G.FragmentSpread G.Name]
  -> [G.TypedOperationDefinition G.FragmentSpread G.Name]
  -> m (G.TypedOperationDefinition G.FragmentSpread G.Name)
getTypedOp opNameM selSets opDefs =
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
      return $ G.TypedOperationDefinition
      G.OperationTypeQuery Nothing [] [] selSet
    (Nothing, [], [opDef])  ->
      return opDef
    (Nothing, _, _) ->
      _throwVE $ "exactly one operation has to be present " <>
      "in the document when operationName is not specified"

data QueryParts
  = QueryParts
  { qpOpDef    :: !(G.TypedOperationDefinition G.FragmentSpread G.Name)
  , qpVarValsM :: !(Maybe VariableValues)
  }

-- | Depending on the request parameters, fetch the correct typed operation
-- definition from the GraphQL query
getQueryParts
  :: ( MonadError QErr m )
  => GQLReqParsed
  -> m QueryParts
getQueryParts (GQLReq opNameM q varValsM) = do

  opDef <- getTypedOp opNameM selSets opDefs

  return $ QueryParts opDef varValsM
  where
    (selSets, opDefs, _fragDefsL) = G.partitionExDefs $ unGQLExecDoc q

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

getExecPlanPartial
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> Bool
  -> GQLReqParsed
  -> m ExecPlanPartial
getExecPlanPartial userInfo sc enableAL req = do

  -- check if query is in allowlist
  when enableAL checkQueryInAllowlist

  let gCtx = getGCtx sc role
  QueryParts tod _ <- flip runReaderT gCtx $ getQueryParts req

  let topLevelNodes = getTopLevelNodes tod
      -- -- gather TypeLoc of topLevelNodes
      -- typeLocs = gatherTypeLocs gCtx topLevelNodes

  -- -- see if they are all the same
  -- typeLoc <- assertSameLocationNodes typeLocs

  return $ GExPHasura (gCtx , tod)
  -- case typeLoc of
  --   VT.TLHasuraType -> do
  --     rootSelSet <- runReaderT (VQ.validateGQ queryParts) gCtx
  --     return $ GExPHasura (gCtx, rootSelSet)
  --   VT.TLRemoteType _ rsi ->
  --     _ -- return $ GExPRemote rsi opDef
  --   VT.TLCustom ->
  --     throw500 "unexpected custom type for top level field"
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


-- An execution operation, in case of
-- queries and mutations it is just a transaction
-- to be executed
data ExecOp
  = ExOpQuery !LazyRespTx !(Maybe EQ.GeneratedSqlMap)
  | ExOpMutation !LazyRespTx
  | ExOpSubs !EL.LiveQueryPlan

-- The graphql query is resolved into an execution operation
type ExecPlanResolved
  = GQExecPlan ExecOp

getResolvedExecPlan
  :: forall m . (HasVersion, MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> SchemaCache
  -> SchemaCacheVer
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> m (Telem.CacheHit, ExecPlanResolved)
getResolvedExecPlan pgExecCtx planCache userInfo sqlGenCtx
  enableAL sc scVer httpManager reqHeaders reqUnparsed = do
  planM <- liftIO $ EP.getPlan scVer (userRole userInfo)
           opNameM queryStr planCache
  let usrVars = userVars userInfo
  case planM of
    -- plans are only for queries and subscriptions
    Just plan -> (Telem.Hit,) . GExPHasura <$> case plan of
      EP.RPQuery queryPlan -> do
        (tx, genSql) <- EQ.queryOpFromPlan usrVars queryVars queryPlan
        return $ ExOpQuery tx (Just genSql)
      EP.RPSubs subsPlan ->
        ExOpSubs <$> EL.reuseLiveQueryPlan pgExecCtx usrVars queryVars subsPlan
    Nothing -> (Telem.Miss,) <$> noExistingPlan
  where
    GQLReq opNameM queryStr queryVars = reqUnparsed
    -- addPlanToCache plan =
    --   liftIO $ EP.addPlan scVer (userRole userInfo)
    --   opNameM queryStr plan planCache
    noExistingPlan :: m ExecPlanResolved
    noExistingPlan = do
      req <- toParsed reqUnparsed
      let
        takeFragment = \case G.ExecutableDefinitionFragment f -> Just f; _ -> Nothing
        fragments =
            mapMaybe takeFragment $ unGQLExecDoc $ _grQuery req
      planPartial <- getExecPlanPartial userInfo sc enableAL req
      case planPartial of
        GExPHasura (gCtx, G.TypedOperationDefinition G.OperationTypeQuery _ varDefs _ selSet) -> do
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          (queryTx, plan, genSql) <-
            getQueryOp gCtx sqlGenCtx userInfo inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
          return $ GExPHasura $ ExOpQuery queryTx (Just genSql)
        GExPHasura (gCtx, G.TypedOperationDefinition G.OperationTypeMutation _ varDefs _ selSet) -> do
          inlinedSelSet <- EI.inlineSelectionSet fragments selSet
          queryTx <- getMutOp gCtx sqlGenCtx userInfo inlinedSelSet varDefs (_grVariables reqUnparsed)
          -- traverse_ (addPlanToCache . EP.RPQuery) plan
          return $ GExPHasura $ ExOpMutation queryTx
        _ -> _
      -- forM partialExecPlan $ \(gCtx, rootSelSet) ->
      --   case rootSelSet of
      --     VQ.RMutation selSet ->
      --       ExOpMutation <$> getMutOp gCtx sqlGenCtx userInfo httpManager reqHeaders selSet
      --     VQ.RQuery selSet -> do
      --       (queryTx, plan, genSql) <- getQueryOp gCtx sqlGenCtx userInfo queryReusability selSet
      --       traverse_ (addPlanToCache . EP.RPQuery) plan
      --       return $ ExOpQuery queryTx (Just genSql)
      --     VQ.RSubscription fld -> do
      --       (lqOp, plan) <- getSubsOp pgExecCtx gCtx sqlGenCtx userInfo queryReusability fld
      --       traverse_ (addPlanToCache . EP.RPSubs) plan
      --       return $ ExOpSubs lqOp

-- Monad for resolving a hasura query/mutation
type E m =
  ReaderT ( UserInfo
          , SQLGenCtx
          ) (ExceptT QErr m)

runE
  :: (MonadError QErr m)
  => SQLGenCtx
  -> UserInfo
  -> E m a
  -> m a
runE sqlGenCtx userInfo action = do
  res <- runExceptT $ runReaderT action (userInfo, sqlGenCtx)
  either throwError return res

getQueryOp
  :: (MonadError QErr m)
  => C.GQLContext
  -> SQLGenCtx
  -> UserInfo
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe VariableValues
  -> m (LazyRespTx, Maybe EQ.ReusableQueryPlan, EQ.GeneratedSqlMap)
getQueryOp gqlContext sqlGenCtx userInfo fields varDefs varValsM =
  runE sqlGenCtx userInfo $ EQ.convertQuerySelSet gqlContext fields varDefs varValsM

mutationRootName :: Text
mutationRootName = "mutation_root"

getMutOp
  :: (HasVersion, MonadError QErr m, MonadIO m)
  => C.GQLContext
  -> SQLGenCtx
  -> UserInfo
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe VariableValues
  -> m LazyRespTx
getMutOp gqlContext sqlGenCtx userInfo fields varDefs varValsM =
  runE sqlGenCtx userInfo $ EM.convertMutationSelectionSet gqlContext fields varDefs varValsM
  where

-- getSubsOpM
--   :: ( MonadError QErr m
--      , MonadReader r m
--      , Has SQLGenCtx r
--      , Has UserInfo r
--      , MonadIO m
--      )
--   => PGExecCtx
--   -> G.Selection G.Name
--   -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
-- getSubsOpM pgExecCtx fld =
--   case VQ._fName fld of
--     "__typename" ->
--       _throwVE "you cannot create a subscription on '__typename' field"
--     _            -> do
--       (astUnresolved, finalReusability) <- runReusabilityTWith initialReusability $
--         GR.queryFldToPGAST fld
--       let varTypes = finalReusability ^? _Reusable
--       EL.buildLiveQueryPlan pgExecCtx (VQ._fAlias fld) astUnresolved varTypes
--
-- getSubsOp
--   :: ( MonadError QErr m
--      , MonadIO m
--      )
--   => PGExecCtx
--   -> SQLGenCtx
--   -> UserInfo
--   -> G.Selection G.Name
--   -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
-- getSubsOp pgExecCtx sqlGenCtx userInfo fld =
--   runE gCtx sqlGenCtx userInfo $ getSubsOpM pgExecCtx queryReusability fld

execRemoteGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLReqUnparsed
  -> RemoteSchemaInfo
  -> G.TypedOperationDefinition G.FragmentSpread G.Name
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
  let cookieHdrs = getCookieHdr (resp ^.. Wreq.responseHeader "Set-Cookie")
      respHdrs  = Just $ mkRespHeaders cookieHdrs
      !httpResp = HttpResponse (encJFromLBS $ resp ^. Wreq.responseBody) respHdrs
  return (time, httpResp)

  where
    RemoteSchemaInfo url hdrConf fwdClientHdrs timeout = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow = \case
      HTTP.HttpExceptionRequest _req content -> throw500 $ T.pack . show $ content
      HTTP.InvalidUrlException _url reason -> throw500 $ T.pack . show $ reason

    userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
                     userInfoToList userInfo

    getCookieHdr = fmap ("Set-Cookie",)

    mkRespHeaders = map (\(k, v) -> Header (bsToTxt $ CI.original k, bsToTxt v))
