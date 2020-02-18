{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Hasura.GraphQL.Execute
  ( GQExecPlanPartial
  , GQRemoteRelPlan(..)
  , GQExecPlan(..)

  , getExecPlanPartial

  , ExQueryField(..)
  , ExQueryOp(..)
  , ExecOp(..)
  , getResolvedExecPlan
  , execRemoteGQ
  , getSubsOp

  , EP.PlanCache
  , EP.mkPlanCacheOptions
  , EP.PlanCacheOptions
  , EP.initPlanCache
  , EP.clearPlanCache
  , EP.dumpPlanCache

  , ExecutionCtx(..)

  , mkQuery
  , fieldsToRequest
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Data.Has
import           Data.List.Extended                     (uniques)
import           Data.Time

import qualified Data.Aeson                             as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Sequence                          as Seq
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Data.UUID                              as UUID
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq


import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.RemoteJoins
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Types
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId, mkClientHeadersForward)
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.SQL.Time
import           Hasura.SQL.Value

import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Query           as EQ
import qualified Hasura.GraphQL.Resolve                 as GR
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem

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
-- Enforces the current limitation
assertSameLocationNodes
  :: (MonadError QErr m) => [VT.TypeLoc] -> m VT.TypeLoc
assertSameLocationNodes typeLocs =
  -- case Set.toList (Set.fromList typeLocs) of
  case uniques typeLocs of
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

data GQRemoteRelPlan (p :: RRF_P)
  = GQRemoteRelPlan
  { rrpRemoteRelField   :: RemoteRelBranch p
  , rrpRemoteSchemaInfo :: RemoteSchemaInfo
  }

deriving instance Show (GQRemoteRelPlan 'RRF_Tree)
deriving instance Show (GQRemoteRelPlan 'RRF_Splice)

data GQExecPlan a
  = GExPHasura !a
  | GExPRemote !RemoteSchemaInfo !G.TypedOperationDefinition
  deriving (Show, Eq, Functor, Foldable, Traversable)

type GQExecPlanPartial = GQExecPlan (GCtx, VQ.RootSelSet)

-- | Split the 'rrSelSet' from the 'RemoteRelBranch'
mkQuery :: GQRemoteRelPlan 'RRF_Tree -> JoinArguments -> (GQRemoteRelPlan 'RRF_Splice, [VQ.Field])
mkQuery GQRemoteRelPlan{..} JoinArguments{..} =
  let RemoteRelBranch{..} = rrpRemoteRelField
      indexedRows = enumerateRowAliases $ toList joinArguments
      batchFields =
        flip map indexedRows $ \(alias, varArgs) ->
           fieldCallsToField
             rrArguments
             varArgs
             rrSelSet
             alias
             (rtrRemoteFields rrRemoteRelationship)
   in (GQRemoteRelPlan (rrFieldToSplice rrpRemoteRelField) rrpRemoteSchemaInfo, batchFields)

getExecPlanPartial
  :: (MonadReusability m, MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> Bool
  -> GQLReqParsed
  -> m GQExecPlanPartial
getExecPlanPartial userInfo sc enableAL req = do
  -- check if query is in allowlist
  when enableAL checkQueryInAllowlist

  gCtx <- flip runCacheRT sc $ getGCtx role gCtxRoleMap
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
    VT.TLRemoteType _ rsi  ->
      pure $ GExPRemote rsi opDef
    VT.TLRemoteRelType _ ->
      throw500 "unexpected remote relationships for top level field"
    VT.TLCustom ->
      throw500 "unexpected custom type for top level field"
  where
    role = userRole userInfo
    gCtxRoleMap = scGCtxMap sc
    checkQueryInAllowlist =
      when (role /= adminRole) $ do
        let notInAllowlist =
              not $ VQ.isQueryInAllowlist (_grQuery req) (scAllowlist sc)
        when notInAllowlist $ modifyQErr modErr $ throwVE "query is not allowed"

    modErr e =
      let msg = "query is not in any of the allowlists"
       in e {qeInternal = Just $ J.object ["message" J..= J.String msg]}


-- An execution operation, in case of queries and mutations it is just a
data ExQueryField
  = EQFPlain !LazyRespTx
  | EQFRemoteJoin !(NE.NonEmpty (GQRemoteRelPlan 'RRF_Tree)) !LazyRespTx
  deriving Show

data ExQueryOp
  = EQOSimple !LazyRespTx
  | EQOComposite !(Seq.Seq ExQueryField)
  deriving Show

-- transaction to be executed
data ExecOp
  = ExOpQuery !ExQueryOp !(Maybe EQ.GeneratedSqlMap)
  | ExOpMutation !LazyRespTx
  | ExOpSubs !EL.LiveQueryPlan
  deriving Show

type GQExecPlanResolved = GQExecPlan ExecOp

getResolvedExecPlan
  :: forall m. (HasVersion, MonadError QErr m, MonadIO m)
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
  -> m (Telem.CacheHit,  GQExecPlanResolved)
getResolvedExecPlan pgExecCtx planCache userInfo sqlGenCtx
  enableAL sc scVer httpManager reqHeaders reqUnparsed
  = do
  planM <- liftIO $ EP.getPlan scVer (userRole userInfo)
           opNameM queryStr planCache
  let usrVars = userVars userInfo
  case planM of
    -- plans are only for queries and subscriptions
    Just plan -> (Telem.Hit,) . GExPHasura <$> case plan of
      EP.RPQuery queryPlan -> do
        (tx, genSql) <- EQ.queryOpFromPlan usrVars queryVars queryPlan
        pure $ ExOpQuery (EQOSimple tx) (Just genSql)
      EP.RPSubs subsPlan ->
        ExOpSubs <$> EL.reuseLiveQueryPlan pgExecCtx usrVars queryVars subsPlan
    Nothing -> (Telem.Miss,) <$> noExistingPlan
  where
    GQLReq opNameM queryStr queryVars = reqUnparsed
    addPlanToCache plan = liftIO $ EP.addPlan scVer (userRole userInfo) opNameM queryStr plan planCache

    noExistingPlan :: m GQExecPlanResolved
    noExistingPlan = do
      req <- toParsed reqUnparsed
      (partialExecPlan, queryReusability) <- runReusabilityT $ getExecPlanPartial userInfo sc enableAL req
      forM partialExecPlan $ \(gCtx, rootSelSet) ->
        case rootSelSet of
          VQ.RMutation selSet ->
            ExOpMutation <$> getMutOp gCtx sqlGenCtx userInfo httpManager reqHeaders selSet
          VQ.RQuery selSet -> do
            let resolvedFieldSet = flip map (toList selSet) $ \field ->
                  let (newField, remoteRelFields) = rebuildFieldStrippingRemoteRels field
                  in (newField, NE.nonEmpty remoteRelFields ,field)

            if all (isNothing . (^. _2)) resolvedFieldSet then do
              (queryTx, plan, genSql) <- getQueryOp gCtx sqlGenCtx userInfo queryReusability selSet
              traverse_ (addPlanToCache . EP.RPQuery) plan
              pure $ ExOpQuery (EQOSimple queryTx) (Just genSql)
            else do
                (resolvedFields, sqlMaps) <- fmap unzip $ forM resolvedFieldSet $ \(newField, maybeRemoteRels, _) -> do
                  (queryTx, _, genSql) <- getQueryOp gCtx sqlGenCtx userInfo queryReusability $ Seq.singleton newField
                  case maybeRemoteRels of
                    Nothing -> pure (EQFPlain queryTx, genSql)
                    Just remoteRels -> do
                      remoteRelsResolved <- mkRemoteRelPlans remoteRels
                      pure (EQFRemoteJoin remoteRelsResolved queryTx, genSql)
                pure $ ExOpQuery (EQOComposite $ Seq.fromList resolvedFields) (Just $ mconcat sqlMaps)
          VQ.RSubscription fld -> do
            (lqOp, plan) <- getSubsOp pgExecCtx gCtx sqlGenCtx userInfo queryReusability fld
            traverse_ (addPlanToCache . EP.RPSubs) plan
            pure $ ExOpSubs lqOp

    mkRemoteRelPlans :: NE.NonEmpty (RemoteRelBranch 'RRF_Tree) -> m (NE.NonEmpty (GQRemoteRelPlan 'RRF_Tree))
    mkRemoteRelPlans = traverse (\remoteRelField -> GQRemoteRelPlan remoteRelField <$> getRsi remoteRelField)
      where
        getRsi remoteRel =
          case Map.lookup
                 (rtrRemoteSchema
                    (rrRemoteRelationship remoteRel))
                 (scRemoteSchemas sc) of
            Just remoteSchemaCtx -> pure $ rscInfo remoteSchemaCtx
            Nothing              -> throw500 "could not find remote schema info"

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
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> QueryReusability
  -> VQ.SelSet
  -> m (LazyRespTx, Maybe EQ.ReusableQueryPlan, EQ.GeneratedSqlMap)
getQueryOp gCtx sqlGenCtx userInfo queryReusability fields =
  runE gCtx sqlGenCtx userInfo $ EQ.convertQuerySelSet queryReusability fields

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
  -> m LazyRespTx
resolveMutSelSet fields = do
  aliasedTxs <- forM (toList fields) $ \fld -> do
    fldRespTx <- case VQ._fName fld of
      "__typename" -> return $ return $ encJFromJValue mutationRootName
      _            -> fmap liftTx . evalReusabilityT $ GR.mutFldToTx fld
    return (G.unName $ G.unAlias $ VQ._fAlias fld, fldRespTx)

  -- combines all transactions into a single transaction
  return $ liftTx $ toSingleTx aliasedTxs
  where
    toSingleTx aliasedTxs = fmap encJFromAssocList $ forM aliasedTxs sequence

getMutOp
  :: (HasVersion, MonadError QErr m, MonadIO m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> HTTP.Manager
  -> [N.Header]
  -> VQ.SelSet
  -> m LazyRespTx
getMutOp ctx sqlGenCtx userInfo manager reqHeaders selSet =
  runE_ $ resolveMutSelSet selSet
  where
    runE_ action = do
      res <- runExceptT $ runReaderT action
        ( userInfo, queryCtxMap, mutationCtxMap
        , typeMap, fldMap, ordByCtx, insCtxMap, sqlGenCtx
        , manager, reqHeaders
        )
      either throwError return res
      where
        queryCtxMap = _gQueryCtxMap ctx
        mutationCtxMap = _gMutationCtxMap ctx
        typeMap = _gTypes ctx
        fldMap = _gFields ctx
        ordByCtx = _gOrdByCtx ctx
        insCtxMap = _gInsCtxMap ctx

getSubsOpM
  :: ( MonadError QErr m
     , MonadReader r m
     , Has QueryCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     , MonadIO m
     )
  => PGExecCtx
  -> QueryReusability
  -> VQ.Field
  -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
getSubsOpM pgExecCtx initialReusability fld =
  case VQ._fName fld of
    "__typename" ->
      throwVE "you cannot create a subscription on '__typename' field"
    _            -> do
      (astUnresolved, finalReusability) <- runReusabilityTWith initialReusability $
        GR.queryFldToPGAST fld
      let varTypes = finalReusability ^? _Reusable
      EL.buildLiveQueryPlan pgExecCtx (VQ._fAlias fld) astUnresolved varTypes

getSubsOp
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> QueryReusability
  -> VQ.Field
  -> m (EL.LiveQueryPlan, Maybe EL.ReusableLiveQueryPlan)
getSubsOp pgExecCtx gCtx sqlGenCtx userInfo queryReusability fld =
  runE gCtx sqlGenCtx userInfo $ getSubsOpM pgExecCtx queryReusability fld

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
  -> G.OperationType
  -> m (DiffTime, HttpResponse EncJSON)
  -- ^ Also returns time spent in http request, for telemetry.
execRemoteGQ reqId userInfo reqHdrs q rsi opType = do
  execCtx <- ask
  let logger  = _ecxLogger execCtx
      manager = _ecxHttpManager execCtx
  when (opType == G.OperationTypeSubscription) $
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

    getCookieHdr = fmap (\h -> ("Set-Cookie", h))

    mkRespHeaders = map (\(k, v) -> Header (bsToTxt $ CI.original k, bsToTxt v))

fieldsToRequest
  :: (MonadIO m, MonadError QErr m)
  => G.OperationType
  -> [VQ.Field]
  -> m GQLReqParsed
fieldsToRequest opType fields = do
  case traverse fieldToField fields of
    Right gfields ->
      pure
        (GQLReq
           { _grOperationName = Nothing
           , _grQuery =
               GQLExecDoc
                 [ G.ExecutableDefinitionOperation
                     (G.OperationDefinitionTyped
                       (emptyOperationDefinition {
                         G._todSelectionSet = (map G.SelectionField gfields)
                         } )
                       )
                 ]
           , _grVariables = Nothing -- TODO: Put variables in here?
           })
    Left err -> throw500 ("While converting remote field: " <> err)
    where
      emptyOperationDefinition =
        G.TypedOperationDefinition {
          G._todType = opType
        , G._todName = Nothing
        , G._todVariableDefinitions = []
        , G._todDirectives = []
        , G._todSelectionSet = [] }

fieldToField :: VQ.Field -> Either Text G.Field
fieldToField VQ.Field{..} = do
  _fArguments <- traverse makeArgument (Map.toList _fArguments)
  _fSelectionSet <- fmap G.SelectionField . toList <$>
    traverse fieldToField _fSelSet
  _fDirectives <- pure []
  _fAlias      <- pure (Just _fAlias)
  pure G.Field{..}

makeArgument :: (G.Name, AnnInpVal) -> Either Text G.Argument
makeArgument (_aName, annInpVal) =
  do _aValue <- annInpValToValue annInpVal
     pure $ G.Argument {..}

annInpValToValue :: AnnInpVal -> Either Text G.Value
annInpValToValue = annGValueToValue . _aivValue

annGValueToValue :: AnnGValue -> Either Text G.Value
annGValueToValue = fromMaybe (pure G.VNull) .
  \case
    AGScalar _ty mv ->
      pgcolvalueToGValue <$> mv
    AGEnum _ _enumVal ->
      pure (Left "enum not supported")
    AGObject _ mobj ->
      flip fmap mobj $ \obj -> do
        fields <-
          traverse
            (\(_ofName, av) -> do
               _ofValue <- annInpValToValue av
               pure (G.ObjectFieldG {..}))
            (OMap.toList obj)
        pure (G.VObject (G.ObjectValueG fields))
    AGArray _ mvs ->
      fmap (G.VList . G.ListValueG) . traverse annInpValToValue <$> mvs

pgcolvalueToGValue :: PGScalarValue -> Either Text G.Value
pgcolvalueToGValue colVal = case colVal of
  PGValInteger i  -> pure $ G.VInt $ fromIntegral i
  PGValSmallInt i -> pure $ G.VInt $ fromIntegral i
  PGValBigInt i   -> pure $ G.VInt $ fromIntegral i
  PGValFloat f    -> pure $ G.VFloat $ realToFrac f
  PGValDouble d   -> pure $ G.VFloat $ realToFrac d
  -- TODO: Scientific is a danger zone; use its safe conv function.
  PGValNumeric sc -> pure $ G.VFloat $ realToFrac sc
  PGValBoolean b  -> pure $ G.VBoolean b
  PGValChar t     -> pure $ G.VString (G.StringValue (T.singleton t))
  PGValVarchar t  -> pure $ G.VString (G.StringValue t)
  PGValText t     -> pure $ G.VString (G.StringValue t)
  PGValCitext t   -> pure $ G.VString (G.StringValue t)
  PGValDate d     -> pure $ G.VString $ G.StringValue $ T.pack $ showGregorian d
  PGValTimeStampTZ u -> pure $
    G.VString $ G.StringValue $   T.pack $ formatTime defaultTimeLocale "%FT%T%QZ" u
  PGValTimeTZ (ZonedTimeOfDay tod tz) -> pure $
    G.VString $ G.StringValue $   T.pack (show tod ++ timeZoneOffsetString tz)
  PGNull _ -> pure G.VNull
  PGValJSON {}    -> Left "PGValJSON: cannot convert"
  PGValJSONB {}  -> Left "PGValJSONB: cannot convert"
  PGValGeo {}    -> Left "PGValGeo: cannot convert"
  PGValRaster {} -> Left "PGValRaster: cannot convert"
  PGValUUID u    -> pure $ G.VString (G.StringValue $ UUID.toText u)
  PGValUnknown t -> pure $ G.VString $ G.StringValue t
