{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE DataKinds                #-}
module Hasura.GraphQL.Execute
  ( QExecPlanResolved(..)
  , QExecPlanUnresolved(..)
  , QExecPlanPartial(..)
  , QExecPlan(..)
  , getExecPlanPartial
  , getOpTypeFromExecOp

  , ExecOp(..)
  , getExecPlan
  , execRemoteGQ

  , EP.PlanCache
  , EP.initPlanCache
  , EP.clearPlanCache
  , EP.dumpPlanCache

  , ExecutionCtx(..)

  , mkQuery
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Data.Has
import           Data.List
import           Data.List.NonEmpty                     (NonEmpty (..))
import           Hasura.GraphQL.Validate.Field

import qualified Data.Aeson                             as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Sequence                          as Seq
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Validate.Types
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId,
                                                         filterRequestHeaders)

import qualified Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Execute.Query           as EQ
import           Hasura.GraphQL.Execute.RemoteJoins
import qualified Hasura.GraphQL.Resolve                 as GR
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.Logging                         as L

data QExecPlanPartial
  = ExPHasuraPartial !(GCtx, VQ.HasuraTopField, [G.VariableDefinition])
  | ExPRemotePartial !VQ.RemoteTopField
--
-- The 'a' is parameterised so this AST can represent
-- intermediate passes
data QExecPlanCore a
  = ExPCoreHasura !a
  | ExPCoreRemote !RemoteSchemaInfo !G.TypedOperationDefinition
  deriving (Functor, Foldable, Traversable)

-- The current execution plan of a graphql operation, it is
-- currently, either local pg execution or a remote execution
data QExecPlanResolved
  = ExPHasura !ExecOp
  | ExPRemote !VQ.RemoteTopField
  -- | ExPMixed !ExecOp (NonEmpty RemoteRelBranch)
  deriving Show


-- | Split the 'rrSelSet' from the 'RemoteRelBranch'
mkQuery :: G.OperationType -> JoinArguments -> QExecPlanUnresolved -> (RemoteRelBranch 'RRF_Splice, VQ.RemoteTopField)
mkQuery rtqOperationType JoinArguments{..} QExecPlanUnresolved{..}  =
  let RemoteRelBranch{..} = remoteRelField
      indexedRows = enumerateRowAliases $ toList joinArguments
      rtqFields =
        flip map indexedRows $ \(alias, varArgs) ->
           fieldCallsToField
             rrArguments
             varArgs
             rrSelSet
             alias
             (rtrRemoteFields rrRemoteRelationship)
   in (rrFieldToSplice remoteRelField, VQ.RemoteTopField{..})


data QExecPlan = Leaf QExecPlanResolved | Tree QExecPlanResolved (NonEmpty QExecPlanUnresolved)
  deriving Show
data QExecPlanUnresolved 
  = QExecPlanUnresolved 
  { remoteRelField      :: RemoteRelBranch 'RRF_Tree
  , rtqRemoteSchemaInfo :: RemoteSchemaInfo
  } deriving Show

-- | Execution context
data ExecutionCtx
  = ExecutionCtx
  { _ecxLogger          :: !L.Logger
  , _ecxSqlGenCtx       :: !SQLGenCtx
  , _ecxPgExecCtx       :: !PGExecCtx
  , _ecxPlanCache       :: !EP.PlanCache
  , _ecxSchemaCache     :: !SchemaCache
  , _ecxSchemaCacheVer  :: !SchemaCacheVer
  , _ecxHttpManager     :: !HTTP.Manager
  , _ecxEnableAllowList :: !Bool
  }

getExecPlanPartial
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> Bool
  -> GQLReqParsed
  -> m (Seq.Seq QExecPlanPartial)
getExecPlanPartial UserInfo{userRole} sc enableAL req = do

  -- check if query is in allowlist
  when enableAL checkQueryInAllowlist

  (gCtx, _)  <- flip runStateT sc $ getGCtx userRole gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  topFields <- runReaderT (VQ.validateGQ queryParts) gCtx
  let varDefs = G._todVariableDefinitions $ VQ.qpOpDef queryParts
  return $
    fmap
      (\case
          VQ.HasuraLocatedTopField hasuraTopField ->
            ExPHasuraPartial (gCtx, hasuraTopField, varDefs)
          VQ.RemoteLocatedTopField remoteTopField -> ExPRemotePartial remoteTopField)
      topFields
  where
    gCtxRoleMap = scGCtxMap sc

    checkQueryInAllowlist =
      -- only for non-admin roles
      when (userRole /= adminRole) $ do
        let notInAllowlist =
              not $ VQ.isQueryInAllowlist (_grQuery req) (scAllowlist sc)
        when notInAllowlist $ modifyQErr modErr $ throwVE "query is not allowed"

    modErr e =
      let msg = "query is not in any of the allowlists"
      in e{qeInternal = Just $ J.object [ "message" J..= J.String msg]}


-- An execution operation, in case of queries and mutations it is just a
-- transaction to be executed
data ExecOp
  = ExOpQuery !LazyRespTx !(Maybe EQ.GeneratedSqlMap)
  | ExOpMutation !LazyRespTx
  | ExOpSubs !EL.LiveQueryOp
  deriving Show

getOpTypeFromExecOp :: ExecOp -> G.OperationType
getOpTypeFromExecOp = \case
  ExOpQuery _ _ -> G.OperationTypeQuery
  ExOpMutation _ -> G.OperationTypeMutation
  ExOpSubs _ -> G.OperationTypeSubscription

getExecPlan
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> SchemaCache
  -> SchemaCacheVer
  -> GQLReqUnparsed
  -> m (Seq.Seq QExecPlan)
getExecPlan pgExecCtx planCache userInfo@UserInfo{..} sqlGenCtx enableAL sc scVer reqUnparsed@GQLReq{..} = do
  liftIO (EP.getPlan scVer userRole _grOperationName _grQuery planCache) >>= \case
    Just plan ->
      -- pure $ pure $ Leaf (ExPHasura <$>
      case plan of
        -- plans are only for queries and subscriptions
        EP.RPQuery queryPlan -> do
          (tx, genSql) <- EQ.queryOpFromPlan userVars _grVariables queryPlan
          pure $ Seq.singleton (Leaf (ExPHasura (ExOpQuery tx (Just genSql))))
        EP.RPSubs subsPlan -> do
          liveQueryOp <- EL.subsOpFromPlan pgExecCtx userVars _grVariables subsPlan
          pure $ Seq.singleton (Leaf (ExPHasura (ExOpSubs liveQueryOp)))
    Nothing -> noExistingPlan
  where
    addPlanToCache plan =
      -- liftIO $
      EP.addPlan scVer userRole _grOperationName _grQuery plan planCache
    noExistingPlan = do
      req <- toParsed reqUnparsed
      partialExecPlans <- getExecPlanPartial userInfo sc enableAL req
      forM partialExecPlans $ \partialExecPlan ->
        case partialExecPlan of
          ExPRemotePartial r -> pure (Leaf $ ExPRemote r)
          ExPHasuraPartial (gCtx, rootSelSet, varDefs) -> do
            -- let runE' :: (MonadError QErr m) => E m a -> m a
            --     runE' action = do
            --       res <- runExceptT $ runReaderT action
            --         (userInfo, _gQueryCtxMap, _gTypes, _gFields, _gOrdByCtx, _gInsCtxMap)
            --       either throwError return res

                -- getQueryOp = runE' . EQ.convertQuerySelSet varDefs . pure

            case rootSelSet of
              VQ.HasuraTopMutation field ->
                Leaf . ExPHasura . ExOpMutation <$>
                (getMutOp gCtx sqlGenCtx userInfo (Seq.singleton field))

              VQ.HasuraTopQuery originalField -> do
                case rebuildFieldStrippingRemoteRels originalField of
                  Nothing -> do
                    (queryTx, _planM, genSql) <- getQueryOp gCtx sqlGenCtx
                                                 userInfo (Seq.singleton originalField) varDefs

                    -- TODO: How to cache query for each field?
                    -- mapM_ (addPlanToCache . EP.RPQuery) planM
                    pure $ Leaf . ExPHasura $ ExOpQuery queryTx (Just genSql)
                  Just (newHasuraField, remoteRelFields) -> do
                    -- trace
                    --   (unlines
                    --      [ "originalField = " ++ show originalField
                    --      , "newField = " ++ show newField
                    --      , "cursors = " ++ show (fmap rrRelFieldPath cursors)
                    --      ])
                    (queryTx, _planM, genSql) <- getQueryOp gCtx sqlGenCtx
                                                 userInfo (Seq.singleton newHasuraField) varDefs

                    Tree (ExPHasura $ ExOpQuery queryTx (Just genSql)) <$>
                      mkUnresolvedPlans remoteRelFields
              VQ.HasuraTopSubscription fld -> do
                (lqOp, _planM) <- getSubsOp pgExecCtx gCtx sqlGenCtx
                                  userInfo reqUnparsed varDefs fld

                -- TODO: How to cache query for each field?
                -- mapM_ (addPlanToCache . EP.RPSubs) planM
                pure $ Leaf . ExPHasura $ ExOpSubs lqOp

    mkUnresolvedPlans :: MonadError QErr m => NonEmpty (RemoteRelBranch 'RRF_Tree) -> m (NonEmpty QExecPlanUnresolved)
    mkUnresolvedPlans = traverse (\remoteRelField -> QExecPlanUnresolved remoteRelField <$> getRsi remoteRelField)
      where
        getRsi remoteRel =
          case Map.lookup
                 (rtrRemoteSchema
                    (rrRemoteRelationship remoteRel))
                 (scRemoteSchemas sc) of
            Just remoteSchemaCtx -> pure $ rscInfo remoteSchemaCtx
            Nothing -> throw500 "could not find remote schema info"

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
  -> VQ.SelSet
  -> [G.VariableDefinition]
  -> m (LazyRespTx, Maybe EQ.ReusableQueryPlan, EQ.GeneratedSqlMap)
getQueryOp gCtx sqlGenCtx userInfo fields varDefs =
  runE gCtx sqlGenCtx userInfo $ EQ.convertQuerySelSet varDefs fields

mutationRootName :: Text
mutationRootName = "mutation_root"

resolveMutSelSet
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has MutationCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     )
  => VQ.SelSet
  -> m LazyRespTx
resolveMutSelSet fields = do
  aliasedTxs <- forM (toList fields) $ \fld -> do
    fldRespTx <- case VQ._fName fld of
      "__typename" -> return $ return $ encJFromJValue mutationRootName
      _            -> liftTx <$> GR.mutFldToTx fld
    return (G.unName $ G.unAlias $ VQ._fAlias fld, fldRespTx)

  -- combines all transactions into a single transaction
  return $ toSingleTx aliasedTxs
  where
    -- A list of aliased transactions for eg
    -- [("f1", Tx r1), ("f2", Tx r2)]
    -- are converted into a single transaction as follows
    -- Tx {"f1": r1, "f2": r2}
    toSingleTx :: [(Text, LazyRespTx)] -> LazyRespTx
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $
        forM aliasedTxs sequence

getMutOp
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> VQ.SelSet
  -> m LazyRespTx
getMutOp ctx sqlGenCtx userInfo selSet =
  runE ctx sqlGenCtx userInfo $ resolveMutSelSet selSet

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
  -> GQLReqUnparsed
  -> [G.VariableDefinition]
  -> VQ.Field
  -> m (EL.LiveQueryOp, Maybe EL.SubsPlan)
getSubsOpM pgExecCtx req varDefs fld =
  case VQ._fName fld of
    "__typename" ->
      throwVE "you cannot create a subscription on '__typename' field"
    _            -> do
      astUnresolved <- GR.queryFldToPGAST fld
      EL.subsOpFromPGAST pgExecCtx req varDefs (VQ._fAlias fld, astUnresolved)

getSubsOp
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> GQLReqUnparsed
  -> [G.VariableDefinition]
  -> VQ.Field
  -> m (EL.LiveQueryOp, Maybe EL.SubsPlan)
getSubsOp pgExecCtx gCtx sqlGenCtx userInfo req varDefs fld =
  runE gCtx sqlGenCtx userInfo $ getSubsOpM pgExecCtx req varDefs fld

execRemoteGQ
  :: ( MonadIO m
     , MonadError QErr m
     , MonadReader ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> G.OperationType -- This should come from Field
  -> RemoteSchemaInfo
  -> Either GQLReqUnparsed [Field]
  -> m (HttpResponse EncJSON)
execRemoteGQ _reqId userInfo reqHdrs opType rsi bsOrField = do
  execCtx <- ask
  let _logger  = _ecxLogger execCtx
      manager = _ecxHttpManager execCtx
  hdrs <- getHeadersFromConf hdrConf
  let confHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
      clientHdrs = bool [] filteredHeaders fwdClientHdrs
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
  jsonbytes <-
    case bsOrField of
      Right field -> do
        gqlReq <- fieldsToRequest opType field
        let jsonbytes = encJToLBS (encJFromJValue gqlReq)
        pure jsonbytes
      Left unparsedQuery -> do
        -- liftIO $ logGraphqlQuery logger $ QueryLog unparsedQuery Nothing reqId
        pure (J.encode $ J.toJSON unparsedQuery)
  let req = initReq
           { HTTP.method = "POST"
           , HTTP.requestHeaders = finalHeaders
           , HTTP.requestBody = HTTP.RequestBodyLBS jsonbytes
           , HTTP.responseTimeout = HTTP.responseTimeoutMicro (timeout * 1000000)
           }

  -- TODO: Log here!!!
  -- liftIO $ logGraphqlQuery logger $ QueryLog q Nothing reqId
  res  <- liftIO $ try $ HTTP.httpLbs req manager
  resp <- either httpThrow return res
  let cookieHdrs = getCookieHdr (resp ^.. Wreq.responseHeader "Set-Cookie")
      respHdrs  = Just $ mkRespHeaders cookieHdrs
  return $ HttpResponse (encJFromLBS $ resp ^. Wreq.responseBody) respHdrs
  where
    RemoteSchemaInfo url hdrConf fwdClientHdrs timeout = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow = \case
      HTTP.HttpExceptionRequest _req content -> throw500 $ T.pack . show $ content
      HTTP.InvalidUrlException _url reason -> throw500 $ T.pack . show $ reason

    userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
                     userInfoToList userInfo
    filteredHeaders = filterUserVars $ filterRequestHeaders reqHdrs
    filterUserVars hdrs =
      let txHdrs = map (\(n, v) -> (bsToTxt $ CI.original n, bsToTxt v)) hdrs
      in map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
         filter (not . isUserVar . fst) txHdrs

    getCookieHdr = fmap (\h -> ("Set-Cookie", h))

    mkRespHeaders hdrs =
      map (\(k, v) -> Header (bsToTxt $ CI.original k, bsToTxt v)) hdrs
