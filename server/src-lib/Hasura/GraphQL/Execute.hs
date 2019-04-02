module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , ExecPlanResolved
  , ResolvedOp(..)
  , ExecPlanUnresolved

  , getExecPlan
  , getResolvedExecPlan
  , execRemoteGQ

  , EP.PlanCache
  , EP.initPlanCache
  , EP.clearPlanCache
  , EP.dumpPlanCache

  , module EL

  ) where

import           Control.Exception                      (try)
import           Control.Lens

import qualified Data.ByteString.Lazy                   as BL
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
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute.Context         as EC
import           Hasura.GraphQL.Execute.LiveQuery       as EL
import qualified Hasura.GraphQL.Execute.Plan            as EP
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT

data GQExecPlan a
  = GExPHasura !a
  | GExPRemote !RemoteSchemaInfo !G.TypedOperationDefinition
  deriving (Functor, Foldable, Traversable)

type ExecPlanUnresolved
  = GQExecPlan (GCtx, VQ.RootSelSet, [G.VariableDefinition])

data ResolvedOp
  = ROQuery !LazyRespTx
  | ROMutation !LazyRespTx
  | ROSubs !EL.LiveQueryOp

type ExecPlanResolved
  = GQExecPlan ResolvedOp

getExecPlan
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> GQLReqParsed
  -> m ExecPlanUnresolved
getExecPlan userInfo sc req = do

  (gCtx, _)  <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  let opDef = VQ.qpOpDef queryParts
      topLevelNodes = getTopLevelNodes opDef
      -- gather TypeLoc of topLevelNodes
      typeLocs = gatherTypeLocs gCtx topLevelNodes

  -- see if they are all the same
  typeLoc <- assertSameLocationNodes typeLocs

  case typeLoc of
    VT.HasuraType -> do
      rootSelSet <- runReaderT (VQ.validateGQ queryParts) gCtx
      let varDefs = G._todVariableDefinitions $ VQ.qpOpDef queryParts
      return $ GExPHasura (gCtx, rootSelSet, varDefs)
    VT.RemoteType _ rsi ->
      return $ GExPRemote rsi opDef
  where
    gCtxRoleMap = scGCtxMap sc

getResolvedExecPlan
  :: (MonadError QErr m, MonadIO m)
  => EP.PlanCache
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> SchemaCacheVer
  -> GQLReqUnparsed
  -> m ExecPlanResolved
getResolvedExecPlan planCache userInfo sqlGenCtx sc scVer reqUnparsed = do
  planM <- liftIO $ EP.getPlan scVer (userRole userInfo)
                opNameM queryStr planCache
  case planM of
    -- plans are only for queries and subscriptions
    Just plan ->
      GExPHasura <$> case plan of
        EP.RPQuery queryPlan -> ROQuery <$> EP.mkNewQueryTx queryVars queryPlan
        EP.RPSubs subsPlan -> ROSubs <$> EP.mkSubsOp queryVars subsPlan
    Nothing -> noExistingPlan
  where
    GQLReq opNameM queryStr queryVars = reqUnparsed
    noExistingPlan = do
      req      <- toParsed reqUnparsed
      execPlan <- getExecPlan userInfo sc req
      forM execPlan $ \(gCtx, rootSelSet, varDefs) ->
        case rootSelSet of
          VQ.RMutation selSet ->
            ROMutation <$> EC.getMutTx gCtx sqlGenCtx userInfo selSet
          VQ.RQuery selSet -> do
            (queryTx, planM) <- EC.getQueryTx gCtx sqlGenCtx
                                userInfo selSet varDefs
            forM_ planM $ \plan ->
              liftIO $ EP.addPlan scVer (userRole userInfo)
              opNameM queryStr (EP.RPQuery plan) planCache
            return $ ROQuery queryTx
          VQ.RSubscription fld -> do
            (lqOp, planM) <- EC.getSubsOp gCtx sqlGenCtx
                             userInfo fld varDefs
            forM_ planM $ \plan ->
              liftIO $ EP.addPlan scVer (userRole userInfo)
              opNameM queryStr (EP.RPSubs plan) planCache
            return $ ROSubs lqOp

execRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> UserInfo
  -> [N.Header]
  -> BL.ByteString
  -- ^ the raw request string
  -> RemoteSchemaInfo
  -> G.TypedOperationDefinition
  -> m EncJSON
execRemoteGQ manager userInfo reqHdrs q rsi opDef = do
  let opTy = G._todType opDef
  when (opTy == G.OperationTypeSubscription) $
    throw400 NotSupported "subscription to remote server is not supported"
  hdrs <- getHeadersFromConf hdrConf
  let confHdrs   = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
      clientHdrs = bool [] filteredHeaders fwdClientHdrs
      options    = wreqOptions manager (userInfoToHdrs ++ clientHdrs ++ confHdrs)

  res  <- liftIO $ try $ Wreq.postWith options (show url) q
  resp <- either httpThrow return res
  return $ encJFromLBS $ resp ^. Wreq.responseBody

  where
    RemoteSchemaInfo url hdrConf fwdClientHdrs = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow err = throw500 $ T.pack . show $ err

    userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
                 userInfoToList userInfo
    filteredHeaders = flip filter reqHdrs $ \(n, _) ->
      n `notElem` [ "Content-Length", "Content-MD5", "User-Agent", "Host"
                  , "Origin", "Referer" , "Accept", "Accept-Encoding"
                  , "Accept-Language", "Accept-Datetime"
                  , "Cache-Control", "Connection", "DNT"
                  ]

assertSameLocationNodes
  :: (MonadError QErr m) => [VT.TypeLoc] -> m VT.TypeLoc
assertSameLocationNodes typeLocs =
  case Set.toList (Set.fromList typeLocs) of
    -- this shouldn't happen
    []    -> return VT.HasuraType
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
