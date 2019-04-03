{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , getExecPlan
  , execRemoteGQ
  , transformGQRequest
  ) where

import           Control.Exception                      (try)
import           Control.Lens                           hiding (op)

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

import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT

data GQExecPlan
  = GExPHasura !GCtx !VQ.RootSelSet
  | GExPRemote !RemoteSchemaInfo !G.TypedOperationDefinition
  | GExPMixed  ![GQExecPlan]


type QueryMap = Map.HashMap VT.TypeLoc [G.Name]

getExecPlan
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> GraphQLRequest
  -> m GQExecPlan
  -- -> m (GQSelSet a)
getExecPlan userInfo sc req = do

  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  let opDef = VQ.qpOpDef queryParts
      topLevelNodes = getTopLevelNodes opDef
      -- gather TypeLoc of topLevelNodes
      typeLocs = gatherTypeLocs gCtx topLevelNodes
      typeLocWTypes = zip typeLocs topLevelNodes
      queryMap = foldr upsertType mempty typeLocWTypes
      queries = splitQuery opDef queryMap

  -- traceM "OPERATION DEF =============>>>>"
  -- liftIO $ pPrint opDef
  -- traceM "SPLITTED QUERY ===============>>>>>>"
  -- liftIO $ pPrint queries
  mkExecPlan gCtx queries
  where
    mkExecPlan gCtx queries = case queries of
      [(VT.HasuraType, opDef')] -> do
        let newQ = transformGQRequest req opDef'
        queryParts <- flip runReaderT gCtx $ VQ.getQueryParts newQ
        GExPHasura gCtx <$> runReaderT (VQ.validateGQ queryParts) gCtx
      [(VT.RemoteType _ rsi, opDef')] ->
        return $ GExPRemote rsi opDef'
      xs -> GExPMixed <$> mapM (\x -> mkExecPlan gCtx [x]) xs

    gCtxRoleMap = scGCtxMap sc

    upsertType :: (VT.TypeLoc, G.Name) -> QueryMap -> QueryMap
    upsertType (tl, n) qMap = Map.alter (Just . maybe [n] (n:)) tl qMap

    splitQuery
      :: G.TypedOperationDefinition -> QueryMap
      -> [(VT.TypeLoc , G.TypedOperationDefinition)]
    splitQuery opDef qMap =
      flip map (Map.toList qMap) $ \(tl, tyNames) ->
        let (flds, vars) = pickFields opDef tyNames $ G._todSelectionSet opDef
        in (tl, mkNewOpDef opDef flds vars)

    pickFields opDef fldNames selSet =
      let flds = filter filterSel selSet
          vars = filterVars flds $ G._todVariableDefinitions opDef
      in (flds, vars)
      where
        filterSel sel = case sel of
          G.SelectionField fld -> G._fName fld `elem` fldNames
          _                    -> False


    filterVars selSet vars =
      let args = join $ map getArgs selSet
          filterVar var = G._vdVariable var `elem` join (map getVars args)
      in filter filterVar vars

    getArgs :: G.Selection -> [G.Argument]
    getArgs sel = case sel of
      G.SelectionField fld -> G._fArguments fld
      _                    -> []

    getVars :: G.Argument -> [G.Variable]
    getVars arg = getVarFromVal $ G._aValue arg

    getVarFromVal :: G.Value -> [G.Variable]
    getVarFromVal val = case val of
      G.VVariable v -> [v]
      G.VObject o   ->
        let xs = map G._ofValue $ G.unObjectValue o
        in join $ map getVarFromVal xs
      _ -> []

    mkNewOpDef opDef selset varDefs =
      opDef { G._todSelectionSet = selset
            , G._todVariableDefinitions = varDefs
            }

transformGQRequest
  :: GraphQLRequest -> G.TypedOperationDefinition -> GraphQLRequest
transformGQRequest (GraphQLRequest opNameM _ varValsM) opDef =
  let q = GraphQLQuery [G.ExecutableDefinitionOperation $
                        G.OperationDefinitionTyped opDef]
      vars = map G._vdVariable $ G._todVariableDefinitions opDef
      varValsFltrd = Map.filterWithKey (\k _ -> k `elem` vars) <$> varValsM
  in GraphQLRequest opNameM q varValsFltrd


execRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> UserInfo
  -> [N.Header]
  -> GraphQLRequest
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

  res  <- liftIO $ try $ Wreq.postWith options (show url) (encJFromJValue q)
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

assertSameLocationNodes :: (MonadError QErr m) => [VT.TypeLoc] -> m VT.TypeLoc
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
