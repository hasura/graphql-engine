{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , GExPHasura(..)
  , GExPRemote(..)
  , getExecPlan
  , execRemoteGQ
  , transformGQRequest
  ) where

import           Debug.Trace

import           Control.Exception                      (try)
import           Control.Lens                           hiding (op)
import           Data.List                              (nub)

import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
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

data GExPHasura = GExPHasura !GCtx !VQ.RootSelSet (Maybe [GExPRemote])
  deriving (Show, Eq)

data GExPRemote = GExPRemote !RemoteSchemaInfo !GraphQLRequest !VQ.RootSelSet
  deriving (Show, Eq)

data GQExecPlan = GQExecPlan (Maybe GExPHasura) [GExPRemote]
  deriving (Show, Eq)

type QueryMap = Map.HashMap VT.TypeLoc [G.Name]
type SplitQueryParts = (G.TypedOperationDefinition, [G.FragmentDefinition])

getExecPlan
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> GraphQLRequest
  -> m GQExecPlan
getExecPlan userInfo sc req = do
  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req
  rootSelSet <- runReaderT (VQ.validateGQ queryParts) gCtx
  let opDef = VQ.qpOpDef queryParts
      fragDefs = VQ.qpFragDefsL queryParts
      topLevelNodes = getTopLevelNodes opDef
      typeLocs = gatherTypeLocs gCtx topLevelNodes
      typeLocWTypes = zip typeLocs topLevelNodes
      queryMap = foldr upsertType mempty typeLocWTypes
      querySplits = splitQuery opDef fragDefs queryMap
      newQs = map (\(tl, qp) -> (,) tl $ uncurry (transformGQRequest req) qp)
              querySplits
      typelocstemp = traceShow (map fst newQs)

  newRootSels <- forM newQs $ \(tl, q) ->
    flip runReaderT gCtx $ do
      p <- VQ.getQueryParts q
      (,) tl <$> VQ.validateGQ p

  let newReqs = zipWith (\(tl,q) (_,rs) -> (tl, (q,rs))) newQs newRootSels

  case newReqs of
    -- this shouldn't happen
    [] ->
      return $ GQExecPlan Nothing []
    (x: []) -> case x of
      (VT.HasuraType, (reqh, rootSelSeth)) -> do
        -- let remotePlans = getRemotePlans rootSelSet
        --     hasuraSelSet = removeRemoteFields rootSelSet
        let remotePlans = []
            hasuraSelSet = rootSelSeth
        return $ GQExecPlan (Just $ GExPHasura gCtx hasuraSelSet (toMaybe remotePlans) ) []
      (VT.RemoteType _ rsi, _) -> return $ GQExecPlan Nothing [GExPRemote rsi req rootSelSet]
    -- (x:y:[]) -> case (x,y) of
    --   ( (VT.HasuraType, (newq, rootSelSet)), (VT.RemoteType _ rsi dep, (newq, rs)) ) -> do
    --     let remotePlans = getRemotePlans rootSelSet
    --         hasuraSelSet = removeRemoteFields rootSelSet
    --         otherRemotePlans = GQExecPlan Nothing [(GExPRemote rsi req rootSelSet dep)]
    --     qParts <- flip runReaderT gCtx $ VQ.getQueryParts newq
    --     return $ GQExecPlan (Just $ GExPHasura gCtx <$> runReaderT (VQ.validateGQ qParts) gCtx)
      _ -> throw500 "unexpected plan"
  where
    toMaybe [] = Nothing
    toMaybe x  =  Just x

    -- combPlan :: GQExecPlan -> Either GExPHasura GExPRemote -> m GQExecPlan
    -- combPlan (GQExecPlan initHasuraPlan initRemPlans )(Left hasuraPlan) = case initHasuraPlan of
    --   Nothing -> return $ GQExecPlan (Just hasuraPlan) initRemPlans
    --   Just plan  -> if plan == hasuraPlan
    --     then return $ GQExecPlan (Just hasuraPlan) initRemPlans
    --     else throw500 "cannot combine multiple hasura plans"
    -- combPlan (GQExecPlan initHasuraPlan initRemPlans )(Right remPlan) = return $ GQExecPlan initHasuraPlan (initRemPlans ++ [remPlan])

    gCtxRoleMap = scGCtxMap sc

    upsertType :: (VT.TypeLoc, G.Name) -> QueryMap -> QueryMap
    upsertType (tl, n) qMap = Map.alter (Just . maybe [n] (n:)) tl qMap

    splitQuery
      :: G.TypedOperationDefinition -> [G.FragmentDefinition] -> QueryMap
      -> [(VT.TypeLoc, SplitQueryParts)]
    splitQuery opDef fragDefs qMap =
      flip map (Map.toList qMap) $ \(tl, tyNames) ->
        let selSet = G._todSelectionSet opDef
            (flds, vars) = pickFields opDef tyNames selSet
            frags = pickFrags flds fragDefs
        in (tl, (mkNewOpDef opDef flds vars, frags))

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
      G.SelectionField fld -> G._fArguments fld ++
                              join (map getArgs $ G._fSelectionSet fld)
      _                    -> []

    getVars :: G.Argument -> [G.Variable]
    getVars arg = getVarFromVal $ G._aValue arg

    getVarFromVal :: G.Value -> [G.Variable]
    getVarFromVal val = case val of
      G.VVariable v -> [v]
      G.VList l -> case G.unListValue l of
        [] -> []
        xs -> join $ map getVarFromVal xs
      G.VObject o   ->
        let xs = map G._ofValue $ G.unObjectValue o
        in join $ map getVarFromVal xs
      _ -> []

    pickFrags selSet fragDefs =
      let perSel sel = case sel of
            G.SelectionField fld ->
              join $ map perSel $ G._fSelectionSet fld
            G.SelectionFragmentSpread fs  -> [G._fsName fs]
            G.SelectionInlineFragment ilf ->
              join $ map perSel $ G._ifSelectionSet ilf

          fragNames = join $ map perSel selSet
          curFrags = filter (\fd -> G._fdName fd `elem` fragNames) fragDefs

          otherFragNames = join . join $ map (map perSel . G._fdSelectionSet)
                           curFrags
          otherFrags = filter (\fd -> G._fdName fd `elem` otherFragNames) fragDefs

      in nub $ curFrags ++ otherFrags

    mkNewOpDef opDef selset varDefs =
      opDef { G._todSelectionSet = selset
            , G._todVariableDefinitions = varDefs
            }


getRemotePlans = undefined
removeRemoteFields = undefined

transformGQRequest
  :: GraphQLRequest
  -> G.TypedOperationDefinition
  -> [G.FragmentDefinition]
  -> GraphQLRequest
transformGQRequest (GraphQLRequest opNameM _ varValsM) opDef fragDefs =
  let op = G.ExecutableDefinitionOperation $ G.OperationDefinitionTyped opDef
      frags = map G.ExecutableDefinitionFragment fragDefs
      exDefs = op:frags
      q = GraphQLQuery exDefs
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
  -> VQ.RootSelSet
  -> m EncJSON
execRemoteGQ manager userInfo reqHdrs q rsi rSelSet = case rSelSet of
  VQ.RSubscription _ ->
    throw400 NotSupported "subscription to remote server is not supported"
  _ -> do
    hdrs <- getHeadersFromConf hdrConf
    let confHdrs   = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
        clientHdrs = bool [] filteredHeaders fwdClientHdrs
        options    = wreqOptions manager (userInfoToHdrs ++ clientHdrs ++
                                          confHdrs)

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
