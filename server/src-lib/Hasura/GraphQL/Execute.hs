module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , GExPHasura(..)
  , GExPRemote(..)
  , getExecPlan
  , execRemoteGQ
  , transformGQRequest
  ) where

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


data GExPHasura
  = GExPHasura !GCtx !VQ.RootSelSet
  deriving (Show, Eq)

data GExPRemote
  = GExPRemote !RemoteSchemaInfo !GraphQLRequest !VQ.RootSelSet
  deriving (Show, Eq)

data GQExecPlan
  = GQExecPlan (Maybe GExPHasura) [GExPRemote]
  deriving (Show, Eq)

data QueryParts
  = QueryParts
  { _qpOpDef     :: !G.TypedOperationDefinition
  , _qpFragDefsL :: ![G.FragmentDefinition]
  --, _qpVarValsM  :: !(Maybe VariableValues)
  } deriving (Show, Eq)

type SplitSelSets    = Map.HashMap VT.TypeLoc G.SelectionSet
type SplitQueryParts = Map.HashMap VT.TypeLoc QueryParts


getExecPlan
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> GraphQLRequest
  -> m GQExecPlan
getExecPlan userInfo sc req = do
  (gCtx, _)  <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  let opDef = VQ.qpOpDef queryParts
      fragDefs = VQ.qpFragDefsL queryParts
      ssSplits = foldr (splitSelSet gCtx) mempty $ G._todSelectionSet opDef
      querySplits = splitQuery opDef fragDefs ssSplits
      newQs = Map.map (transformGQRequest req) querySplits

  newRootSels <- forM (Map.toList newQs) $ \(tl, q) ->
    flip runReaderT gCtx $
      (,) tl <$> (VQ.getQueryParts q >>= VQ.validateGQ)

  let newReqs = zipWith (\(tl,q) (_,rs) -> (tl, (q,rs))) (Map.toList newQs) newRootSels

  case newReqs of
    -- this shouldn't happen
    [] -> throw500 "unexpected plan"

    [(VT.HasuraType, (_, rootSelSet))] ->
      return $ GQExecPlan (Just $ GExPHasura gCtx rootSelSet) []
    [(VT.RemoteType _ rsi, (_, rootSelSet))] ->
      return $ GQExecPlan Nothing [GExPRemote rsi req rootSelSet]
    xs -> foldM (combPlan gCtx) (GQExecPlan Nothing []) xs

  where
    gCtxRoleMap = scGCtxMap sc

    combPlan gCtx plan (tl, (newq, rs)) = case tl of
      VT.HasuraType -> do
        let GQExecPlan hPlan remPlans = plan
        case hPlan of
          Nothing -> return $ GQExecPlan (Just $ GExPHasura gCtx rs) remPlans
          Just _  -> throw500 "encountered more than one hasura plan!"
      VT.RemoteType _ rsi -> do
        let GQExecPlan hPlan remPlans = plan
        return $ GQExecPlan hPlan (GExPRemote rsi newq rs:remPlans)


splitSelSet :: GCtx -> G.Selection -> SplitSelSets -> SplitSelSets
splitSelSet ctx sel theMap = case sel of
  s@(G.SelectionField fld) ->
    let loc = getTypeLoc ctx $ G._fName fld
    in maybe theMap (\l -> upsertType (l, s) theMap) loc
  -- TODO: what do we with fragments? we don't have any typeinfo on them
  _ -> theMap
  where
    upsertType (tl, n) qMap = Map.alter (Just . maybe [n] (n:)) tl qMap


splitQuery
  :: G.TypedOperationDefinition
  -> [G.FragmentDefinition]
  -> SplitSelSets
  -> SplitQueryParts
splitQuery opDef fragDefs qMap =
  flip Map.map qMap $ \selSet ->
    let frags = pickFrags selSet
        vars  = filterVars selSet $ G._todVariableDefinitions opDef
    in mkNewQParts frags selSet vars
  where
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

    pickFrags selSet =
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

    mkNewQParts frags selset varDefs =
      let od = opDef { G._todSelectionSet = selset
                     , G._todVariableDefinitions = varDefs
                     }
      in QueryParts od frags


transformGQRequest :: GraphQLRequest -> QueryParts -> GraphQLRequest
transformGQRequest (GraphQLRequest opNameM _ varValsM)
                   (QueryParts opDef fragDefs) =
  let op = G.ExecutableDefinitionOperation $ G.OperationDefinitionTyped opDef
      frags = map G.ExecutableDefinitionFragment fragDefs
      exDefs = op:frags
      q = GraphQLQuery exDefs
      -- TODO: do this variable splitting in query splitting?
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


getTypeLoc :: GCtx -> G.Name -> Maybe VT.TypeLoc
getTypeLoc gCtx node =
    VT._fiLoc <$> Map.lookup node schemaNodes
  where
    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
      in maybe qr (Map.union qr) mr
