module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , GExPHasura(..)
  , GExPRemote(..)
  , IsAsync(..)
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

newtype IsAsync
  = IsAsync { unIsAsync :: Bool }
  deriving (Show, Eq)

asyncDir :: G.Name
asyncDir = G.Name "async"

data GExPRemote
  = GExPRemote
  { _geprRemoteInfo :: !RemoteSchemaInfo
  , _geprRequest    :: !GraphQLRequest
  , _geprRootSelSet :: !VQ.RootSelSet
  , _geprIsAsync    :: !IsAsync
  } deriving (Show, Eq)

data GQExecPlan
  = GQExecPlan
  { _gqepHasura :: !(Maybe GExPHasura)
  , _gqepRemote :: ![GExPRemote]
  , _gqepOpType :: !G.OperationType
  } deriving (Show, Eq)

data QueryParts
  = QueryParts
  { _qpOpDef     :: !G.TypedOperationDefinition
  , _qpFragDefsL :: ![G.FragmentDefinition]
  , _qpIsAsync   :: !IsAsync
  --, _qpVarValsM  :: !(Maybe VariableValues)
  } deriving (Show, Eq)

data SelSetInfo = SelSetInfo !G.SelectionSet !IsAsync

type SplitSelSets    = Map.HashMap VT.TypeLoc SelSetInfo
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
  _ <- flip runReaderT gCtx $ VQ.validateGQ queryParts

  let opDef = VQ.qpOpDef queryParts
      fragDefs = VQ.qpFragDefsL queryParts
      ssSplits = foldr (splitSelSet gCtx) mempty $ G._todSelectionSet opDef
      querySplits = splitQuery opDef fragDefs ssSplits
      newQs = Map.map (transformGQRequest req) querySplits
      opTy = G._todType opDef

  newRootSels <- forM (Map.toList newQs) $ \(tl, (q, _)) ->
    flip runReaderT gCtx $
      (,) tl <$> (VQ.getQueryParts q >>= VQ.validateGQ)

  let newReqs = zipWith (\(tl,q) (_,rs) -> (tl, (q,rs))) (Map.toList newQs) newRootSels

  case newReqs of
    -- this shouldn't happen
    [] -> throw500 "unexpected plan"

    [(VT.HasuraType, (_, rootSelSet))] ->
      return $ GQExecPlan (Just $ GExPHasura gCtx rootSelSet) [] opTy
    [(VT.RemoteType _ rsi, ((_, async), rootSelSet))] ->
      return $ GQExecPlan Nothing [GExPRemote rsi req rootSelSet async] opTy
    xs -> foldM (combPlan gCtx) (GQExecPlan Nothing [] opTy) xs

  where
    gCtxRoleMap = scGCtxMap sc

    combPlan gCtx plan (tl, ((newq, async), rs)) = do
      let GQExecPlan hPlan remPlans ty = plan
      case tl of
        VT.HasuraType ->
          case hPlan of
            Nothing ->
              return $ GQExecPlan (Just $ GExPHasura gCtx rs) remPlans ty
            Just _  -> throw500 "encountered more than one hasura plan!"
        VT.RemoteType _ rsi -> do
          let newplan = (GExPRemote rsi newq rs async):remPlans
          return $ GQExecPlan hPlan newplan ty


splitSelSet :: GCtx -> G.Selection -> SplitSelSets -> SplitSelSets
splitSelSet ctx sel theMap = case sel of
  G.SelectionField fld ->
    let loc = getTypeLoc ctx $ G._fName fld
        async = maybe (IsAsync False) (const $ IsAsync True) $
                find (\d -> G._dName d == asyncDir) $ G._fDirectives fld
        otherDirs = filter (\d -> G._dName d /= asyncDir) $ G._fDirectives fld
        newFld = fld { G._fDirectives = otherDirs }
        newSel = G.SelectionField newFld
    in maybe theMap (\l -> upsertType (l, newSel) theMap async) loc
  -- TODO: what do we with fragments? we don't have any typeinfo on them
  _ -> theMap
  where
    upsertType (tl, n) qMap async = Map.alter (fn async n) tl qMap
    fn async n mVal =
      Just $ case mVal of
        Nothing               -> SelSetInfo [n] async
        Just (SelSetInfo s _) -> SelSetInfo (n:s) async


splitQuery
  :: G.TypedOperationDefinition
  -> [G.FragmentDefinition]
  -> SplitSelSets
  -> SplitQueryParts
splitQuery opDef fragDefs qMap =
  flip Map.map qMap $ \(SelSetInfo selSet isAsync) ->
    let frags = pickFrags selSet
        vars  = filterVars selSet $ G._todVariableDefinitions opDef
    in mkNewQParts frags selSet vars isAsync
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

    mkNewQParts frags selset varDefs isAsync =
      let od = opDef { G._todSelectionSet = selset
                     , G._todVariableDefinitions = varDefs
                     }
      in QueryParts od frags isAsync


transformGQRequest :: GraphQLRequest -> QueryParts -> (GraphQLRequest, IsAsync)
transformGQRequest (GraphQLRequest opNameM _ varValsM)
                   (QueryParts opDef fragDefs isAsync) =
  let op = G.ExecutableDefinitionOperation $ G.OperationDefinitionTyped opDef
      frags = map G.ExecutableDefinitionFragment fragDefs
      exDefs = op:frags
      q = GraphQLQuery exDefs
      -- TODO: do this variable splitting in query splitting?
      vars = map G._vdVariable $ G._todVariableDefinitions opDef
      varValsFltrd = Map.filterWithKey (\k _ -> k `elem` vars) <$> varValsM
  in (GraphQLRequest opNameM q varValsFltrd, isAsync)


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
  -- TODO: use getFieldInfo from Validation.Context
  VT._fiLoc <$> Map.lookup node schemaNodes
  where
    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
          sr = VT._otiFields <$> _gSubRoot gCtx
      in foldr (\x cm -> maybe cm (Map.union cm) x) qr [sr, mr]
