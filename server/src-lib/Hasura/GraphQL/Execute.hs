{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Hasura.GraphQL.Execute
  ( GQExecPlan(..)
  , GExPHasura(..)
  , GExPRemote(..)
  , GExPDepRemote(..)
  , GraphQLRequestPromise(..)
  , JoinInfo(..)
  , IsAsync(..)
  , getExecPlan
  , execRemoteGQ
  , transformGQRequest
  ) where

import           Control.Exception                      (try)
import           Control.Lens                           hiding (op)
import           Data.List                              (nub)

import qualified Data.Aeson                             as J
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Hasura.GraphQL.Context                 as GC
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
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.ContextTypes    as CT
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Field          as VF
import qualified Hasura.GraphQL.Validate.Types          as VT

data JoinInfo
  = JoinInfo
  { jiParentAlias   :: !G.Alias
  , jiParentJoinKey :: !G.Name -- change to alias
  , jiChildJoinKey  :: !G.Name
  , jiChildAlias    :: !G.Alias
  , jiNamespace     :: !(Maybe G.Name)
  } deriving (Show, Eq)

newtype GraphQLRequestPromise
  = GraphQLRequestPromise {
      grpResolve  :: forall m. (MonadError QErr m) => [J.Value] -> m GraphQLRequest
    }

instance Show GraphQLRequestPromise where
  show _ = "GraphQLRequestPromise Func"

data GExPDepRemote = GExPDepRemote !RemoteSchemaInfo !GraphQLRequestPromise JoinInfo IsAsync
  deriving Show

data GExPHasura = GExPHasura !GCtx !VQ.RootSelSet [GExPDepRemote]
  deriving Show

data GExPRemote
  = GExPRemote
  { _geprRemoteInfo :: !RemoteSchemaInfo
  , _geprRequest    :: !GraphQLRequest
  , _geprIsAsync    :: !IsAsync
  } deriving (Show, Eq)

data GQExecPlan
  = GQExecPlan
  { _gqepHasura :: !(Maybe GExPHasura)
  , _gqepRemote :: ![GExPRemote]
  , _gqepOpType :: !G.OperationType
  } deriving Show

data QueryPartsA
  = QueryPartsA
  { _qpOpDef     :: !G.TypedOperationDefinition
  , _qpFragDefsL :: ![G.FragmentDefinition]
  , _qpIsAsync   :: !IsAsync
  } deriving Show

data SelSetInfo = SelSetInfo !G.SelectionSet !IsAsync

type SplitSelSets    = Map.HashMap VT.TypeLoc SelSetInfo
type SplitQueryParts = Map.HashMap VT.TypeLoc QueryPartsA

newtype IsAsync
  = IsAsync { unIsAsync :: Bool }
  deriving (Show, Eq)

asyncDir :: G.Name
asyncDir = G.Name "async"

getExecPlan
  :: (MonadError QErr m)
  => UserInfo
  -> SchemaCache
  -> GraphQLRequest
  -> m GQExecPlan
getExecPlan userInfo sc req = do
  (gCtx, _)  <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req
  let remoteSchemaMap = scRemoteResolvers sc
  rootSelSet <- flip runReaderT gCtx $ VQ.validateGQ queryParts

  let opDef = VQ.qpOpDef queryParts
      fragDefs = VQ.qpFragDefsL queryParts
      ssSplits = foldr (splitSelSet gCtx) mempty $ G._todSelectionSet opDef
      querySplits = splitQuery opDef fragDefs ssSplits
      newQs = Map.map (transformGQRequest req) querySplits
      opTy = G._todType opDef

  case Map.toList newQs of
    -- this shouldn't happen
    [] -> throw500 "unexpected plan"

    [(VT.HasuraType, _)] -> do
      -- use the rootSelSet which is already resolved
      remotePlans <- getRemoteRelPlans gCtx req rootSelSet remoteSchemaMap
      return $ GQExecPlan (Just $ GExPHasura gCtx rootSelSet remotePlans) [] opTy
    [(VT.RemoteType rsi, (_, isAsync))] ->
      -- use the whole query
      return $ GQExecPlan Nothing [GExPRemote rsi req isAsync] opTy
    xs -> foldM (combPlan gCtx remoteSchemaMap) (GQExecPlan Nothing [] opTy) xs

  where
    gCtxRoleMap = scGCtxMap sc

    combPlan gCtx schemaMap plan (tl, (q, async)) = do
      let GQExecPlan hPlan remPlans ty = plan
      case tl of
        VT.HasuraType ->
          case hPlan of
            Nothing -> do
              rs <- runReaderT (VQ.getQueryParts q >>= VQ.validateGQ) gCtx
              depRemotePlans <- getRemoteRelPlans gCtx req rs schemaMap
              return $ GQExecPlan (Just $ GExPHasura gCtx rs depRemotePlans) remPlans ty
            Just _  -> throw500 "encountered more than one hasura plan!"
        VT.RemoteType rsi -> do
          let newplan = (GExPRemote rsi q async):remPlans
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


filterVars :: [G.Selection] -> [G.VariableDefinition] -> [G.VariableDefinition]
filterVars selSet vars =
  let args = join $ map getArgs selSet
      filterVar var = G._vdVariable var `elem` join (map getVars args)
  in filter filterVar vars
  where
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
      in QueryPartsA od frags isAsync

batchQueryWithVals
  :: (MonadError QErr m)
  => GraphQLRequest
  -> G.Field
  -> VF.Field
  -> RemoteRelInfo
  -> [J.Value]
  -> m GraphQLRequest
batchQueryWithVals initReq origFld remFld rri values = do
  newDef <- mkDef
  return GraphQLRequest
    { _grOperationName = Nothing
    , _grQuery  = GraphQLQuery [newDef]
    , _grVariables  = Nothing
    }
  where
    mkDef = do
      untypedDef <- getOpDef
      return $ G.ExecutableDefinitionOperation untypedDef
    getOpDef = do
      fields <- generateFields values
      let namespaceM =  rfiNamespace $ rriRemoteField rri
      case namespaceM of
        Nothing -> return $ G.OperationDefinitionUnTyped fields
        Just ns -> do
          let nsFldEmpty = generateEmptyNSField ns
              fieldsWithNS = nsFldEmpty
                { G._fSelectionSet = fields }
          return $ G.OperationDefinitionUnTyped [G.SelectionField fieldsWithNS]
    generateFields jVals = forMWithInd jVals $
      (\val ind -> do
          gVal <- getGValFromJVal val
          return $ G.SelectionField G.Field
            { G._fAlias = Just (mkFldAlias (rriName rri) ind)
            , G._fName = rfiName $ rriRemoteField rri
            , G._fArguments = [G.Argument (rriInputField rri) gVal]
            , G._fDirectives = []
            , G._fSelectionSet = G._fSelectionSet origFld
            }
      )

    generateEmptyNSField ns = G.Field
      { G._fAlias = Nothing
      , G._fName = ns
      , G._fArguments = []
      , G._fDirectives = []
      , G._fSelectionSet = []
      }

    mkFldAlias relName ind = G.Alias . G.Name $ getRelTxt relName <> "_" <> T.pack (show ind)

    getGValFromJVal jVal = case jVal of
      J.String str -> return . G.VString . G.StringValue $ str
      _            -> throw500 "only string join columns are supported"

    forMWithInd :: (Monad m) => [a] ->  (a -> Int -> m b) -> m [b]
    forMWithInd xs f = zipWithM f xs [0..]

getRemoteRelPlans :: (MonadError QErr m) => GCtx -> GraphQLRequest -> VQ.RootSelSet -> RemoteSchemaMap -> m [GExPDepRemote]
getRemoteRelPlans gCtx req rss schemaMap = do
  case rss of
    VQ.RQuery ss -> do
      let hasuraTopFields = gatherHasuraFields (toList ss)
          remoteFields = getRemoteFields hasuraTopFields

      output <- concat <$> forM remoteFields (mkDepRemotePlan gCtx)

      return output
    _            -> return []
    where
      mkDepRemotePlan gCtx' (hf, rFlds) =
        forM rFlds $ \rf -> do
        let tableTy = VF._fType hf
        queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req
        let opDef = VQ.qpOpDef queryParts
        remFldG <- getFieldFromOpDef opDef (VF._fName hf) (VF._fName rf)
        rri <- getRemoteRelInfoFromField gCtx' (tableTy, VF._fName rf)
        let remoteSchemaName = rriRSchemaName rri
        rsi <- onNothing (Map.lookup remoteSchemaName schemaMap) $ throw500 "could not find remote schema"
        let joinInfo = JoinInfo
              { jiParentAlias = VF._fAlias hf
              , jiParentJoinKey = mkColName $ rriColumn rri  -- how to find alias
              , jiChildJoinKey = rriInputField rri -- join key maybe different than input key?
              , jiChildAlias = VF._fAlias rf
              , jiNamespace = rfiNamespace $ rriRemoteField rri
              }

              -- field should be remote node or relationship node?
        return $ GExPDepRemote rsi (GraphQLRequestPromise (batchQueryWithVals req remFldG rf rri)) joinInfo (IsAsync False)

      getFieldFromOpDef opDef hName remName = do
        let selSet = G._todSelectionSet opDef
            fieldM = findField hName selSet
        field <- assertField fieldM
        let innerSelSet = G._fSelectionSet field
            innerFieldM = findField remName innerSelSet

        innerField <- assertField innerFieldM
        return innerField

      assertField fieldM = do
        field <- onNothing fieldM $ throw500 "could not find field"
        case field of
          G.SelectionField fld -> return fld
          _                    -> throw500 "not a selection field"


      findField name selSet = flip find selSet $
        (\sel -> case sel of
          G.SelectionField fld -> name == G._fName fld
          _                    -> False
        )

      onlyHasura fldInfo = case VT._fiLoc fldInfo of
        VT.HasuraType -> True
        _             -> False

      onlyFldRemote typFld =  case typFld of
        CT.FldRemote _ -> True
        _              -> False

      getRemoteRelInfoFromField gctx nameTup = do
        let typedFieldM = Map.lookup nameTup (GC._gFields gctx)
        fld <- onNothing typedFieldM (throw500 "cannot find field")
        case fld of
          CT.FldRemote rri -> return rri
          _                -> throw500 "not a remote field"

      mkColName (PGCol n) = G.Name n

      onlyRemote = not.onlyHasura

      gatherHasuraFields fields =
        catMaybes $ flip map fields $ \fld ->
        if isJust (Map.lookup (VF._fName fld) hasuraQueryNodes)
        then Just fld
        else Nothing

      hasuraQueryNodes = Map.filter onlyHasura $ VT._otiFields $ _gQueryRoot gCtx
      remoteQueryNodes = Map.filter onlyRemote $ VT._otiFields $ _gQueryRoot gCtx
      remoteRelationships = filter onlyFldRemote $ Map.elems $ _gFields gCtx

      getRemoteFields fields = flip map fields $ \fld ->
        let fldSelSet = VF._fSelSet fld
            remoteFlds = filter isRemoteField (toList fldSelSet)
        in (fld, remoteFlds)

      isRemoteField fld =
        let fldName = VF._fName fld
            remoteRelM = find (hasRemoteRel fldName) remoteRelationships
        in isJust remoteRelM

      hasRemoteRel remRelName typFld = case typFld of
        CT.FldRemote rri -> G.unName remRelName == getRelTxt (rriName rri)
        _                -> False

transformGQRequest :: GraphQLRequest -> QueryPartsA -> (GraphQLRequest, IsAsync)
transformGQRequest (GraphQLRequest opNameM _ varValsM)
                   (QueryPartsA opDef fragDefs isAsync) =
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
  -> GExPRemote
  -> G.OperationType
  -> m EncJSON
execRemoteGQ manager userInfo reqHdrs plan opTy = do
  let GExPRemote rsi q _ = plan
      RemoteSchemaInfo _ url hdrConf fwdClientHdrs = rsi
  when (opTy == G.OperationTypeSubscription) $
    throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"

  hdrs <- getHeadersFromConf hdrConf
  let confHdrs   = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
      clientHdrs = bool [] filteredHeaders fwdClientHdrs
      options    = wreqOptions manager (userInfoToHdrs ++ clientHdrs ++
                                        confHdrs)

  res  <- liftIO $ try $ Wreq.postWith options (show url) (encJFromJValue q)
  resp <- either httpThrow return res
  return $ encJFromLBS $ resp ^. Wreq.responseBody

  where
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
