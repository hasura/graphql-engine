module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , queryOpFromPlan
  , ReusableQueryPlan
  , GeneratedSqlMap
  , PreparedSql(..)
  , GraphQLQueryType(..)
  ) where

import qualified Data.Aeson                             as J
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as Map
import qualified Data.IntMap                            as IntMap
import qualified Data.TByteString                       as TBS
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Control.Lens                           ((^?))
import           Data.Has

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.GraphQL.Validate.SelectionSet   as V
import qualified Hasura.SQL.DML                         as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Action
import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.RemoteJoin
import           Hasura.RQL.DML.Select
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type PlanVariables = Map.HashMap G.Variable Int

-- | The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
-- prepared argument and not the binary encoding in PG format
type PrepArgMap = IntMap.IntMap (Q.PrepArg, PGScalarValue)

data PGPlan
  = PGPlan
  { _ppQuery       :: !Q.Query
  , _ppVariables   :: !PlanVariables
  , _ppPrepared    :: !PrepArgMap
  , _ppRemoteJoins :: !(Maybe RemoteJoins)
  }

instance J.ToJSON PGPlan where
  toJSON (PGPlan q vars prepared _) =
    J.object [ "query"     J..= Q.getQueryText q
             , "variables" J..= vars
             , "prepared"  J..= fmap show prepared
             ]

data RootFieldPlan
  = RFPRaw !B.ByteString
  | RFPPostgres !PGPlan

fldPlanFromJ :: (J.ToJSON a) => a -> RootFieldPlan
fldPlanFromJ = RFPRaw . LBS.toStrict . J.encode

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPRaw encJson     -> J.toJSON $ TBS.fromBS encJson
    RFPPostgres pgPlan -> J.toJSON pgPlan

type FieldPlans = [(G.Alias, RootFieldPlan)]

data ReusableQueryPlan
  = ReusableQueryPlan
  { _rqpVariableTypes :: !ReusableVariableTypes
  , _rqpFldPlans      :: !FieldPlans
  }

instance J.ToJSON ReusableQueryPlan where
  toJSON (ReusableQueryPlan varTypes fldPlans) =
    J.object [ "variables"       J..= varTypes
             , "field_plans"     J..= fldPlans
             ]

withPlan
  :: (MonadError QErr m)
  => SessionVariables -> PGPlan -> ReusableVariableValues -> m PreparedSql
withPlan usrVars (PGPlan q reqVars prepMap rq) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  let args = withSessionVariables usrVars $ IntMap.elems prepMap'
  return $ PreparedSql q args rq
  where
    getVar accum (var, prepNo) = do
      let varName = G.unName $ G.unVariable var
      colVal <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      let prepVal = (toBinaryValue colVal, pstValue colVal)
      return $ IntMap.insert prepNo prepVal accum

-- turn the current plan into a transaction
mkCurPlanTx
  :: (HasVersion, MonadError QErr m)
  => HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> FieldPlans
  -> m (LazyRespTx, GeneratedSqlMap)
mkCurPlanTx manager reqHdrs userInfo fldPlans = do
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp                      -> return $ RRRaw resp
      RFPPostgres (PGPlan q _ prepMap rq) -> do
        let args = withSessionVariables (_uiSession userInfo) $ IntMap.elems prepMap
        return $ RRSql $ PreparedSql q args rq
    return (alias, fldResp)

  return (mkLazyRespTx manager reqHdrs userInfo resolved, mkGeneratedSqlMap resolved)

withSessionVariables :: SessionVariables -> [(Q.PrepArg, PGScalarValue)] -> [(Q.PrepArg, PGScalarValue)]
withSessionVariables usrVars list =
  let usrVarsAsPgScalar = PGValJSON $ Q.JSON $ J.toJSON usrVars
      prepArg = Q.toPrepVal (Q.AltJ usrVars)
  in (prepArg, usrVarsAsPgScalar):list

data PlanningSt
  = PlanningSt
  { _psArgNumber :: !Int
  , _psVariables :: !PlanVariables
  , _psPrepped   :: !PrepArgMap
  }

initPlanningSt :: PlanningSt
initPlanningSt =
  PlanningSt 2 Map.empty IntMap.empty

getVarArgNum :: (MonadState PlanningSt m) => G.Variable -> m Int
getVarArgNum var = do
  PlanningSt curArgNum vars prepped <- get
  case Map.lookup var vars of
    Just argNum -> pure argNum
    Nothing     -> do
      put $ PlanningSt (curArgNum + 1) (Map.insert var curArgNum vars) prepped
      pure curArgNum

addPrepArg
  :: (MonadState PlanningSt m)
  => Int -> (Q.PrepArg, PGScalarValue) -> m ()
addPrepArg argNum arg = do
  PlanningSt curArgNum vars prepped <- get
  put $ PlanningSt curArgNum vars $ IntMap.insert argNum arg prepped

getNextArgNum :: (MonadState PlanningSt m) => m Int
getNextArgNum = do
  PlanningSt curArgNum vars prepped <- get
  put $ PlanningSt (curArgNum + 1) vars prepped
  return curArgNum

prepareWithPlan :: (MonadState PlanningSt m) => UnresolvedVal -> m S.SQLExp
prepareWithPlan = \case
  R.UVPG annPGVal -> do
    let AnnPGVal varM _ colVal = annPGVal
    argNum <- case varM of
      Just var -> getVarArgNum var
      Nothing  -> getNextArgNum
    addPrepArg argNum (toBinaryValue colVal, pstValue colVal)
    return $ toPrepParam argNum (pstType colVal)

  R.UVSessVar ty sessVar -> do
    let sessVarVal =
          S.SEOpApp (S.SQLOp "->>")
          [currentSession, S.SELit $ sessionVariableToText sessVar]
    return $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessVarVal
      PGTypeArray _      -> sessVarVal

  R.UVSQL sqlExp -> pure sqlExp
  R.UVSession    -> pure currentSession
  where
    currentSession = S.SEPrep 1

convertQuerySelSet
  :: ( MonadError QErr m
     , MonadReader r m
     , Has TypeMap r
     , Has QueryCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     , HasVersion
     , MonadIO m
     )
  => HTTP.Manager
  -> [N.Header]
  -> QueryReusability
  -> V.ObjectSelectionSet
  -> QueryActionExecuter
  -> m (LazyRespTx, Maybe ReusableQueryPlan, GeneratedSqlMap)
convertQuerySelSet manager reqHdrs initialReusability selSet actionRunner = do
  userInfo <- asks getter
  (fldPlans, finalReusability) <- runReusabilityTWith initialReusability $
    fmap (map (\(a, b) -> (G.Alias $ G.Name a, b))) $ V.traverseObjectSelectionSet selSet $ \fld ->
      case V._fName fld of
        "__type"     -> fldPlanFromJ <$> R.typeR fld
        "__schema"   -> fldPlanFromJ <$> R.schemaR fld
        "__typename" -> pure $ fldPlanFromJ queryRootNamedType
        _            -> do
          unresolvedAst <- R.queryFldToPGAST fld actionRunner
          (q, PlanningSt _ vars prepped) <- flip runStateT initPlanningSt $
            R.traverseQueryRootFldAST prepareWithPlan unresolvedAst
          let (query, remoteJoins) = R.toPGQuery q
          pure . RFPPostgres $ PGPlan query vars prepped remoteJoins
      -- pure (V._fAlias fld, fldPlan)
  let varTypes = finalReusability ^? _Reusable
      reusablePlan = ReusableQueryPlan <$> varTypes <*> pure fldPlans
  (tx, sql) <- mkCurPlanTx manager reqHdrs userInfo fldPlans
  pure (tx, reusablePlan, sql)

-- use the existing plan and new variables to create a pg query
queryOpFromPlan
  :: (HasVersion, MonadError QErr m)
  => HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> Maybe GH.VariableValues
  -> ReusableQueryPlan
  -> m (LazyRespTx, GeneratedSqlMap)
queryOpFromPlan manager reqHdrs userInfo varValsM (ReusableQueryPlan varTypes fldPlans) = do
  validatedVars <- GV.validateVariablesForReuse varTypes varValsM
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) ->
    (alias,) <$> case fldPlan of
      RFPRaw resp        -> return $ RRRaw resp
      RFPPostgres pgPlan -> RRSql <$> withPlan (_uiSession userInfo) pgPlan validatedVars

  return (mkLazyRespTx manager reqHdrs userInfo resolved, mkGeneratedSqlMap resolved)

data PreparedSql
  = PreparedSql
  { _psQuery       :: !Q.Query
  , _psPrepArgs    :: ![(Q.PrepArg, PGScalarValue)]
    -- ^ The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
    -- prepared argument (PGScalarValue) and not the binary encoding in PG format (Q.PrepArg)
  , _psRemoteJoins :: !(Maybe RemoteJoins)
  }
  deriving Show

-- | Required to log in `query-log`
instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs _) =
    J.object [ "query" J..= Q.getQueryText q
             , "prepared_arguments" J..= map (txtEncodedPGVal . snd) prepArgs
             ]

-- | Intermediate reperesentation of a computed SQL statement and prepared
-- arguments, or a raw bytestring (mostly, for introspection responses)
-- From this intermediate representation, a `LazyTx` can be generated, or the
-- SQL can be logged etc.
data ResolvedQuery
  = RRRaw !B.ByteString
  | RRSql !PreparedSql

-- | The computed SQL with alias which can be logged. Nothing here represents no
-- SQL for cases like introspection responses. Tuple of alias to a (maybe)
-- prepared statement
type GeneratedSqlMap = [(G.Alias, Maybe PreparedSql)]

mkLazyRespTx
  :: HasVersion
  => HTTP.Manager -> [N.Header] -> UserInfo -> [(G.Alias, ResolvedQuery)] -> LazyRespTx
mkLazyRespTx manager reqHdrs userInfo resolved =
  fmap encJFromAssocList $ forM resolved $ \(alias, node) -> do
    resp <- case node of
      RRRaw bs                      -> return $ encJFromBS bs
      RRSql (PreparedSql q args maybeRemoteJoins) -> do
        let prepArgs = map fst args
        case maybeRemoteJoins of
          Nothing -> liftTx $ asSingleRowJsonResp q prepArgs
          Just remoteJoins ->
            executeQueryWithRemoteJoins manager reqHdrs userInfo q prepArgs remoteJoins
    return (G.unName $ G.unAlias alias, resp)

mkGeneratedSqlMap :: [(G.Alias, ResolvedQuery)] -> GeneratedSqlMap
mkGeneratedSqlMap resolved =
  flip map resolved $ \(alias, node) ->
    let res = case node of
                RRRaw _  -> Nothing
                RRSql ps -> Just ps
    in (alias, res)

-- The GraphQL Query type
data GraphQLQueryType
  = QueryHasura
  | QueryRelay
  deriving (Show, Eq, Ord, Generic)
instance Hashable GraphQLQueryType

instance J.ToJSON GraphQLQueryType where
  toJSON = \case
    QueryHasura -> "hasura"
    QueryRelay  -> "relay"
