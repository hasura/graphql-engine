module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , queryOpFromPlan
  , ReusableQueryPlan
  , GeneratedSqlMap
  , PreparedSql(..)
  ) where

import           Data.Has

import qualified Data.Aeson                             as J
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.IntMap                            as IntMap
import qualified Data.TByteString                       as TBS
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.GraphQL.Validate.Field          as V
import qualified Hasura.SQL.DML                         as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Select                  (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

type PlanVariables = Map.HashMap G.Variable (Int, PGColType)
type PrepArgMap = IntMap.IntMap Q.PrepArg

data PGPlan
  = PGPlan
  { _ppQuery     :: !Q.Query
  , _ppVariables :: !PlanVariables
  , _ppPrepared  :: !PrepArgMap
  }

instance J.ToJSON PGPlan where
  toJSON (PGPlan q vars prepared) =
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

type VariableTypes = Map.HashMap G.Variable PGColType

data QueryPlan
  = QueryPlan
  { _qpVariables :: ![G.VariableDefinition]
  , _qpFldPlans  :: ![(G.Alias, RootFieldPlan)]
  }

data ReusableQueryPlan
  = ReusableQueryPlan
  { _rqpVariableTypes :: !VariableTypes
  , _rqpFldPlans      :: ![(G.Alias, RootFieldPlan)]
  }

instance J.ToJSON ReusableQueryPlan where
  toJSON (ReusableQueryPlan varTypes fldPlans) =
    J.object [ "variables"       J..= show varTypes
             , "field_plans"     J..= fldPlans
             ]

getReusablePlan :: QueryPlan -> Maybe ReusableQueryPlan
getReusablePlan (QueryPlan vars fldPlans) =
  if all fldPlanReusable $ map snd fldPlans
  then Just $ ReusableQueryPlan varTypes fldPlans
  else Nothing
  where
    allVars = Set.fromList $ map G._vdVariable vars

    -- this is quite aggressive, we can improve this by
    -- computing used variables in each field
    allUsed fldPlanVars =
      Set.null $ Set.difference allVars $ Set.fromList fldPlanVars

    fldPlanReusable = \case
      RFPRaw _           -> True
      RFPPostgres pgPlan -> allUsed $ Map.keys $ _ppVariables pgPlan

    varTypesOfPlan = \case
      RFPRaw _           -> mempty
      RFPPostgres pgPlan -> snd <$> _ppVariables pgPlan

    varTypes = Map.unions $ map (varTypesOfPlan . snd) fldPlans

withPlan
  :: (MonadError QErr m)
  => UserVars -> PGPlan -> GV.AnnPGVarVals -> m PreparedSql
withPlan usrVars (PGPlan q reqVars prepMap) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  let args = withUserVars usrVars $ IntMap.elems prepMap'
  return $ PreparedSql q args
  where
    getVar accum (var, (prepNo, _)) = do
      let varName = G.unName $ G.unVariable var
      (_, colVal) <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      let prepVal = binEncoder colVal
      return $ IntMap.insert prepNo prepVal accum

-- turn the current plan into a transaction
mkCurPlanTx
  :: (MonadError QErr m)
  => UserVars
  -> QueryPlan
  -> m (LazyRespTx, GeneratedSqlMap)
mkCurPlanTx usrVars (QueryPlan _ fldPlans) = do
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp                      -> return $ RRRaw resp
      RFPPostgres (PGPlan q _ prepMap) -> do
        let args = withUserVars usrVars $ IntMap.elems prepMap
        return $ RRSql $ PreparedSql q args
    return (alias, fldResp)

  return (mkLazyRespTx resolved, mkGeneratedSqlMap resolved)

withUserVars :: UserVars -> [Q.PrepArg] -> [Q.PrepArg]
withUserVars usrVars l =
  Q.toPrepVal (Q.AltJ usrVars):l

data PlanningSt
  = PlanningSt
  { _psArgNumber :: !Int
  , _psVariables :: !PlanVariables
  , _psPrepped   :: !PrepArgMap
  }

initPlanningSt :: PlanningSt
initPlanningSt =
  PlanningSt 2 Map.empty IntMap.empty

getVarArgNum
  :: (MonadState PlanningSt m)
  => G.Variable -> PGColType -> m Int
getVarArgNum var colTy = do
  PlanningSt curArgNum vars prepped <- get
  case Map.lookup var vars of
    Just argNum -> return $ fst argNum
    Nothing     -> do
      put $ PlanningSt (curArgNum + 1)
        (Map.insert var (curArgNum, colTy) vars) prepped
      return curArgNum

addPrepArg
  :: (MonadState PlanningSt m)
  => Int -> Q.PrepArg -> m ()
addPrepArg argNum arg = do
  PlanningSt curArgNum vars prepped <- get
  put $ PlanningSt curArgNum vars $ IntMap.insert argNum arg prepped

getNextArgNum
  :: (MonadState PlanningSt m)
  => m Int
getNextArgNum = do
  PlanningSt curArgNum vars prepped <- get
  put $ PlanningSt (curArgNum + 1) vars prepped
  return curArgNum

prepareWithPlan
  :: (MonadState PlanningSt m)
  => UnresolvedVal -> m S.SQLExp
prepareWithPlan = \case
  R.UVPG annPGVal -> do
    let AnnPGVal varM isNullable colTy colVal = annPGVal
    argNum <- case (varM, isNullable) of
      (Just var, False) -> getVarArgNum var colTy
      _                 -> getNextArgNum
    addPrepArg argNum $ binEncoder colVal
    return $ toPrepParam argNum colTy
  R.UVSessVar ty sessVar -> do
    let sessVarVal =
          S.SEOpApp (S.SQLOp "->>")
          [S.SEPrep 1, S.SELit $ T.toLower sessVar]
    return $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PgTypeSimple colTy -> withGeoVal colTy sessVarVal
      PgTypeArray _      -> sessVarVal
  R.UVSQL sqlExp -> return sqlExp

queryRootName :: Text
queryRootName = "query_root"

convertQuerySelSet
  :: ( MonadError QErr m
     , MonadReader r m
     , Has TypeMap r
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     )
  => [G.VariableDefinition]
  -> V.SelSet
  -> m (LazyRespTx, Maybe ReusableQueryPlan, GeneratedSqlMap)
convertQuerySelSet varDefs fields = do
  usrVars <- asks (userVars . getter)
  fldPlans <- forM (toList fields) $ \fld -> do
    fldPlan <- case V._fName fld of
      "__type"     -> fldPlanFromJ <$> R.typeR fld
      "__schema"   -> fldPlanFromJ <$> R.schemaR fld
      "__typename" -> return $ fldPlanFromJ queryRootName
      _            -> do
        unresolvedAst <- R.queryFldToPGAST fld
        (q, PlanningSt _ vars prepped) <-
          flip runStateT initPlanningSt $ R.traverseQueryRootFldAST
          prepareWithPlan unresolvedAst
        return $ RFPPostgres $ PGPlan (R.toPGQuery q) vars prepped
    return (V._fAlias fld, fldPlan)
  let queryPlan     = QueryPlan varDefs fldPlans
      reusablePlanM = getReusablePlan queryPlan
  (tx, sql) <- mkCurPlanTx usrVars queryPlan
  return (tx, reusablePlanM, sql)

-- use the existing plan and new variables to create a pg query
queryOpFromPlan
  :: (MonadError QErr m)
  => UserVars
  -> Maybe GH.VariableValues
  -> ReusableQueryPlan
  -> m (LazyRespTx, GeneratedSqlMap)
queryOpFromPlan usrVars varValsM (ReusableQueryPlan varTypes fldPlans) = do
  validatedVars <- GV.getAnnPGVarVals varTypes varValsM
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) ->
    (alias,) <$> case fldPlan of
      RFPRaw resp        -> return $ RRRaw resp
      RFPPostgres pgPlan -> RRSql <$> withPlan usrVars pgPlan validatedVars

  return (mkLazyRespTx resolved, mkGeneratedSqlMap resolved)


data PreparedSql
  = PreparedSql
  { _psQuery    :: !Q.Query
  , _psPrepArgs :: ![Q.PrepArg]
  }

-- | Required to log in `query-log`
instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs) =
      J.object [ "query" J..= Q.getQueryText q
               , "prepared_arguments" J..= fmap prepArgsJVal prepArgs
               ]
    where
      prepArgsJVal (_, arg) = fmap (bsToTxt . fst) arg

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

mkLazyRespTx :: [(G.Alias, ResolvedQuery)] -> LazyRespTx
mkLazyRespTx resolved =
  fmap encJFromAssocList $ forM resolved $ \(alias, node) -> do
    resp <- case node of
      RRRaw bs                   -> return $ encJFromBS bs
      RRSql (PreparedSql q args) -> liftTx $ asSingleRowJsonResp q args
    return (G.unName $ G.unAlias alias, resp)

mkGeneratedSqlMap :: [(G.Alias, ResolvedQuery)] -> GeneratedSqlMap
mkGeneratedSqlMap resolved =
  flip map resolved $ \(alias, node) ->
    let res = case node of
                RRRaw _  -> Nothing
                RRSql ps -> Just ps
    in (alias, res)
