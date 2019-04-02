module Hasura.GraphQL.Execute.Context
  ( getMutTx
  , getQueryTx
  , getSubsTx
  ) where

import           Data.Has

import qualified Data.HashMap.Strict            as Map
import qualified Data.IntMap                    as IntMap
import qualified Database.PG.Query              as Q
import qualified Language.GraphQL.Draft.Syntax  as G

import qualified Hasura.GraphQL.Execute.Plan    as Plan
import qualified Hasura.GraphQL.Resolve         as R
import qualified Hasura.GraphQL.Validate.Field  as V
import qualified Hasura.SQL.DML                 as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Value
import           Hasura.SQL.Types

-- Monad for resolving a hasura query/mutation
type E =
  ReaderT ( UserInfo
          , OpCtxMap
          , TypeMap
          , FieldMap
          , OrdByCtx
          , InsCtxMap
          , SQLGenCtx
          ) (Except QErr)

runE
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> E a
  -> m a
runE ctx sqlGenCtx userInfo action =
  either throwError return $
  runExcept $ runReaderT action
  (userInfo, opCtxMap, typeMap, fldMap, ordByCtx, insCtxMap, sqlGenCtx)
  where
    opCtxMap = _gOpCtxMap ctx
    typeMap = _gTypes ctx
    fldMap = _gFields ctx
    ordByCtx = _gOrdByCtx ctx
    insCtxMap = _gInsCtxMap ctx

data PlanningSt
  = PlanningSt
  { _psArgNumber :: !Int
  , _psVariables :: !Plan.PlanVariables
  , _psPrepped   :: !Plan.PrepArgMap
  }

initPlanningSt :: PlanningSt
initPlanningSt = PlanningSt 1 Map.empty IntMap.empty

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
  => AnnPGVal -> m S.SQLExp
prepareWithPlan annPGVal  = do
  argNum <- case (varM, isNullable) of
    (Just var, False) -> getVarArgNum var colTy
    _                 -> getNextArgNum
  addPrepArg argNum $ binEncoder colVal
  return $ toPrepParam argNum colTy
  where
    AnnPGVal varM isNullable colTy colVal = annPGVal

mkRootTypeName :: G.OperationType -> Text
mkRootTypeName = \case
  G.OperationTypeQuery        -> "query_root"
  G.OperationTypeMutation     -> "mutation_root"
  G.OperationTypeSubscription -> "subscription_root"

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
  => V.SelSet
  -> m [(G.Alias, Plan.RootFieldPlan)]
convertQuerySelSet fields =
  forM (toList fields) $ \fld -> do
    fldPlan <- case V._fName fld of
      "__type"     -> Plan.RFPRaw . encJFromJValue <$> R.typeR fld
      "__schema"   -> Plan.RFPRaw . encJFromJValue <$> R.schemaR fld
      "__typename" -> return $ Plan.RFPRaw $ encJFromJValue $
                      mkRootTypeName G.OperationTypeQuery
      _            -> do
        (q, PlanningSt _ vars prepped) <-
          flip runStateT initPlanningSt $ R.queryFldToSQL prepareWithPlan fld
        return $ Plan.RFPPostgres $ Plan.PGPlan q vars prepped
    return (V._fAlias fld, fldPlan)

getQueryTx'
  :: (MonadError QErr m)
  => (V.SelSet -> E [(G.Alias, Plan.RootFieldPlan)])
  -> Bool
  -> GCtx
  -> SQLGenCtx
  -> UserInfo
  -> V.SelSet
  -> [G.VariableDefinition]
  -> m (LazyRespTx, Maybe Plan.ReusablePlan)
getQueryTx' fn isSubs gCtx sqlGenCtx userInfo fields varDefs = do
  fldPlans <- runE gCtx sqlGenCtx userInfo $ fn fields
  let queryPlan     = Plan.QueryPlan isSubs varDefs fldPlans
      reusablePlanM = Plan.getReusablePlan queryPlan
  return (liftTx $ Plan.mkCurPlanTx queryPlan, reusablePlanM)

getQueryTx
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> V.SelSet
  -> [G.VariableDefinition]
  -> m (LazyRespTx, Maybe Plan.ReusablePlan)
getQueryTx =
  getQueryTx' convertQuerySelSet False

resolveMutSelSet
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     )
  => V.SelSet
  -> m LazyRespTx
resolveMutSelSet fields = do
  aliasedTxs <- forM (toList fields) $ \fld -> do
    fldRespTx <- case V._fName fld of
      "__typename" -> return $ return $ encJFromJValue $
                      mkRootTypeName G.OperationTypeMutation
      _            -> liftTx <$> R.mutFldToTx fld
    return (G.unName $ G.unAlias $ V._fAlias fld, fldRespTx)

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
      forM aliasedTxs $ \(al, tx) -> (,) al <$> tx


getMutTx
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> V.SelSet
  -> m LazyRespTx
getMutTx ctx sqlGenCtx userInfo selSet =
  runE ctx sqlGenCtx userInfo $ resolveMutSelSet selSet

convertSubsFld
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     )
  => V.Field
  -> m (G.Alias, Plan.RootFieldPlan)
convertSubsFld fld = do
  fldPlan <- case V._fName fld of
    "__typename" -> return $ Plan.RFPRaw $ encJFromJValue $
                    mkRootTypeName G.OperationTypeSubscription
    _            -> do
      (q, PlanningSt _ vars prepped) <-
        flip runStateT initPlanningSt $ R.queryFldToSQL prepareWithPlan fld
      return $ Plan.RFPPostgres $ Plan.PGPlan q vars prepped
  return (V._fAlias fld, fldPlan)

getSubsTx
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> V.Field
  -> [G.VariableDefinition]
  -> m (LazyRespTx, Maybe Plan.ReusablePlan)
getSubsTx gCtx sqlGenCtx userInfo fld =
  getQueryTx' (mapM convertSubsFld . toList) True gCtx
  sqlGenCtx userInfo $ pure fld
