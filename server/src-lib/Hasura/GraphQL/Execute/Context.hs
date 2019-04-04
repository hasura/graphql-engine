module Hasura.GraphQL.Execute.Context
  ( getMutTx
  , getQueryTx
  , getSubsOp
  ) where

import           Data.Has

import qualified Data.HashMap.Strict              as Map
import qualified Data.HashSet                     as Set
import qualified Data.IntMap                      as IntMap
import qualified Data.Text                        as T
import qualified Database.PG.Query                as Q
import qualified Language.GraphQL.Draft.Syntax    as G

import qualified Hasura.GraphQL.Execute.LiveQuery as LQ
import qualified Hasura.GraphQL.Execute.Plan      as Plan
import qualified Hasura.GraphQL.Resolve           as R
import qualified Hasura.GraphQL.Validate.Field    as V
import qualified Hasura.SQL.DML                   as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Select            (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

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

getQueryTx
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> V.SelSet
  -> [G.VariableDefinition]
  -> m (LazyRespTx, Maybe Plan.ReusableQueryPlan)
getQueryTx gCtx sqlGenCtx userInfo fields varDefs = do
  fldPlans <- runE gCtx sqlGenCtx userInfo $ convertQuerySelSet fields
  let queryPlan     = Plan.QueryPlan varDefs fldPlans
      reusablePlanM = Plan.getReusablePlan queryPlan
  return (Plan.mkCurPlanTx queryPlan, reusablePlanM)

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

collectNonNullableVars
  :: (MonadState Plan.VariableTypes m)
  => UnresolvedVal -> m UnresolvedVal
collectNonNullableVars val = do
  case val of
    UVPG annPGVal -> do
      let AnnPGVal varM isNullable colTy _ = annPGVal
      case (varM, isNullable) of
        (Just var, False) -> modify (Map.insert var colTy)
        _                 -> return ()
    _             -> return ()
  return val

toMultiplexedQueryVar
  :: (MonadState TextEncodedVariables m)
  => UnresolvedVal -> m S.SQLExp
toMultiplexedQueryVar = \case
  UVPG annPGVal ->
    let AnnPGVal varM isNullable colTy colVal = annPGVal
    in case (varM, isNullable) of
      -- we don't check for nullability as
      -- this is only used for reusable plans
      -- the check has to be made before this
      (Just var, _) -> do
        modify $ Map.insert var (txtEncodedPGVal colVal)
        return $ fromResVars colTy
          [ "variables"
          , G.unName $ G.unVariable var
          ]
      _             -> return $ toTxtValue colTy colVal
  -- TODO: check the logic around colTy and session variable's type
  UVSessVar colTy sessVar ->
    return $ fromResVars colTy [ "user", T.toLower sessVar]
  UVSQL sqlExp -> return sqlExp
  where
    fromResVars colTy jPath =
      S.withTyAnn colTy $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIden $ S.QIden (S.QualIden $ Iden "_subs")
        (Iden "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]

type TextEncodedVariables
  = Map.HashMap G.Variable TxtEncodedPGVal

getSubsOpM
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     )
  => [G.VariableDefinition]
  -> V.Field
  -> m (LQ.LiveQueryOp, Maybe Plan.SubsPlan)
getSubsOpM varDefs fld = do
  userInfo <- asks getter
  case V._fName fld of
    "__typename" -> do
      let tx = return $ encJFromJValue $
               mkRootTypeName G.OperationTypeSubscription
      return (LQ.LQFallback $ withAlias tx, Nothing)
    _            -> do
      astUnresolved <- R.queryFldToPGAST fld
      (_, varTypes) <- flip runStateT mempty $ R.traverseQueryRootFldAST
                       collectNonNullableVars astUnresolved
      -- can the subscription be multiplexed?
      if Set.fromList (Map.keys varTypes) == allVars
        then multiplexedOp varTypes astUnresolved
        else fallbackOp userInfo astUnresolved
  where
    allVars = Set.fromList $ map G._vdVariable varDefs

    fldAls  = V._fAlias fld

    -- multiplexed subscription
    multiplexedOp varTypes astUnresolved = do
      (astResolved, txtEncodedVars) <-
        flip runStateT mempty $ R.traverseQueryRootFldAST
        toMultiplexedQueryVar astUnresolved
      let mxQuery = LQ.mkMxQuery $ R.toPGQuery astResolved
          plan    = Plan.SubsPlan fldAls mxQuery varTypes
      return (LQ.LQMultiplexed (fldAls, mxQuery, txtEncodedVars), Just plan)

    -- fallback tx subscription
    fallbackOp userInfo astUnresolved = do
      (astResolved, prepArgs) <-
        withPrepArgs $ R.traverseQueryRootFldAST
        resolveValPrep astUnresolved
      let tx = withUserInfo userInfo $ liftTx $
               asSingleRowJsonResp (R.toPGQuery astResolved) $ toList prepArgs
      return (LQ.LQFallback $ withAlias tx, Nothing)

    fldAlsT = G.unName $ G.unAlias fldAls
    withAlias tx =
      encJFromAssocList . pure . (,) fldAlsT <$> tx

getSubsOp
  :: (MonadError QErr m)
  => GCtx
  -> SQLGenCtx
  -> UserInfo
  -> V.Field
  -> [G.VariableDefinition]
  -> m (LQ.LiveQueryOp, Maybe Plan.SubsPlan)
getSubsOp gCtx sqlGenCtx userInfo fld varDefs =
  runE gCtx sqlGenCtx userInfo $ getSubsOpM varDefs fld
