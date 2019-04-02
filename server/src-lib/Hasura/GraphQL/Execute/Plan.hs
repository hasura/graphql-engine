module Hasura.GraphQL.Execute.Plan
  ( RootFieldPlan(..)
  , PGPlan(..)
  , PlanVariables
  , PrepArgMap
  , QueryPlan(..)
  , ReusablePlan
  , getReusablePlan
  , mkNewQueryTx
  , mkCurPlanTx
  , PlanCache
  , getPlan
  , addPlan
  , initPlanCache
  , clearPlanCache
  , dumpPlanCache
  ) where

import qualified Hasura.Cache                           as Cache
import           Hasura.EncJSON
import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.IntMap.Strict                     as IntMap
import qualified Data.TByteString                       as TBS
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.GraphQL.Resolve.Context
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import           Hasura.RQL.DML.Select                  (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

data PlanId
  = PlanId
  { _piSchemaCacheVersion :: !SchemaCacheVer
  , _piRole               :: !RoleName
  , _piOperationName      :: !(Maybe GH.OperationName)
  , _piQuery              :: !GH.GQLQueryText
  } deriving (Show, Eq, Generic)

instance Hashable PlanId

instance J.ToJSON PlanId where
  toJSON (PlanId scVer rn opNameM query) =
    J.object
    [ "schema_cache_version" J..= scVer
    , "role" J..= rn
    , "operation" J..= opNameM
    , "query" J..= query
    ]

newtype PlanCache
  = PlanCache
  { _unPlanCache :: Cache.UnboundedCache PlanId ReusablePlan }

initPlanCache :: IO PlanCache
initPlanCache = PlanCache <$> Cache.initCache

getPlan
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> PlanCache -> IO (Maybe ReusablePlan)
getPlan schemaVer rn opNameM q (PlanCache planCache) =
  Cache.lookup planCache planId
  where
    planId = PlanId schemaVer rn opNameM q

addPlan
  :: SchemaCacheVer -> RoleName -> Maybe GH.OperationName -> GH.GQLQueryText
  -> ReusablePlan -> PlanCache -> IO ()
addPlan schemaVer rn opNameM q queryPlan (PlanCache planCache) =
  Cache.insert planCache planId queryPlan
  where
    planId = PlanId schemaVer rn opNameM q

clearPlanCache :: PlanCache -> IO ()
clearPlanCache (PlanCache planCache) =
  Cache.clearCache planCache

dumpPlanCache :: PlanCache -> IO J.Value
dumpPlanCache (PlanCache cache) =
  J.toJSON <$> Cache.mapCache dumpEntry cache
  where
    dumpEntry (planId, plan) =
      J.object
      [ "id" J..= planId
      , "plan" J..= plan
      ]

data RootFieldPlan
  = RFPRaw !EncJSON
  | RFPPostgres !PGPlan

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPRaw encJson     -> J.toJSON $ TBS.fromLBS $ encJToLBS encJson
    RFPPostgres pgPlan -> J.toJSON pgPlan

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

type VariableTypes = Map.HashMap G.Variable PGColType

data QueryPlan
  = QueryPlan
  { _qpIsSubscription :: !Bool
  , _qpVariables      :: ![G.VariableDefinition]
  , _qpFldPlans       :: ![(G.Alias, RootFieldPlan)]
  }

data ReusablePlan
  = ReusablePlan
  { _rpIsSubscription :: !Bool
  , _rpVariableTypes  :: !VariableTypes
  , _rpFldPlans       :: ![(G.Alias, RootFieldPlan)]
  }

instance J.ToJSON ReusablePlan where
  toJSON (ReusablePlan isSubs varTypes fldPlans) =
    J.object [ "is_subscription" J..= isSubs
             , "variables"      J..= show varTypes
             , "field_plans"     J..= fldPlans
             ]

getReusablePlan :: QueryPlan -> Maybe ReusablePlan
getReusablePlan (QueryPlan isSubs vars fldPlans) =
  if all fldPlanReusable $ map snd fldPlans
  then Just $ ReusablePlan isSubs varTypes fldPlans
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
  :: PGPlan -> Map.HashMap G.Variable PGColValue -> RespTx
withPlan (PGPlan q reqVars prepMap) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  let args = IntMap.elems prepMap'
  asSingleRowJsonResp q args
  where
    getVar accum (var, (prepNo, _)) = do
      let varName = G.unName $ G.unVariable var
      colVal <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      let prepVal = binEncoder colVal
      return $ IntMap.insert prepNo prepVal accum

-- use the existing plan and new variables to create a pg query
mkNewQueryTx
  :: (MonadError QErr m)
  => Maybe GH.VariableValues
  -> ReusablePlan
  -> m (Bool, LazyRespTx)
mkNewQueryTx varValsM (ReusablePlan isSubs varTypes fldPlans) = do
  validatedVars <- validateVars
  let tx = fmap encJFromAssocList $ forM fldPlans $ \(alias, fldPlan) -> do
        fldResp <- case fldPlan of
          RFPRaw resp        -> return resp
          RFPPostgres pgPlan -> liftTx $ withPlan pgPlan validatedVars
        return (G.unName $ G.unAlias alias, fldResp)
  return (isSubs, tx)
  where
    varVals = fromMaybe Map.empty varValsM
    -- this is in similar spirit to the validation
    -- logic of variable values in validate.hs however
    -- here it is much simpler and can get rid of typemap requirement
    validateVars =
      flip Map.traverseWithKey varTypes $ \varName varType -> do
        let unexpectedVars = filter
              (not . (`Map.member` varTypes)) $ Map.keys varVals
        unless (null unexpectedVars) $
          throwVE $ "unexpected variables in variableValues: " <>
          GV.showVars unexpectedVars
        varVal <- onNothing (Map.lookup varName varVals) $
          throwVE $ "expecting a value for non-nullable variable: " <>
          GV.showVars [varName] <>
          -- we don't have the graphql type
          -- " of type: " <> T.pack (show varType) <>
          " in variableValues"
        runAesonParser (parsePGValue varType) varVal

-- turn the current plan into a transaction
mkCurPlanTx
  :: QueryPlan
  -> Q.TxE QErr EncJSON
mkCurPlanTx (QueryPlan _ _ fldPlans) =
  fmap encJFromAssocList $ forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp        -> return resp
      RFPPostgres pgPlan -> planTx pgPlan
    return (G.unName $ G.unAlias alias, fldResp)
  where
    planTx (PGPlan q _ prepMap) =
      asSingleRowJsonResp q $ IntMap.elems prepMap
