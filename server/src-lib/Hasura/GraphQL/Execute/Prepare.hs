module Hasura.GraphQL.Execute.Prepare
  ( PlanVariables
  , PrepArgMap
  , PlanningSt(..)
  , RemoteCall
  , ExecutionPlan(..)
  , ExecutionJoin(..)
  , ExecutionStep(..)
  , initPlanningSt
  , runPlan
  , prepareWithPlan
  , prepareWithoutPlan
  , validateSessionVariables
  , withUserVars
  , buildTypedOperation
  ) where


import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.IntMap                            as IntMap
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.SQL.DML                         as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Schema
import           Hasura.RQL.DML.Internal                (currentSession)
import           Hasura.RQL.Types
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value


type PlanVariables = Map.HashMap G.Name Int

-- | The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
-- prepared argument and not the binary encoding in PG format
type PrepArgMap = IntMap.IntMap (Q.PrepArg, PGScalarValue)

-- | Full execution plan to process one GraphQL query.
data ExecutionPlan db remote raw a =
  ExecutionPlan (ExecutionStep db remote raw) [ExecutionJoin db remote raw a]

data ExecutionJoin db remote raw a = ExecutionJoin
  { -- given a list of objects, how do we generate the query for their relationship
    generate  :: a -> ExecutionPlan db remote raw a
  , -- how do we recombine the relationship items with the original objects
    -- recombine :: relationship -> original -> result
    recombine :: a -> a -> a
  }

-- NOTE:
-- this is neither a functor nor a contravariant functor, given how `a` is used.
-- this partially defeats the purpose of making it generic over a?


type RemoteCall = (RemoteSchemaInfo, G.TypedOperationDefinition G.NoFragments G.Name, Maybe GH.VariableValues)

-- | One execution step to processing a GraphQL query (e.g. one root field).
-- Polymorphic to allow the SQL to be generated in stages.
data ExecutionStep db remote raw
  = ExecStepDB db
  -- ^ A query to execute against the database
  | ExecStepRemote remote -- !RemoteSchemaInfo !(G.Selection G.NoFragments G.Name)
  -- ^ A query to execute against a remote schema
  | ExecStepRaw raw
  -- ^ Output a plain JSON object

data PlanningSt
  = PlanningSt
  { _psArgNumber        :: !Int
  , _psVariables        :: !PlanVariables
  , _psPrepped          :: !PrepArgMap
  , _psSessionVariables :: !(Set.HashSet SessionVariable)
  }

initPlanningSt :: PlanningSt
initPlanningSt =
  PlanningSt 2 Map.empty IntMap.empty Set.empty

runPlan :: StateT PlanningSt m a -> m (a, PlanningSt)
runPlan = flip runStateT initPlanningSt

prepareWithPlan :: (MonadState PlanningSt m) => UnpreparedValue -> m S.SQLExp
prepareWithPlan = \case
  UVParameter PGColumnValue{ pcvValue = colVal } varInfoM -> do
    argNum <- case fmap getName varInfoM of
      Just var -> getVarArgNum var
      Nothing  -> getNextArgNum
    addPrepArg argNum (toBinaryValue colVal, pstValue colVal)
    return $ toPrepParam argNum (pstType colVal)

  UVSessionVar ty sessVar -> do
    sessVarVal <- retrieveAndFlagSessionVariableValue insertSessionVariable sessVar currentSessionExp
    pure $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessVarVal
      PGTypeArray _      -> sessVarVal

  UVLiteral sqlExp -> pure sqlExp
  UVSession        -> pure currentSessionExp
  where
    currentSessionExp = S.SEPrep 1
    insertSessionVariable sessVar plan =
      plan { _psSessionVariables = Set.insert sessVar $ _psSessionVariables plan }

prepareWithoutPlan :: (MonadState (Set.HashSet SessionVariable) m) => UnpreparedValue -> m S.SQLExp
prepareWithoutPlan = \case
  UVParameter pgValue _   -> pure $ toTxtValue $ pcvValue pgValue
  UVLiteral sqlExp        -> pure sqlExp
  UVSession               -> pure currentSession
  UVSessionVar ty sessVar -> do
    sessVarVal <- retrieveAndFlagSessionVariableValue Set.insert sessVar currentSession
    -- TODO: this piece of code appears at least three times: twice here
    -- and once in RQL.DML.Internal. Some de-duplication is in order.
    pure $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessVarVal
      PGTypeArray _      -> sessVarVal

retrieveAndFlagSessionVariableValue
  :: (MonadState s m)
  => (SessionVariable -> s -> s)
  -> SessionVariable
  -> S.SQLExp
  -> m S.SQLExp
retrieveAndFlagSessionVariableValue updateState sessVar currentSessionExp = do
  modify $ updateState sessVar
  pure $ S.SEOpApp (S.SQLOp "->>")
    [currentSessionExp, S.SELit $ sessionVariableToText sessVar]

withUserVars :: SessionVariables -> [(Q.PrepArg, PGScalarValue)] -> [(Q.PrepArg, PGScalarValue)]
withUserVars usrVars list =
  let usrVarsAsPgScalar = PGValJSON $ Q.JSON $ J.toJSON usrVars
      prepArg = Q.toPrepVal (Q.AltJ usrVars)
  in (prepArg, usrVarsAsPgScalar):list

validateSessionVariables :: MonadError QErr m => Set.HashSet SessionVariable -> SessionVariables -> m ()
validateSessionVariables requiredVariables sessionVariables = do
  let missingSessionVariables = requiredVariables `Set.difference` getSessionVariablesSet sessionVariables
  unless (null missingSessionVariables) do
    throw400 NotFound $ "missing session variables: " <> T.intercalate ", " (dquote . sessionVariableToText <$> toList missingSessionVariables)

getVarArgNum :: (MonadState PlanningSt m) => G.Name -> m Int
getVarArgNum var = do
  PlanningSt curArgNum vars prepped sessionVariables <- get
  case Map.lookup var vars of
    Just argNum -> pure argNum
    Nothing     -> do
      put $ PlanningSt (curArgNum + 1) (Map.insert var curArgNum vars) prepped sessionVariables
      pure curArgNum

addPrepArg
  :: (MonadState PlanningSt m)
  => Int -> (Q.PrepArg, PGScalarValue) -> m ()
addPrepArg argNum arg = do
  PlanningSt curArgNum vars prepped sessionVariables <- get
  put $ PlanningSt curArgNum vars (IntMap.insert argNum arg prepped) sessionVariables

getNextArgNum :: (MonadState PlanningSt m) => m Int
getNextArgNum = do
  PlanningSt curArgNum vars prepped sessionVariables <- get
  put $ PlanningSt (curArgNum + 1) vars prepped sessionVariables
  return curArgNum

unresolveVariables
  :: forall fragments
   . Functor fragments
  => G.SelectionSet fragments Variable
  -> G.SelectionSet fragments G.Name
unresolveVariables =
  fmap (fmap (getName . vInfo))

collectVariables
  :: forall fragments var
   . (Foldable fragments, Hashable var, Eq var)
  => G.SelectionSet fragments var
  -> Set.HashSet var
collectVariables =
  Set.unions . fmap (foldMap Set.singleton)

buildTypedOperation
  :: forall frag
   . (Functor frag, Foldable frag)
  => G.OperationType
  -> [G.VariableDefinition]
  -> G.SelectionSet frag Variable
  -> Maybe GH.VariableValues
  -> (G.TypedOperationDefinition frag G.Name, Maybe GH.VariableValues)
buildTypedOperation tp varDefs selSet varValsM =
  let unresolvedSelSet = unresolveVariables selSet
      requiredVars = collectVariables unresolvedSelSet
      restrictedDefs = filter (\varDef -> G._vdName varDef `Set.member` requiredVars) varDefs
      restrictedValsM = flip Map.intersection (Set.toMap requiredVars) <$> varValsM
  in (G.TypedOperationDefinition tp Nothing restrictedDefs [] unresolvedSelSet, restrictedValsM)
