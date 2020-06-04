module Hasura.GraphQL.Execute.Prepare
  ( PlanVariables
  , PrepArgMap
  , PlanningSt(..)
  , initPlanningSt
  , runPlan
  , prepareWithPlan
  , withUserVars
  ) where


import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.IntMap                   as IntMap
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Data.Aeson                             as J

import qualified Hasura.SQL.DML                as S

import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Schema  (getName)
import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Hasura.RQL.Types


type PlanVariables = Map.HashMap G.Name Int

-- | The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
-- prepared argument and not the binary encoding in PG format
type PrepArgMap = IntMap.IntMap (Q.PrepArg, PGScalarValue)


data PlanningSt
  = PlanningSt
  { _psArgNumber :: !Int
  , _psVariables :: !PlanVariables
  , _psPrepped   :: !PrepArgMap
  }

initPlanningSt :: PlanningSt
initPlanningSt =
  PlanningSt 2 Map.empty IntMap.empty

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
    let sessVarVal =
          S.SEOpApp (S.SQLOp "->>")
          [currentSession, S.SELit $ T.toLower sessVar]
    return $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessVarVal
      PGTypeArray _      -> sessVarVal

  UVLiteral sqlExp -> pure sqlExp
  UVSession        -> pure currentSession
  where
    currentSession = S.SEPrep 1

withUserVars :: UserVars -> [(Q.PrepArg, PGScalarValue)] -> [(Q.PrepArg, PGScalarValue)]
withUserVars usrVars list =
  let usrVarsAsPgScalar = PGValJSON $ Q.JSON $ J.toJSON usrVars
      prepArg = Q.toPrepVal (Q.AltJ usrVars)
  in (prepArg, usrVarsAsPgScalar):list

getVarArgNum :: (MonadState PlanningSt m) => G.Name -> m Int
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
