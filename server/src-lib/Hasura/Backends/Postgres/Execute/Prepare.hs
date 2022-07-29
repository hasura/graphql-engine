-- | Postgres Execute Prepare
--
-- Deals with translating (session) variables to SQL expressions. Uses a state
-- monad to keep track of things like variables and generating fresh variable
-- names.
--
-- See 'Hasura.Backends.Postgres.Instances.Execute'.
module Hasura.Backends.Postgres.Execute.Prepare
  ( PlanVariables,
    PrepArgMap,
    PlanningSt (..),
    ExecutionPlan,
    ExecutionStep (..),
    initPlanningSt,
    prepareWithPlan,
    prepareWithoutPlan,
    withUserVars,
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.IntMap qualified as IntMap
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Column
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
  ( Code (NotFound),
    QErr,
    throw400,
  )
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Parser.Names
import Hasura.Prelude
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend
import Hasura.Session
  ( SessionVariables,
    UserInfo (_uiSession),
    getSessionVariableValue,
    sessionVariableToText,
  )
import Language.GraphQL.Draft.Syntax qualified as G

type PlanVariables = Map.HashMap G.Name Int

-- | The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
-- prepared argument and not the binary encoding in PG format
type PrepArgMap = IntMap.IntMap (Q.PrepArg, PGScalarValue)

data PlanningSt = PlanningSt
  { _psArgNumber :: Int,
    _psVariables :: PlanVariables,
    _psPrepped :: PrepArgMap
  }
  deriving stock (Eq, Show)

initPlanningSt :: PlanningSt
initPlanningSt = PlanningSt 2 Map.empty IntMap.empty

-- | If we're preparing a value with planning state, we favour referring to
-- values by their prepared argument index. If the value refers to a session
-- value, we look for it in prepared value (1) and access the particular keys
-- using the JSONB @->>@ accessor.
prepareWithPlan ::
  ( MonadState PlanningSt m,
    MonadError QErr m
  ) =>
  UserInfo ->
  UnpreparedValue ('Postgres pgKind) ->
  m S.SQLExp
prepareWithPlan userInfo = \case
  UVParameter varInfoM ColumnValue {..} -> do
    argNum <- maybe getNextArgNum (getVarArgNum . getName) varInfoM
    addPrepArg argNum (binEncoder cvValue, cvValue)
    return $ toPrepParam argNum (unsafePGColumnToBackend cvType)
  UVSessionVar ty sessVar -> do
    -- For queries, we need to make sure the session variables are passed. However,
    -- we want to keep them as variables in the resulting SQL in order to keep
    -- hitting query caching for similar queries.
    _ <-
      getSessionVariableValue sessVar (_uiSession userInfo)
        `onNothing` throw400
          NotFound
          ("missing session variable: " <>> sessionVariableToText sessVar)
    let sessVarVal =
          S.SEOpApp
            (S.SQLOp "->>")
            [currentSessionExp, S.SELit $ sessionVariableToText sessVar]
    pure $ withTypeAnn ty sessVarVal
  UVLiteral sqlExp -> pure sqlExp
  UVSession -> pure currentSessionExp
  where
    currentSessionExp = S.SEPrep 1

-- | If we're /not/ using a prepared statement, substitution is pretty naïve:
-- we resolve session variable names, ignore parameter names, and substitute
-- into the 'S.SQLExp'.
prepareWithoutPlan ::
  (MonadError QErr m) =>
  UserInfo ->
  UnpreparedValue ('Postgres pgKind) ->
  m S.SQLExp
prepareWithoutPlan userInfo = \case
  UVParameter _ cv -> pure $ toTxtValue cv
  UVLiteral sqlExp -> pure sqlExp
  UVSession -> pure $ sessionInfoJsonExp $ _uiSession userInfo
  UVSessionVar ty sessVar -> do
    let maybeSessionVariableValue =
          getSessionVariableValue sessVar (_uiSession userInfo)
    sessionVariableValue <-
      fmap S.SELit
        <$> onNothing maybeSessionVariableValue
        $ throw400 NotFound $
          "missing session variable: " <>> sessionVariableToText sessVar
    pure $ withTypeAnn ty sessionVariableValue

-- | The map of user session variables is always given the number (1) as its
-- variable argument number (see 'getVarArgNum'). If we want to refer to a
-- particular variable in this map, we use JSONB functions to interrogate
-- variable (1).
withUserVars :: SessionVariables -> PrepArgMap -> PrepArgMap
withUserVars usrVars list =
  let usrVarsAsPgScalar = PGValJSON $ Q.JSON $ J.toJSON usrVars
      prepArg = Q.toPrepVal (Q.AltJ usrVars)
   in IntMap.insert 1 (prepArg, usrVarsAsPgScalar) list

-- | In prepared statements, we refer to variables by a number, not their name.
-- If the statement already refers to a variable, then we'll already have a
-- number for it, and so we just return that. Otherwise, we produce a new
-- number, and that will refer to the variable from now on.
getVarArgNum :: (MonadState PlanningSt m) => G.Name -> m Int
getVarArgNum var = do
  PlanningSt curArgNum vars prepped <- get
  Map.lookup var vars `onNothing` do
    put $ PlanningSt (curArgNum + 1) (Map.insert var curArgNum vars) prepped
    pure curArgNum

-- | Add a prepared argument to the prepared argument map. These are keyed by
-- the variable argument numbers, which can be computed using 'getVarArgNum'.
addPrepArg ::
  (MonadState PlanningSt m) =>
  Int ->
  (Q.PrepArg, PGScalarValue) ->
  m ()
addPrepArg argNum arg = modify \s ->
  s {_psPrepped = IntMap.insert argNum arg (_psPrepped s)}

-- | Get '_psArgNumber' from inside the 'PlanningSt' and increment it for the
-- next operation. Think of this as a pure analogue to 'Data.Unique.newUnique'.
getNextArgNum :: (MonadState PlanningSt m) => m Int
getNextArgNum = state \s ->
  ( _psArgNumber s,
    s {_psArgNumber = _psArgNumber s + 1}
  )
