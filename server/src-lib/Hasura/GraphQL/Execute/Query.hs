module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , queryOpFromPlan
  , ReusableQueryPlan
  , GeneratedSqlMap
  , PreparedSql(..)
  , traverseQueryRootField -- for live query planning
  , irToRootFieldPlan
  ) where

import qualified Data.Aeson                             as J
import qualified Data.ByteString                        as B
import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.IntMap                            as IntMap
import qualified Data.Sequence.NonEmpty                 as NESeq
import qualified Data.TByteString                       as TBS
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.SQL.DML                         as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Monad
import           Hasura.Prelude
import           Hasura.RQL.DML.Select                  (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.RQL.DML.Select                  as DS

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

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPRaw encJson     -> J.toJSON $ TBS.fromBS encJson
    RFPPostgres pgPlan -> J.toJSON pgPlan

type FieldPlans = [(G.Name, RootFieldPlan)]

data ReusableVariableTypes  -- FIXME
data ReusableVariableValues -- FIXME

data ReusableQueryPlan
  = ReusableQueryPlan
  { _rqpVariableTypes :: !ReusableVariableTypes
  , _rqpFldPlans      :: !FieldPlans
  }

instance J.ToJSON ReusableQueryPlan where
  toJSON (ReusableQueryPlan varTypes fldPlans) =
    J.object [ "variables"       J..= () -- varTypes    FIXME!!
             , "field_plans"     J..= fldPlans
             ]

withPlan
  :: (MonadError QErr m)
  => UserVars -> PGPlan -> HashMap G.Name (WithScalarType PGScalarValue) -> m PreparedSql
withPlan usrVars (PGPlan q reqVars prepMap) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  let args = withUserVars usrVars $ IntMap.elems prepMap'
  return $ PreparedSql q args
  where
    getVar accum (var, prepNo) = do
      let varName = G.unName var
      colVal <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      let prepVal = (toBinaryValue colVal, pstValue colVal)
      return $ IntMap.insert prepNo prepVal accum

-- turn the current plan into a transaction
mkCurPlanTx
  :: (MonadError QErr m)
  => UserVars
  -> FieldPlans
  -> m (LazyRespTx, GeneratedSqlMap)
mkCurPlanTx usrVars fldPlans = do
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp                      -> return $ RRRaw resp
      RFPPostgres (PGPlan q _ prepMap) -> do
        let args = withUserVars usrVars $ IntMap.elems prepMap
        return $ RRSql $ PreparedSql q args
    return (alias, fldResp)

  return (mkLazyRespTx resolved, mkGeneratedSqlMap resolved)

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

-- convert a query from an intermediate representation to... another
irToRootFieldPlan
  :: PlanVariables
  -> PrepArgMap
  -> QueryDB S.SQLExp -> PGPlan
irToRootFieldPlan vars prepped = \case
  QDBSimple s      -> PGPlan (DS.selectQuerySQL DS.JASMultipleRows s) vars prepped
  QDBPrimaryKey s  -> PGPlan (DS.selectQuerySQL DS.JASSingleObject s) vars prepped
  QDBAggregation s -> PGPlan (DS.selectAggQuerySQL s) vars prepped

traverseQueryRootField
  :: forall f a b c d
   . Applicative f
  => (a -> f b)
  -> RootField (QueryDB a) c d
  -> f (RootField (QueryDB b) c d)
traverseQueryRootField f =
  traverseDB f'
  where
    f' :: QueryDB a -> f (QueryDB b)
    f' = \case
      QDBSimple s       -> QDBSimple      <$> DS.traverseAnnSimpleSel f s
      QDBPrimaryKey s   -> QDBPrimaryKey  <$> DS.traverseAnnSimpleSel f s
      QDBAggregation s  -> QDBAggregation <$> DS.traverseAnnAggSel f s

convertQuerySelSet
  :: forall m
   . MonadError QErr m
  => GQLContext
  -> UserVars
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m ( ExecutionPlan (LazyRespTx, GeneratedSqlMap) (G.Name, RemoteCall) (G.Name, J.Value)
       , Maybe ReusableQueryPlan
       , InsOrdHashMap G.Name (QueryRootField UnpreparedValue)
       )
convertQuerySelSet gqlContext usrVars fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  queryPlans <- for unpreparedQueries \unpreparedQuery -> do
    (preparedQuery, PlanningSt _ planVars planVals)
      <- flip runStateT initPlanningSt
      $  traverseQueryRootField prepareWithPlan unpreparedQuery
    traverseDB (pure . irToRootFieldPlan planVars planVals) preparedQuery

  -- This monster makes sure that consecutive database operation get executed together
  let collect :: ExecutionPlan (NESeq.NESeq (G.Name, RootFieldPlan)) (G.Name, RemoteCall) (G.Name, J.Value)
              -> G.Name
              -> RootField PGPlan RemoteCall J.Value
              -> ExecutionPlan (NESeq.NESeq (G.Name, RootFieldPlan)) (G.Name, RemoteCall) (G.Name, J.Value)
      collect (plans NESeq.:||> ExecStepDB dbs) name (RFDB db)
        = (plans NESeq.:||> ExecStepDB (dbs NESeq.|> (name, RFPPostgres db)))
      collect plans                             name (RFRemote x)
        = plans NESeq.|>    ExecStepRemote (name, x)
      collect plans                             name (RFRaw    x)
        = plans NESeq.|>    ExecStepRaw    (name, x)
      collect1 :: ExecutionPlan (NESeq.NESeq (G.Name, RootFieldPlan)) (G.Name, RemoteCall) (G.Name, J.Value)
      collect1 = NESeq.singleton $ case head (OMap.toList queryPlans) of
        (name, RFDB db) -> ExecStepDB $ NESeq.singleton (name, RFPPostgres db)
        (name, RFRemote rem) -> ExecStepRemote (name, rem)
        (name, RFRaw raw) -> ExecStepRaw (name, raw)
      -- TODO rather than this fromlist tolist mess, pass non-empty insertion ordered hash maps
      collectedPlans :: ExecutionPlan (NESeq.NESeq (G.Name, RootFieldPlan)) (G.Name, RemoteCall) (G.Name, J.Value)
      collectedPlans = OMap.foldlWithKey' collect collect1 $ OMap.fromList $ tail $ OMap.toList queryPlans
      -- This is where PGPlans get converted to LazyRespTx's
      dbTx :: ExecutionStep (NESeq.NESeq (G.Name, RootFieldPlan)) b c
           -> m (ExecutionStep (LazyRespTx, GeneratedSqlMap) b c)
      dbTx (ExecStepDB   seq) = ExecStepDB <$> mkCurPlanTx usrVars (toList seq)
      dbTx (ExecStepRemote z) = pure (ExecStepRemote z)
      dbTx (ExecStepRaw    z) = pure (ExecStepRaw    z)
  executionPlan <- traverse dbTx collectedPlans

  pure (executionPlan, Nothing, unpreparedQueries) -- FIXME ReusableQueryPlan
  where
    reportParseErrors errs = case NESeq.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage } ->
        throwError (err400 ValidationFailed peMessage){ qePath = pePath }

-- use the existing plan and new variables to create a pg query
queryOpFromPlan
  :: (MonadError QErr m)
  => UserVars
  -> Maybe GH.VariableValues
  -> ReusableQueryPlan
  -> m (LazyRespTx, GeneratedSqlMap)
queryOpFromPlan usrVars varValsM (ReusableQueryPlan varTypes fldPlans) = do
  validatedVars <- _validateVariablesForReuse varTypes varValsM
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) ->
    (alias,) <$> case fldPlan of
      RFPRaw resp        -> return $ RRRaw resp
      RFPPostgres pgPlan -> RRSql <$> withPlan usrVars pgPlan validatedVars

  return (mkLazyRespTx resolved, mkGeneratedSqlMap resolved)

data PreparedSql
  = PreparedSql
  { _psQuery    :: !Q.Query
  , _psPrepArgs :: ![(Q.PrepArg, PGScalarValue)]
    -- ^ The value is (Q.PrepArg, PGScalarValue) because we want to log the human-readable value of the
    -- prepared argument (PGScalarValue) and not the binary encoding in PG format (Q.PrepArg)
  }

-- | Required to log in `query-log`
instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs) =
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
type GeneratedSqlMap = [(G.Name, Maybe PreparedSql)]

mkLazyRespTx :: [(G.Name, ResolvedQuery)] -> LazyRespTx
mkLazyRespTx resolved =
  fmap encJFromAssocList $ forM resolved $ \(alias, node) -> do
    resp <- case node of
      RRRaw bs                   -> return $ encJFromBS bs
      RRSql (PreparedSql q args) -> liftTx $ asSingleRowJsonResp q (map fst args)
    return (G.unName alias, resp)

mkGeneratedSqlMap :: [(G.Name, ResolvedQuery)] -> GeneratedSqlMap
mkGeneratedSqlMap resolved =
  flip map resolved $ \(alias, node) ->
    let res = case node of
                RRRaw _  -> Nothing
                RRSql ps -> Just ps
    in (alias, res)
