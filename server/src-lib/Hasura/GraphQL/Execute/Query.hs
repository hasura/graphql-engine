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
import qualified Data.Sequence                          as Seq
import qualified Data.Sequence.NonEmpty                 as NESeq
import qualified Data.TByteString                       as TBS
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import           Hasura.Server.Version                  (HasVersion)
import qualified Hasura.SQL.DML                         as S

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Parser.Monad
import           Hasura.GraphQL.Resolve.Action
import           Hasura.Prelude
import           Hasura.RQL.DML.RemoteJoin
import           Hasura.RQL.DML.Select                  (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Hasura.Session

import qualified Hasura.RQL.DML.Select                  as DS

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
  | RFPActionQuery !LazyRespTx

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPRaw encJson     -> J.toJSON $ TBS.fromBS encJson
    RFPPostgres pgPlan -> J.toJSON pgPlan
    RFPActionQuery _   -> J.String "Action Execution Tx"

type FieldPlans = [(G.Name, RootFieldPlan)]

data ActionQueryPlan
  = AQPAsyncQuery !DS.AnnSimpleSel -- ^ Cacheable plan
  | AQPQuery !RespTx -- ^ Non cacheable transaction

actionQueryToRootFieldPlan
  :: PlanVariables -> PrepArgMap -> ActionQueryPlan -> RootFieldPlan
actionQueryToRootFieldPlan vars prepped = \case
  AQPAsyncQuery s -> RFPPostgres $
    PGPlan (DS.selectQuerySQL DS.JASSingleObject s) vars prepped Nothing
  AQPQuery tx     -> RFPActionQuery (liftTx tx)

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
  => SessionVariables -> PGPlan -> HashMap G.Name (WithScalarType PGScalarValue) -> m PreparedSql
withPlan usrVars (PGPlan q reqVars prepMap remoteJoins) annVars = do
  prepMap' <- foldM getVar prepMap (Map.toList reqVars)
  let args = withUserVars usrVars $ IntMap.elems prepMap'
  return $ PreparedSql q args remoteJoins
  where
    getVar accum (var, prepNo) = do
      let varName = G.unName var
      colVal <- onNothing (Map.lookup var annVars) $
        throw500 $ "missing variable in annVars : " <> varName
      let prepVal = (toBinaryValue colVal, pstValue colVal)
      return $ IntMap.insert prepNo prepVal accum

-- turn the current plan into a transaction
mkCurPlanTx
  :: (HasVersion, MonadError QErr m)
  => HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> FieldPlans
  -> m (LazyRespTx, GeneratedSqlMap)
mkCurPlanTx manager reqHdrs userInfo fldPlans = do
  -- generate the SQL and prepared vars or the bytestring
  resolved <- forM fldPlans $ \(alias, fldPlan) -> do
    fldResp <- case fldPlan of
      RFPRaw resp                      -> return $ RRRaw resp
      RFPPostgres (PGPlan q _ prepMap remoteJoins) -> do
        let args = withUserVars (_uiSession userInfo) $ IntMap.elems prepMap
        return $ RRSql $ PreparedSql q args remoteJoins
      RFPActionQuery tx -> pure $ RRActionQuery tx
    return (alias, fldResp)

  return (mkLazyRespTx manager reqHdrs userInfo resolved, mkGeneratedSqlMap resolved)

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
  QDBSimple s      -> mkPGPlan (DS.selectQuerySQL DS.JASMultipleRows) s
  QDBPrimaryKey s  -> mkPGPlan (DS.selectQuerySQL DS.JASSingleObject) s
  QDBAggregation s ->
    let (annAggSel, aggRemoteJoins) = getRemoteJoinsAggregateSelect s
    in PGPlan (DS.selectAggQuerySQL annAggSel) vars prepped aggRemoteJoins
  where
    mkPGPlan f simpleSel =
      let (simpleSel',remoteJoins) = getRemoteJoins simpleSel
      in PGPlan (f simpleSel') vars prepped remoteJoins

traverseQueryRootField
  :: forall f a b c d h
   . Applicative f
  => (a -> f b)
  -> RootField (QueryDB a) c h d
  -> f (RootField (QueryDB b) c h d)
traverseQueryRootField f =
  traverseDB f'
  where
    f' :: QueryDB a -> f (QueryDB b)
    f' = \case
      QDBSimple s       -> QDBSimple      <$> DS.traverseAnnSimpleSel f s
      QDBPrimaryKey s   -> QDBPrimaryKey  <$> DS.traverseAnnSimpleSel f s
      QDBAggregation s  -> QDBAggregation <$> DS.traverseAnnAggSel f s

convertQuerySelSet
  :: forall m. (HasVersion, MonadError QErr m, MonadIO m)
  => GQLContext
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m ( ExecutionPlan (LazyRespTx, GeneratedSqlMap) RemoteCall (G.Name, J.Value)
       , Maybe ReusableQueryPlan
       , InsOrdHashMap G.Name (QueryRootField UnpreparedValue)
       )
convertQuerySelSet gqlContext userInfo manager reqHeaders fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  queryPlans <- for unpreparedQueries \unpreparedQuery -> do
    (preparedQuery, PlanningSt _ planVars planVals)
      <- flip runStateT initPlanningSt
         $ traverseQueryRootField prepareWithPlan unpreparedQuery
           >>= traverseAction convertActionQuery
    traverseDB (pure . irToRootFieldPlan planVars planVals) preparedQuery
      >>= traverseAction (pure . actionQueryToRootFieldPlan planVars planVals)

  -- This monster makes sure that consecutive database operation get executed together
  let dbPlans :: Seq.Seq (G.Name, RootFieldPlan)
      remoteFields :: Seq.Seq (G.Name, RemoteField)
      (dbPlans, remoteFields) = OMap.foldlWithKey' collectPlan (Seq.Empty, Seq.Empty) queryPlans

      collectPlan
        :: (Seq.Seq (G.Name, RootFieldPlan), Seq.Seq (G.Name, RemoteField))
        -> G.Name
        -> RootField PGPlan RemoteField RootFieldPlan J.Value
        -> (Seq.Seq (G.Name, RootFieldPlan), Seq.Seq (G.Name, RemoteField))

      collectPlan (seqDB, seqRemote) name (RFRemote r) =
        (seqDB, seqRemote Seq.:|> (name, r))

      collectPlan (seqDB, seqRemote) name (RFDB db)      =
        (seqDB Seq.:|> (name, RFPPostgres db), seqRemote)

      collectPlan (seqDB, seqRemote) name (RFAction rfp) =
        (seqDB Seq.:|> (name, rfp), seqRemote)

      collectPlan (seqDB, seqRemote) name (RFRaw r)      =
        (seqDB Seq.:|> (name, RFPRaw $ LBS.toStrict $ J.encode r), seqRemote)


  executionPlan <- case (dbPlans, remoteFields) of
    (dbs, Seq.Empty) -> ExecStepDB <$> mkCurPlanTx manager reqHeaders userInfo (toList dbs)
    (Seq.Empty, remotes@(firstRemote Seq.:<| _)) -> do
      let (remoteOperation, varValsM) =
            buildTypedOperation
            G.OperationTypeQuery
            varDefs
            (map (G.SelectionField . snd . snd) $ toList remotes)
            varValsM
      if all (\remote' -> fst (snd firstRemote) == fst (snd remote')) remotes
        then return $ ExecStepRemote (fst (snd firstRemote), remoteOperation, varValsM)
        else throw400 NotSupported "Mixed remote schemas are not supported"
    _ -> throw400 NotSupported "Heterogeneous execution of database and remote schemas not supported"
  pure (executionPlan, Nothing, unpreparedQueries) -- FIXME ReusableQueryPlan
  where
    usrVars = _uiSession userInfo
    reportParseErrors errs = case NESeq.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage } ->
        throwError (err400 ValidationFailed peMessage){ qePath = pePath }

    convertActionQuery
      :: ActionQuery UnpreparedValue -> StateT PlanningSt m ActionQueryPlan
    convertActionQuery = \case
      AQQuery s -> (AQPQuery . fst) <$>
        lift (resolveActionExecution s $ ActionExecContext manager reqHeaders usrVars)
      AQAsync s -> AQPAsyncQuery <$>
        DS.traverseAnnSimpleSel prepareWithPlan (resolveAsyncActionQuery userInfo s)

-- use the existing plan and new variables to create a pg query
queryOpFromPlan
  :: (HasVersion, MonadError QErr m)
  => HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> Maybe GH.VariableValues
  -> ReusableQueryPlan
  -> m (LazyRespTx, GeneratedSqlMap)
queryOpFromPlan manager reqHdrs userInfo varValsM (ReusableQueryPlan varTypes fldPlans) = do
  validatedVars <- _validateVariablesForReuse varTypes varValsM
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
  | RRActionQuery !LazyRespTx

-- | The computed SQL with alias which can be logged. Nothing here represents no
-- SQL for cases like introspection responses. Tuple of alias to a (maybe)
-- prepared statement
type GeneratedSqlMap = [(G.Name, Maybe PreparedSql)]

mkLazyRespTx :: HasVersion
  => HTTP.Manager -> [HTTP.Header] -> UserInfo -> [(G.Name, ResolvedQuery)] -> LazyRespTx
mkLazyRespTx manager reqHdrs userInfo resolved =
  fmap encJFromAssocList $ forM resolved $ \(alias, node) -> do
    resp <- case node of
      RRRaw bs                   -> return $ encJFromBS bs
      RRSql (PreparedSql q args maybeRemoteJoins) -> do
        let prepArgs = map fst args
        case maybeRemoteJoins of
          Nothing -> liftTx $ asSingleRowJsonResp q (map fst args)
          Just remoteJoins ->
            executeQueryWithRemoteJoins manager reqHdrs userInfo q prepArgs remoteJoins
      RRActionQuery tx           -> tx
    return (G.unName alias, resp)

mkGeneratedSqlMap :: [(G.Name, ResolvedQuery)] -> GeneratedSqlMap
mkGeneratedSqlMap resolved =
  flip map resolved $ \(alias, node) ->
    let res = case node of
                RRRaw _         -> Nothing
                RRSql ps        -> Just ps
                RRActionQuery _ -> Nothing
    in (alias, res)
