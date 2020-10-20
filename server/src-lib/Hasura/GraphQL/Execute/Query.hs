module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  -- , queryOpFromPlan
  -- , ReusableQueryPlan
  , PreparedSql(..)
  , traverseQueryRootField -- for live query planning
  , irToRootFieldPlan
  , parseGraphQLQuery

  , MonadQueryInstrumentation(..)
  , ExtractProfile(..)
  , noProfile
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.IntMap                            as IntMap
import qualified Data.Sequence.NonEmpty                 as NESeq
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.Logging                         as L
import qualified Hasura.SQL.DML                         as S
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Class
import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Execute.Types
import           Hasura.GraphQL.Parser
import           Hasura.Prelude
import           Hasura.RQL.DML.RemoteJoin
import           Hasura.RQL.DML.Select                  (asSingleRowJsonResp)
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Value

import qualified Hasura.RQL.DML.Select                  as DS

data PreparedSql
  = PreparedSql
  { _psQuery       :: !Q.Query
  , _psPrepArgs    :: !PrepArgMap
  , _psRemoteJoins :: !(Maybe RemoteJoins)
  }
  deriving Show

-- | Required to log in `query-log`
instance J.ToJSON PreparedSql where
  toJSON (PreparedSql q prepArgs _) =
    J.object [ "query" J..= Q.getQueryText q
             , "prepared_arguments" J..= fmap (pgScalarValueToJson . snd) prepArgs
             ]

data RootFieldPlan
  = RFPPostgres !PreparedSql
  | RFPActionQuery !ActionExecution

instance J.ToJSON RootFieldPlan where
  toJSON = \case
    RFPPostgres pgPlan -> J.toJSON pgPlan
    RFPActionQuery _   -> J.String "Action Execution Tx"

data ActionQueryPlan
  = AQPAsyncQuery !DS.AnnSimpleSel -- ^ Cacheable plan
  | AQPQuery !ActionExecution -- ^ Non cacheable transaction

actionQueryToRootFieldPlan
  :: PrepArgMap -> ActionQueryPlan -> RootFieldPlan
actionQueryToRootFieldPlan prepped = \case
  AQPAsyncQuery s -> RFPPostgres $
    PreparedSql (DS.selectQuerySQL DS.JASSingleObject s) prepped Nothing
  AQPQuery tx     -> RFPActionQuery tx

-- See Note [Temporarily disabling query plan caching]
-- data ReusableVariableTypes
-- data ReusableVariableValues

-- data ReusableQueryPlan
--   = ReusableQueryPlan
--   { _rqpVariableTypes :: !ReusableVariableTypes
--   , _rqpFldPlans      :: !FieldPlans
--   }

-- instance J.ToJSON ReusableQueryPlan where
--   toJSON (ReusableQueryPlan varTypes fldPlans) =
--     J.object [ "variables"       J..= () -- varTypes
--              , "field_plans"     J..= fldPlans
--              ]

-- withPlan
--   :: (MonadError QErr m)
--   => SessionVariables -> PGPlan -> HashMap G.Name (WithScalarType PGScalarValue) -> m PreparedSql
-- withPlan usrVars (PGPlan q reqVars prepMap remoteJoins) annVars = do
--   prepMap' <- foldM getVar prepMap (Map.toList reqVars)
--   let args = withUserVars usrVars $ IntMap.elems prepMap'
--   return $ PreparedSql q args remoteJoins
--   where
--     getVar accum (var, prepNo) = do
--       let varName = G.unName var
--       colVal <- onNothing (Map.lookup var annVars) $
--         throw500 $ "missing variable in annVars : " <> varName
--       let prepVal = (toBinaryValue colVal, pstValue colVal)
--       return $ IntMap.insert prepNo prepVal accum

-- turn the current plan into a transaction
mkCurPlanTx
  :: ( HasVersion
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> HTTP.Manager
  -> [HTTP.Header]
  -> UserInfo
  -> (Q.Query -> Q.Query)
  -> ExtractProfile
  -> PreparedSql
  -> (tx EncJSON, Maybe PreparedSql)
mkCurPlanTx env manager reqHdrs userInfo instrument ep ps = do
  -- generate the SQL and prepared vars or the bytestring
  let args = withUserVars (_uiSession userInfo) prepMap
      -- WARNING: this quietly assumes the intmap keys are contiguous
      prepArgs = fst <$> IntMap.elems args
  (, Just ps) $ case remoteJoinsM of
    Nothing -> do
      Tracing.trace "Postgres" $ runExtractProfile ep =<< liftTx do
        asSingleRowJsonResp (instrument q) prepArgs
    Just remoteJoins ->
      executeQueryWithRemoteJoins env manager reqHdrs userInfo q prepArgs remoteJoins
  where
    PreparedSql q prepMap remoteJoinsM = ps

-- convert a query from an intermediate representation to... another
irToRootFieldPlan
  :: PrepArgMap
  -> QueryDB S.SQLExp -> PreparedSql
irToRootFieldPlan prepped = \case
  QDBSimple s      -> mkPreparedSql getRemoteJoins (DS.selectQuerySQL DS.JASMultipleRows) s
  QDBPrimaryKey s  -> mkPreparedSql getRemoteJoins (DS.selectQuerySQL DS.JASSingleObject) s
  QDBAggregation s -> mkPreparedSql getRemoteJoinsAggregateSelect DS.selectAggregateQuerySQL s
  QDBConnection s  -> mkPreparedSql getRemoteJoinsConnectionSelect DS.connectionSelectQuerySQL s
  where
    mkPreparedSql :: (s -> (t, Maybe RemoteJoins)) -> (t -> Q.Query) -> s -> PreparedSql
    mkPreparedSql getJoins f simpleSel =
      let (simpleSel',remoteJoins) = getJoins simpleSel
      in PreparedSql (f simpleSel') prepped remoteJoins

traverseQueryRootField
  :: forall f a b c d h
   . Applicative f
  => (a -> f b)
  -> RootField (QueryDB a) c h d
  -> f (RootField (QueryDB b) c h d)
traverseQueryRootField f = traverseDB \case
  QDBSimple s       -> QDBSimple      <$> DS.traverseAnnSimpleSelect f s
  QDBPrimaryKey s   -> QDBPrimaryKey  <$> DS.traverseAnnSimpleSelect f s
  QDBAggregation s  -> QDBAggregation <$> DS.traverseAnnAggregateSelect f s
  QDBConnection s   -> QDBConnection  <$> DS.traverseConnectionSelect f s

parseGraphQLQuery
  :: MonadError QErr m
  => GQLContext
  -> [G.VariableDefinition]
  -> Maybe (HashMap G.Name J.Value)
  -> G.SelectionSet G.NoFragments G.Name
  -> m ( InsOrdHashMap G.Name (QueryRootField UnpreparedValue)
       , QueryReusability
       )
parseGraphQLQuery gqlContext varDefs varValsM fields =
  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
  >>= (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors))
  where
    reportParseErrors errs = case NESeq.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage, peCode } ->
        throwError (err400 peCode peMessage){ qePath = pePath }

-- | A method for extracting profiling data from instrumented query results.
newtype ExtractProfile = ExtractProfile
  { runExtractProfile :: forall m. (MonadIO m, Tracing.MonadTrace m) => EncJSON -> m EncJSON
  }

-- | A default implementation for queries with no instrumentation
noProfile :: ExtractProfile
noProfile = ExtractProfile pure

-- | Monads which support query instrumentation
class Monad m => MonadQueryInstrumentation m where
  -- | Returns the appropriate /instrumentation/ (if any) for a SQL query, as
  -- requested by the provided directives. Instrumentation is “SQL middleware”:
  --
  --   * The @'Q.Query' -> 'Q.Query'@ function is applied to the query just
  --     prior to execution, and it can adjust the query in arbitrary ways.
  --
  --   * The 'ExtractProfile' function is applied to the query /result/, and it
  --     can perform arbitrary side-effects based on its contents. (This is
  --     currently just a hook for tracing, a la 'Tracing.MonadTrace'.)
  --
  -- The open-source version of graphql-engine does not currently perform any
  -- instrumentation, so this serves only as a hook for downstream clients.
  askInstrumentQuery
    :: [G.Directive G.Name]
    -> m (Q.Query -> Q.Query, ExtractProfile)

  -- A default for monad transformer instances
  default askInstrumentQuery
    :: (m ~ t n, MonadTrans t, MonadQueryInstrumentation n)
    => [G.Directive G.Name]
    -> m (Q.Query -> Q.Query, ExtractProfile)
  askInstrumentQuery = lift . askInstrumentQuery

instance MonadQueryInstrumentation m => MonadQueryInstrumentation (ReaderT r m)
instance MonadQueryInstrumentation m => MonadQueryInstrumentation (ExceptT e m)
instance MonadQueryInstrumentation m => MonadQueryInstrumentation (Tracing.TraceT m)

convertQuerySelSet
  :: forall m tx .
     ( MonadError QErr m
     , HasVersion
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadQueryInstrumentation m
     , MonadMetadataStorage m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> Q.PGPool
  -> L.Logger L.Hasura
  -> GQLContext
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m ( ExecutionPlan (tx EncJSON, Maybe PreparedSql) ActionExecution
       -- , Maybe ReusableQueryPlan
       , [QueryRootField UnpreparedValue]
       )
convertQuerySelSet env metadataPool logger gqlContext userInfo manager reqHeaders directives fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability) <- parseGraphQLQuery gqlContext varDefs varValsM fields

  -- Transform the RQL AST into a prepared SQL query
  queryPlan <- for unpreparedQueries \unpreparedQuery -> do
    (preparedQuery, PlanningSt _ _ planVals expectedVariables)
      <- flip runStateT initPlanningSt
         $ traverseQueryRootField prepareWithPlan unpreparedQuery
           >>= traverseAction (lift . convertActionQuery)
    validateSessionVariables expectedVariables $ _uiSession userInfo
    traverseDB (pure . irToRootFieldPlan planVals) preparedQuery
      >>= traverseAction (pure . actionQueryToRootFieldPlan planVals)

  (instrument, ep) <- askInstrumentQuery directives

  -- Transform the query plans into an execution plan
  let executionPlan = queryPlan <&> \case
        RFRemote (remoteSchemaInfo, remoteField) ->
          buildExecStepRemote
            remoteSchemaInfo
            G.OperationTypeQuery
            varDefs
            [G.SelectionField remoteField]
            varValsM
        RFDB pgExecCtx db -> ExecStepDB pgExecCtx $
                             mkCurPlanTx env manager reqHeaders userInfo instrument ep db
        RFAction (RFPPostgres db) ->
          let metadataPgExecCtx = mkPGExecCtx Q.ReadCommitted metadataPool
          -- FIXME:- always using metadata storage for action queries
          in ExecStepDB metadataPgExecCtx $
             mkCurPlanTx env manager reqHeaders userInfo instrument ep db
        RFAction (RFPActionQuery action) -> ExecStepAction action
        RFRaw r           -> ExecStepRaw r

  let asts :: [QueryRootField UnpreparedValue]
      asts = OMap.elems unpreparedQueries
  pure (executionPlan, asts)  -- See Note [Temporarily disabling query plan caching]
  where
    usrVars = _uiSession userInfo

    convertActionQuery
      :: ActionQuery UnpreparedValue -> m ActionQueryPlan
    convertActionQuery = \case
      AQQuery s -> do
        result <- resolveActionExecution env logger userInfo metadataPool s $ ActionExecContext manager reqHeaders usrVars
        pure $ AQPQuery $ _aerExecution result
      AQAsync s -> do
        unpreparedAst <- resolveAsyncActionQuery userInfo s
        (resolved, _) <- flip runStateT mempty $
          DS.traverseAnnSimpleSelect prepareWithoutPlan unpreparedAst
        -- let tx = asSingleRowJsonResp (Q.fromBuilder $ toSQL $ DS.mkSQLSelect DS.JASSingleObject resolved) []
        -- -- async action queries should be run in metadata storage
        -- r <-  liftEither =<< (liftIO $ runExceptT $ Q.runTx' metadataPool tx)
        pure $ AQPAsyncQuery resolved

-- See Note [Temporarily disabling query plan caching]
-- use the existing plan and new variables to create a pg query
-- queryOpFromPlan
--   :: ( HasVersion
--      , MonadError QErr m
--      , Tracing.MonadTrace m
--      , MonadIO tx
--      , MonadTx tx
--      , Tracing.MonadTrace tx
--      )
--   => Env.Environment
--   -> HTTP.Manager
--   -> [HTTP.Header]
--   -> UserInfo
--   -> Maybe GH.VariableValues
--   -> ReusableQueryPlan
--   -> m (tx EncJSON, GeneratedSqlMap)
-- queryOpFromPlan env  manager reqHdrs userInfo varValsM (ReusableQueryPlan varTypes fldPlans) = do
--   validatedVars <- _validateVariablesForReuse varTypes varValsM
--   -- generate the SQL and prepared vars or the bytestring
--   resolved <- forM fldPlans $ \(alias, fldPlan) ->
--     (alias,) <$> case fldPlan of
--       RFPRaw resp        -> return $ RRRaw resp
--       RFPPostgres pgPlan -> RRSql <$> withPlan (_uiSession userInfo) pgPlan validatedVars

--   (,) <$> mkLazyRespTx env manager reqHdrs userInfo resolved <*> pure (mkGeneratedSqlMap resolved)
