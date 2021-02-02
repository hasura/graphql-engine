module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  -- , queryOpFromPlan
  -- , ReusableQueryPlan
  , PreparedSql(..)
  , traverseQueryRootField -- for live query planning
  , parseGraphQLQuery

  , MonadQueryInstrumentation(..)
  , ExtractProfile(..)
  , noProfile
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.Sequence.NonEmpty                 as NESeq
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.Logging                         as L
import qualified Hasura.RQL.IR.Select                   as DS
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.Metadata.Class
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session


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
--       let prepVal = (binEncoder colVal, pstValue colVal)
--       return $ IntMap.insert prepNo prepVal accum


traverseQueryRootField
  :: forall f a b c d h backend
   . Applicative f
  => (a -> f b)
  -> RootField (QueryDB backend a) c h d
  -> f (RootField (QueryDB backend b) c h d)
traverseQueryRootField f = traverseDB \case
  QDBSimple s      -> QDBSimple      <$> DS.traverseAnnSimpleSelect f s
  QDBPrimaryKey s  -> QDBPrimaryKey  <$> DS.traverseAnnSimpleSelect f s
  QDBAggregation s -> QDBAggregation <$> DS.traverseAnnAggregateSelect f s
  QDBConnection s  -> QDBConnection  <$> DS.traverseConnectionSelect f s

parseGraphQLQuery
  :: MonadError QErr m
  => GQLContext
  -> [G.VariableDefinition]
  -> Maybe (HashMap G.Name J.Value)
  -> G.SelectionSet G.NoFragments G.Name
  -> m ( InsOrdHashMap G.Name (QueryRootField (UnpreparedValue 'Postgres))
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
instance MonadQueryInstrumentation m => MonadQueryInstrumentation (MetadataStorageT m)

convertQuerySelSet
  :: forall m tx .
     ( MonadError QErr m
     , HasVersion
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadQueryInstrumentation m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> GQLContext
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m ( ExecutionPlan ActionExecutionPlan (tx EncJSON, Maybe PreparedSql)
       -- , Maybe ReusableQueryPlan
       , [QueryRootField (UnpreparedValue 'Postgres)]
       )
convertQuerySelSet env logger gqlContext userInfo manager reqHeaders directives fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability) <- parseGraphQLQuery gqlContext varDefs varValsM fields

  -- Transform the RQL AST into a prepared SQL query
  queryPlan <- for unpreparedQueries \unpreparedQuery -> do
    (preparedQuery, PlanningSt _ _ planVals expectedVariables)
      <- flip runStateT initPlanningSt
         $ traverseQueryRootField prepareWithPlan unpreparedQuery
           >>= traverseRemoteField (resolveRemoteField userInfo)
    validateSessionVariables expectedVariables $ _uiSession userInfo
    traverseDB (pure . irToRootFieldPlan planVals) preparedQuery

  (instrument, ep) <- askInstrumentQuery directives

  -- Transform the query plans into an execution plan
  executionPlan <- forM queryPlan  \case
        RFRemote (RemoteFieldG remoteSchemaInfo remoteField) -> pure $
          buildExecStepRemote
            remoteSchemaInfo
            G.OperationTypeQuery
            [G.SelectionField remoteField]
        RFDB _ e db          -> pure $ ExecStepDB e $ mkCurPlanTx env manager reqHeaders userInfo instrument ep (RFPPostgres db)
        RFAction (AQQuery s) -> ExecStepAction . AEPSync . _aerExecution <$>
                                resolveActionExecution env logger userInfo s (ActionExecContext manager reqHeaders usrVars)
        RFAction (AQAsync s) -> pure $ ExecStepAction $ AEPAsyncQuery (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s
        RFRaw r              -> pure $ ExecStepRaw r

  let asts :: [QueryRootField (UnpreparedValue 'Postgres)]
      asts = OMap.elems unpreparedQueries
  pure (executionPlan, asts)  -- See Note [Temporarily disabling query plan caching]
  where
    usrVars = _uiSession userInfo

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
