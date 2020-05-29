{-# LANGUAGE UndecidableInstances #-}

-- | Construction of multiplexed live query plans; see "Hasura.GraphQL.Execute.LiveQuery" for
-- details.
module Hasura.GraphQL.Execute.LiveQuery.Plan
  ( MultiplexedQuery
  , mkMultiplexedQuery
  , resolveMultiplexedValue

  , CohortId
  , newCohortId
  , CohortVariables
  , executeMultiplexedQuery

  , LiveQueryPlan(..)
  , ParameterizedLiveQueryPlan(..)
  , ReusableLiveQueryPlan
  , ValidatedQueryVariables
  , buildLiveQueryPlan
  , reuseLiveQueryPlan

  , LiveQueryPlanExplanation
  , explainLiveQueryPlan
  ) where

import           Hasura.Prelude
import           Hasura.Session

import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.Extended                    as J
import qualified Data.Aeson.TH                          as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T
import qualified Data.UUID.V4                           as UUID
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

-- remove these when array encoding is merged
import qualified Database.PG.Query.PTI                  as PTI
import qualified PostgreSQL.Binary.Encoding             as PE

import           Control.Lens
import           Data.Has
import           Data.UUID                              (UUID)

import qualified Hasura.GraphQL.Resolve                 as GR
import qualified Hasura.GraphQL.Resolve.Action          as RA
import qualified Hasura.GraphQL.Resolve.Types           as GR
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.GraphQL.Validate.Types          as GV
import qualified Hasura.SQL.DML                         as S

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Utils
import           Hasura.RQL.Types
import           Hasura.SQL.Error
import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Hasura.Server.Version             (HasVersion)

-- -------------------------------------------------------------------------------------------------
-- Multiplexed queries

newtype MultiplexedQuery = MultiplexedQuery { unMultiplexedQuery :: Q.Query }
  deriving (Show, Eq, Hashable, J.ToJSON)

mkMultiplexedQuery :: Map.HashMap G.Alias GR.QueryRootFldResolved -> MultiplexedQuery
mkMultiplexedQuery rootFields = MultiplexedQuery . Q.fromBuilder . toSQL $ S.mkSelect
  { S.selExtr =
    -- SELECT _subs.result_id, _fld_resp.root AS result
    [ S.Extractor (mkQualIden (Iden "_subs") (Iden "result_id")) Nothing
    , S.Extractor (mkQualIden (Iden "_fld_resp") (Iden "root")) (Just . S.Alias $ Iden "result") ]
  , S.selFrom = Just $ S.FromExp [S.FIJoin $
      S.JoinExpr subsInputFromItem S.LeftOuter responseLateralFromItem (S.JoinOn $ S.BELit True)]
  }
  where
    -- FROM unnest($1::uuid[], $2::json[]) _subs (result_id, result_vars)
    subsInputFromItem = S.FIUnnest
      [S.SEPrep 1 `S.SETyAnn` S.TypeAnn "uuid[]", S.SEPrep 2 `S.SETyAnn` S.TypeAnn "json[]"]
      (S.Alias $ Iden "_subs")
      [S.SEIden $ Iden "result_id", S.SEIden $ Iden "result_vars"]

    -- LEFT OUTER JOIN LATERAL ( ... ) _fld_resp
    responseLateralFromItem = S.mkLateralFromItem selectRootFields (S.Alias $ Iden "_fld_resp")
    selectRootFields = S.mkSelect
      { S.selExtr = [S.Extractor rootFieldsJsonAggregate (Just . S.Alias $ Iden "root")]
      , S.selFrom = Just . S.FromExp $
          flip map (Map.toList rootFields) $ \(fieldAlias, resolvedAST) ->
            S.mkSelFromItem (GR.toSQLSelect resolvedAST) (S.Alias $ aliasToIden fieldAlias)
      }

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    rootFieldsJsonAggregate = S.SEFnApp "json_build_object" rootFieldsJsonPairs Nothing
    rootFieldsJsonPairs = flip concatMap (Map.keys rootFields) $ \fieldAlias ->
      [ S.SELit (G.unName $ G.unAlias fieldAlias)
      , mkQualIden (aliasToIden fieldAlias) (Iden "root") ]

    mkQualIden prefix = S.SEQIden . S.QIden (S.QualIden prefix Nothing) -- TODO fix this Nothing of course
    aliasToIden = Iden . G.unName . G.unAlias

-- | Resolves an 'GR.UnresolvedVal' by converting 'GR.UVPG' values to SQL expressions that refer to
-- the @result_vars@ input object, collecting variable values along the way.
resolveMultiplexedValue
  :: (MonadState (GV.ReusableVariableValues, Seq (WithScalarType PGScalarValue)) m)
  => GR.UnresolvedVal -> m S.SQLExp
resolveMultiplexedValue = \case
  GR.UVPG annPGVal -> do
    let GR.AnnPGVal varM _ colVal = annPGVal
    varJsonPath <- case varM of
      Just varName -> do
        modifying _1 $ Map.insert varName colVal
        pure ["query", G.unName $ G.unVariable varName]
      Nothing -> do
        syntheticVarIndex <- gets (length . snd)
        modifying _2 (|> colVal)
        pure ["synthetic", T.pack $ show syntheticVarIndex]
    pure $ fromResVars (PGTypeScalar $ pstType colVal) varJsonPath
  GR.UVSessVar ty sessVar -> pure $ fromResVars ty ["session", sessionVariableToText sessVar]
  GR.UVSQL sqlExp -> pure sqlExp
  GR.UVSession -> pure $ fromResVars (PGTypeScalar PGJSON) ["session"]
  where
    fromResVars pgType jPath = addTypeAnnotation pgType $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIden $ S.QIden (S.QualIden (Iden "_subs") Nothing) (Iden "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]
    addTypeAnnotation pgType = flip S.SETyAnn (S.mkTypeAnn pgType) . case pgType of
      PGTypeScalar scalarType -> withConstructorFn scalarType
      PGTypeArray _           -> id

newtype CohortId = CohortId { unCohortId :: UUID }
  deriving (Show, Eq, Hashable, J.ToJSON, Q.FromCol)

newCohortId :: (MonadIO m) => m CohortId
newCohortId = CohortId <$> liftIO UUID.nextRandom

data CohortVariables
  = CohortVariables
  { _cvSessionVariables   :: !SessionVariables
  , _cvQueryVariables     :: !ValidatedQueryVariables
  , _cvSyntheticVariables :: !ValidatedSyntheticVariables
  -- ^ To allow more queries to be multiplexed together, we introduce “synthetic” variables for
  -- /all/ SQL literals in a query, even if they don’t correspond to any GraphQL variable. For
  -- example, the query
  --
  -- > subscription latest_tracks($condition: tracks_bool_exp!) {
  -- >   tracks(where: $tracks_bool_exp) {
  -- >     id
  -- >     title
  -- >   }
  -- > }
  --
  -- might be executed with similar values for @$condition@, such as @{"album_id": {"_eq": "1"}}@
  -- and @{"album_id": {"_eq": "2"}}@.
  --
  -- Normally, we wouldn’t bother parameterizing over the @1@ and @2@ literals in the resulting
  -- query because we can’t cache that query plan (since different @$condition@ values could lead to
  -- different SQL). However, for live queries, we can still take advantage of the similarity
  -- between the two queries by multiplexing them together, so we replace them with references to
  -- synthetic variables.
  } deriving (Show, Eq, Generic)
instance Hashable CohortVariables

instance J.ToJSON CohortVariables where
  toJSON (CohortVariables sessionVars queryVars syntheticVars) =
    J.object ["session" J..= sessionVars, "query" J..= queryVars, "synthetic" J..= syntheticVars]

-- These types exist only to use the Postgres array encoding.
newtype CohortIdArray = CohortIdArray { unCohortIdArray :: [CohortId] }
  deriving (Show, Eq)
instance Q.ToPrepArg CohortIdArray where
  toPrepVal (CohortIdArray l) = Q.toPrepValHelper PTI.unknown encoder $ map unCohortId l
    where
      encoder = PE.array 2950 . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)
newtype CohortVariablesArray = CohortVariablesArray { unCohortVariablesArray :: [CohortVariables] }
  deriving (Show, Eq)
instance Q.ToPrepArg CohortVariablesArray where
  toPrepVal (CohortVariablesArray l) =
    Q.toPrepValHelper PTI.unknown encoder (map J.toJSON l)
    where
      encoder = PE.array 114 . PE.dimensionArray foldl' (PE.encodingArray . PE.json_ast)

executeMultiplexedQuery
  :: (MonadTx m) => MultiplexedQuery -> [(CohortId, CohortVariables)] -> m [(CohortId, EncJSON)]
executeMultiplexedQuery (MultiplexedQuery query) = executeQuery query

-- | Internal; used by both 'executeMultiplexedQuery' and 'explainLiveQueryPlan'.
executeQuery :: (MonadTx m, Q.FromRow a) => Q.Query -> [(CohortId, CohortVariables)] -> m [a]
executeQuery query cohorts =
  let (cohortIds, cohortVars) = unzip cohorts
      preparedArgs = (CohortIdArray cohortIds, CohortVariablesArray cohortVars)
  in liftTx $ Q.listQE defaultTxErrorHandler query preparedArgs True

-- -------------------------------------------------------------------------------------------------
-- Variable validation

-- | When running multiplexed queries, we have to be especially careful about user input, since
-- invalid values will cause the query to fail, causing collateral damage for anyone else
-- multiplexed into the same query. Therefore, we pre-validate variables against Postgres by
-- executing a no-op query of the shape
--
-- > SELECT 'v1'::t1, 'v2'::t2, ..., 'vn'::tn
--
-- so if any variable values are invalid, the error will be caught early.
newtype ValidatedVariables f = ValidatedVariables (f TxtEncodedPGVal)
deriving instance (Show (f TxtEncodedPGVal)) => Show (ValidatedVariables f)
deriving instance (Eq (f TxtEncodedPGVal)) => Eq (ValidatedVariables f)
deriving instance (Hashable (f TxtEncodedPGVal)) => Hashable (ValidatedVariables f)
deriving instance (J.ToJSON (f TxtEncodedPGVal)) => J.ToJSON (ValidatedVariables f)

type ValidatedQueryVariables = ValidatedVariables (Map.HashMap G.Variable)
type ValidatedSyntheticVariables = ValidatedVariables []

-- | Checks if the provided arguments are valid values for their corresponding types.
-- Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateVariables
  :: (Traversable f, MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> f (WithScalarType PGScalarValue)
  -> m (ValidatedVariables f)
validateVariables pgExecCtx variableValues = do
  let valSel = mkValidationSel $ toList variableValues
  Q.Discard () <- runTx' $ liftTx $
    Q.rawQE dataExnErrHandler (Q.fromBuilder $ toSQL valSel) [] False
  pure . ValidatedVariables $ fmap (txtEncodedPGVal . pstValue) variableValues
  where
    mkExtrs = map (flip S.Extractor Nothing . toTxtValue)
    mkValidationSel vars =
      S.mkSelect { S.selExtr = mkExtrs vars }
    runTx' tx = do
      res <- liftIO $ runExceptT (runLazyTx' pgExecCtx tx)
      liftEither res

    -- Explicitly look for the class of errors raised when the format of a value provided
    -- for a type is incorrect.
    dataExnErrHandler = mkTxErrorHandler (has _PGDataException)

-- -------------------------------------------------------------------------------------------------
-- Live query plans

-- | A self-contained, ready-to-execute live query plan. Contains enough information to find an
-- existing poller that this can be added to /or/ to create a new poller if necessary.
data LiveQueryPlan
  = LiveQueryPlan
  { _lqpParameterizedPlan :: !ParameterizedLiveQueryPlan
  , _lqpVariables         :: !CohortVariables
  } deriving Show

data ParameterizedLiveQueryPlan
  = ParameterizedLiveQueryPlan
  { _plqpRole  :: !RoleName
  , _plqpQuery :: !MultiplexedQuery
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ParameterizedLiveQueryPlan)

data ReusableLiveQueryPlan
  = ReusableLiveQueryPlan
  { _rlqpParameterizedPlan       :: !ParameterizedLiveQueryPlan
  , _rlqpSyntheticVariableValues :: !ValidatedSyntheticVariables
  , _rlqpQueryVariableTypes      :: !GV.ReusableVariableTypes
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ReusableLiveQueryPlan)

-- | Constructs a new execution plan for a live query and returns a reusable version of the plan if
-- possible.
buildLiveQueryPlan
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has GR.FieldMap r
     , Has GR.OrdByCtx r
     , Has GR.QueryCtxMap r
     , Has SQLGenCtx r
     , HasVersion
     , MonadIO m
     )
  => PGExecCtx
  -> GV.QueryReusability
  -> RA.QueryActionExecuter
  -> GV.SelSet
  -> m (LiveQueryPlan, Maybe ReusableLiveQueryPlan)
buildLiveQueryPlan pgExecCtx initialReusability actionExecutioner fields = do
  ((resolvedASTs, (queryVariableValues, syntheticVariableValues)), finalReusability) <-
    GV.runReusabilityTWith initialReusability . flip runStateT mempty $
      fmap Map.fromList . for (toList fields) $ \field -> case GV._fName field of
        "__typename" -> throwVE "you cannot create a subscription on '__typename' field"
        _ -> do
          unresolvedAST <- GR.queryFldToPGAST field actionExecutioner
          resolvedAST <- GR.traverseQueryRootFldAST resolveMultiplexedValue unresolvedAST

          let (_, remoteJoins) = GR.toPGQuery resolvedAST
          -- Reject remote relationships in subscription live query
          when (remoteJoins /= mempty) $
               throw400 NotSupported
                       "Remote relationships are not allowed in subscriptions"
          pure (GV._fAlias field, resolvedAST)

  userInfo <- asks getter
  let multiplexedQuery = mkMultiplexedQuery resolvedASTs
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedLiveQueryPlan roleName multiplexedQuery

  -- We need to ensure that the values provided for variables are correct according to Postgres.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  validatedQueryVars <- validateVariables pgExecCtx queryVariableValues
  validatedSyntheticVars <- validateVariables pgExecCtx (toList syntheticVariableValues)
  let cohortVariables = CohortVariables (_uiSession userInfo) validatedQueryVars validatedSyntheticVars
      plan = LiveQueryPlan parameterizedPlan cohortVariables
      varTypes = finalReusability ^? GV._Reusable
      reusablePlan = ReusableLiveQueryPlan parameterizedPlan validatedSyntheticVars <$> varTypes
  pure (plan, reusablePlan)

reuseLiveQueryPlan
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> SessionVariables
  -> Maybe GH.VariableValues
  -> ReusableLiveQueryPlan
  -> m LiveQueryPlan
reuseLiveQueryPlan pgExecCtx sessionVars queryVars reusablePlan = do
  let ReusableLiveQueryPlan parameterizedPlan syntheticVars queryVarTypes = reusablePlan
  annVarVals <- GV.validateVariablesForReuse queryVarTypes queryVars
  validatedVars <- validateVariables pgExecCtx annVarVals
  pure $ LiveQueryPlan parameterizedPlan (CohortVariables sessionVars validatedVars syntheticVars)

data LiveQueryPlanExplanation
  = LiveQueryPlanExplanation
  { _lqpeSql  :: !Text
  , _lqpePlan :: ![Text]
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''LiveQueryPlanExplanation)

explainLiveQueryPlan :: (MonadTx m, MonadIO m) => LiveQueryPlan -> m LiveQueryPlanExplanation
explainLiveQueryPlan plan = do
  let parameterizedPlan = _lqpParameterizedPlan plan
      queryText = Q.getQueryText . unMultiplexedQuery $ _plqpQuery parameterizedPlan
      explainQuery = Q.fromText $ "EXPLAIN (FORMAT TEXT) " <> queryText
  cohortId <- newCohortId
  explanationLines <- map runIdentity <$> executeQuery explainQuery [(cohortId, _lqpVariables plan)]
  pure $ LiveQueryPlanExplanation queryText explanationLines
