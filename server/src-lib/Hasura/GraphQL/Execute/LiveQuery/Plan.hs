{-# LANGUAGE UndecidableInstances #-}

-- | Construction of multiplexed live query plans; see
-- "Hasura.GraphQL.Execute.LiveQuery" for details.
module Hasura.GraphQL.Execute.LiveQuery.Plan
  ( MultiplexedQuery
  , mkMultiplexedQuery
  , unMultiplexedQuery
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
  -- , reuseLiveQueryPlan

  , LiveQueryPlanExplanation
  , explainLiveQueryPlan
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.Extended                         as J
import qualified Data.ByteString                             as B
import qualified Data.HashSet                                as Set
import qualified Data.Aeson.TH                               as J
import qualified Data.HashMap.Strict                         as Map
import qualified Data.HashMap.Strict.InsOrd                  as OMap
import qualified Data.Text                                   as T
import qualified Data.UUID.V4                                as UUID
import qualified Database.PG.Query                           as Q
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified Database.PG.Query.PTI                       as PTI
import qualified PostgreSQL.Binary.Encoding                  as PE

import           Control.Lens
import           Data.UUID                                   (UUID)
import           Data.Semigroup.Generic

import qualified Hasura.Backends.Postgres.Execute.RemoteJoin as RR
import qualified Hasura.Backends.Postgres.SQL.DML            as S
import qualified Hasura.Backends.Postgres.Translate.Select   as DS
import qualified Hasura.GraphQL.Parser.Schema                as PS
import qualified Hasura.RQL.IR.Select                        as DS

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Error
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Query
import           Hasura.GraphQL.Parser.Column
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Session


-- -------------------------------------------------------------------------------------------------
-- Multiplexed queries

newtype MultiplexedQuery = MultiplexedQuery { unMultiplexedQuery :: Q.Query }
  deriving (Show, Eq, Hashable, J.ToJSON)

toSQLFromItem :: S.Alias -> SubscriptionRootFieldResolved -> S.FromItem
toSQLFromItem alias = \case
  RFDB (QDBPrimaryKey s)  -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  RFDB (QDBSimple s)      -> fromSelect $ DS.mkSQLSelect DS.JASMultipleRows s
  RFDB (QDBAggregation s) -> fromSelect $ DS.mkAggregateSelect s
  RFDB (QDBConnection s)  -> S.mkSelectWithFromItem (DS.mkConnectionSelect s) alias
  RFAction s              -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  where
    fromSelect s = S.mkSelFromItem s alias

mkMultiplexedQuery :: OMap.InsOrdHashMap G.Name SubscriptionRootFieldResolved -> MultiplexedQuery
mkMultiplexedQuery rootFields = MultiplexedQuery . Q.fromBuilder . toSQL $ S.mkSelect
  { S.selExtr =
    -- SELECT _subs.result_id, _fld_resp.root AS result
    [ S.Extractor (mkQualifiedIdentifier (Identifier "_subs") (Identifier "result_id")) Nothing
    , S.Extractor (mkQualifiedIdentifier (Identifier "_fld_resp") (Identifier "root")) (Just . S.Alias $ Identifier "result") ]
  , S.selFrom = Just $ S.FromExp [S.FIJoin $
      S.JoinExpr subsInputFromItem S.LeftOuter responseLateralFromItem (S.JoinOn $ S.BELit True)]
  }
  where
    -- FROM unnest($1::uuid[], $2::json[]) _subs (result_id, result_vars)
    subsInputFromItem = S.FIUnnest
      [S.SEPrep 1 `S.SETyAnn` S.TypeAnn "uuid[]", S.SEPrep 2 `S.SETyAnn` S.TypeAnn "json[]"]
      (S.Alias $ Identifier "_subs")
      [S.SEIdentifier $ Identifier "result_id", S.SEIdentifier $ Identifier "result_vars"]

    -- LEFT OUTER JOIN LATERAL ( ... ) _fld_resp
    responseLateralFromItem = S.mkLateralFromItem selectRootFields (S.Alias $ Identifier "_fld_resp")
    selectRootFields = S.mkSelect
      { S.selExtr = [S.Extractor rootFieldsJsonAggregate (Just . S.Alias $ Identifier "root")]
      , S.selFrom = Just . S.FromExp $
          flip map (OMap.toList rootFields) $ \(fieldAlias, resolvedAST) ->
            toSQLFromItem (S.Alias $ aliasToIdentifier fieldAlias) resolvedAST
      }

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    rootFieldsJsonAggregate = S.SEFnApp "json_build_object" rootFieldsJsonPairs Nothing
    rootFieldsJsonPairs = flip concatMap (OMap.keys rootFields) $ \fieldAlias ->
      [ S.SELit (G.unName fieldAlias)
      , mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "root") ]

    mkQualifiedIdentifier prefix = S.SEQIdentifier . S.QIdentifier (S.QualifiedIdentifier prefix Nothing) -- TODO fix this Nothing of course
    aliasToIdentifier = Identifier . G.unName

newtype CohortId = CohortId { unCohortId :: UUID }
  deriving (Show, Eq, Hashable, J.ToJSON, Q.FromCol)

newCohortId :: (MonadIO m) => m CohortId
newCohortId = CohortId <$> liftIO UUID.nextRandom

data CohortVariables
  = CohortVariables
  { _cvSessionVariables   :: !SessionVariables
-- ^ A set of session variables, pruned to the minimal set actually used by
-- this query. To illustrate the need for this pruning, suppose we have the
-- following query:
--
-- > query {
-- >   articles {
-- >     id
-- >     title
-- >   }
-- > }
--
-- If the select permission on @articles@ is just @{"is_public": true}@, we
-- just generate the SQL query
--
-- > SELECT id, title FROM articles WHERE is_public = true
--
-- which doesn’t use any session variables at all. Therefore, we ought to be
-- able to multiplex all queries of this shape into a single cohort, for quite
-- good performance! But if we don’t prune the session variables, we’ll
-- needlessly split subscribers into several cohorts simply because they have
-- different values for, say, @X-Hasura-User-Id@.
--
-- The 'mkCohortVariables' smart constructor handles pruning the session
-- variables to a minimal set, avoiding this pessimization.
  , _cvQueryVariables     :: !ValidatedQueryVariables
  , _cvSyntheticVariables :: !ValidatedSyntheticVariables
  -- ^ To allow more queries to be multiplexed together, we introduce “synthetic”
  -- variables for /all/ SQL literals in a query, even if they don’t correspond to
  -- any GraphQL variable. For example, the query
  --
  -- > subscription latest_tracks($condition: tracks_bool_exp!) {
  -- >   tracks(where: $tracks_bool_exp) {
  -- >     id
  -- >     title
  -- >   }
  -- > }
  --
  -- might be executed with similar values for @$condition@, such as @{"album_id":
  -- {"_eq": "1"}}@ and @{"album_id": {"_eq": "2"}}@.
  --
  -- Normally, we wouldn’t bother parameterizing over the @1@ and @2@ literals in the
  -- resulting query because we can’t cache that query plan (since different
  -- @$condition@ values could lead to different SQL). However, for live queries, we
  -- can still take advantage of the similarity between the two queries by
  -- multiplexing them together, so we replace them with references to synthetic
  -- variables.
  } deriving (Show, Eq, Generic)
instance Hashable CohortVariables

-- | Builds a cohort's variables by only using the session variables that
-- are required for the subscription
mkCohortVariables
  :: Set.HashSet SessionVariable
  -> SessionVariables
  -> ValidatedQueryVariables
  -> ValidatedSyntheticVariables
  -> CohortVariables
mkCohortVariables requiredSessionVariables sessionVariableValues =
  CohortVariables $ filterSessionVariables (\k _ -> Set.member k requiredSessionVariables)
  sessionVariableValues

instance J.ToJSON CohortVariables where
  toJSON (CohortVariables sessionVars queryVars syntheticVars) =
    J.object [ "session" J..= sessionVars
             , "query" J..= queryVars
             , "synthetic" J..= syntheticVars
             ]

-- These types exist only to use the Postgres array encoding.
newtype CohortIdArray = CohortIdArray { unCohortIdArray :: [CohortId] }
  deriving (Show, Eq)

instance Q.ToPrepArg CohortIdArray where
  toPrepVal (CohortIdArray l) = Q.toPrepValHelper PTI.unknown encoder $ map unCohortId l
    where
      encoder = PE.array 2950 . PE.dimensionArray foldl' (PE.encodingArray . PE.uuid)

newtype CohortVariablesArray
  = CohortVariablesArray { unCohortVariablesArray :: [CohortVariables] }
  deriving (Show, Eq)

instance Q.ToPrepArg CohortVariablesArray where
  toPrepVal (CohortVariablesArray l) =
    Q.toPrepValHelper PTI.unknown encoder (map J.toJSON l)
    where
      encoder = PE.array 114 . PE.dimensionArray foldl' (PE.encodingArray . PE.json_ast)

executeMultiplexedQuery
  :: (MonadTx m)
  => MultiplexedQuery -> [(CohortId, CohortVariables)] -> m [(CohortId, B.ByteString)]
executeMultiplexedQuery (MultiplexedQuery query) = executeQuery query

-- | Internal; used by both 'executeMultiplexedQuery' and 'explainLiveQueryPlan'.
executeQuery
  :: (MonadTx m, Q.FromRow a)
  => Q.Query -> [(CohortId, CohortVariables)] -> m [a]
executeQuery query cohorts =
  let (cohortIds, cohortVars) = unzip cohorts
      preparedArgs = (CohortIdArray cohortIds, CohortVariablesArray cohortVars)
  in liftTx $ Q.listQE defaultTxErrorHandler query preparedArgs True

-- ---------------------------------------------------------------------------
-- Variable validation

-- | When running multiplexed queries, we have to be especially careful about user
-- input, since invalid values will cause the query to fail, causing collateral
-- damage for anyone else multiplexed into the same query.  Therefore, we
-- pre-validate variables against Postgres by executing a no-op query of the shape
--
-- > SELECT 'v1'::t1, 'v2'::t2, ..., 'vn'::tn
--
-- so if any variable values are invalid, the error will be caught early.
newtype ValidatedVariables f = ValidatedVariables (f TxtEncodedPGVal)
deriving instance (Show (f TxtEncodedPGVal)) => Show (ValidatedVariables f)
deriving instance (Eq (f TxtEncodedPGVal)) => Eq (ValidatedVariables f)
deriving instance (Hashable (f TxtEncodedPGVal)) => Hashable (ValidatedVariables f)
deriving instance (J.ToJSON (f TxtEncodedPGVal)) => J.ToJSON (ValidatedVariables f)

type ValidatedQueryVariables = ValidatedVariables (Map.HashMap G.Name)
type ValidatedSyntheticVariables = ValidatedVariables []

-- | Checks if the provided arguments are valid values for their corresponding types.
-- | Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateVariables
  :: (Traversable f, MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> f (WithScalarType PGScalarValue)
  -> m (ValidatedVariables f)
validateVariables pgExecCtx variableValues = do
  let valSel = mkValidationSel $ toList variableValues
  Q.Discard () <- runQueryTx_ $ liftTx $
    Q.rawQE dataExnErrHandler (Q.fromBuilder $ toSQL valSel) [] False
  pure . ValidatedVariables $ fmap (txtEncodedPGVal . pstValue) variableValues
  where
    mkExtrs = map (flip S.Extractor Nothing . toTxtValue)
    mkValidationSel vars =
      S.mkSelect { S.selExtr = mkExtrs vars }
    runQueryTx_ tx = do
      res <- liftIO $ runExceptT (runQueryTx pgExecCtx tx)
      liftEither res

    -- Explicitly look for the class of errors raised when the format of a value
    -- provided for a type is incorrect.
    dataExnErrHandler = mkTxErrorHandler (has _PGDataException)

-- | Internal: Used to collect information about various parameters
-- of a subscription field's AST as we resolve them to SQL expressions.
data QueryParametersInfo
  = QueryParametersInfo
  { _qpiReusableVariableValues     :: !(HashMap G.Name PGColumnValue)
  , _qpiSyntheticVariableValues    :: !(Seq PGColumnValue)
  , _qpiReferencedSessionVariables :: !(Set.HashSet SessionVariable)
  -- ^ The session variables that are referenced in the query root fld's AST.
  -- This information is used to determine a cohort's required session
  -- variables
  } deriving (Generic)
    deriving (Semigroup, Monoid) via (GenericSemigroupMonoid QueryParametersInfo)

makeLenses ''QueryParametersInfo

-- | Resolves an 'GR.UnresolvedVal' by converting 'GR.UVPG' values to SQL
-- expressions that refer to the @result_vars@ input object, collecting information
-- about various parameters of the query along the way.
resolveMultiplexedValue
  :: (MonadState QueryParametersInfo m)
  => UnpreparedValue -> m S.SQLExp
resolveMultiplexedValue = \case
  UVParameter colVal varM -> do
    varJsonPath <- case fmap PS.getName varM of
      Just varName -> do
        modifying qpiReusableVariableValues $ Map.insert varName colVal
        pure ["query", G.unName  varName]
      Nothing -> do
        syntheticVarIndex <- use (qpiSyntheticVariableValues . to length)
        modifying qpiSyntheticVariableValues (|> colVal)
        pure ["synthetic", T.pack $ show syntheticVarIndex]
    pure $ fromResVars (PGTypeScalar $ pstType $ pcvValue colVal) varJsonPath
  UVSessionVar ty sessVar -> do
    modifying qpiReferencedSessionVariables (Set.insert sessVar)
    pure $ fromResVars ty ["session", sessionVariableToText sessVar]
  UVLiteral sqlExp -> pure sqlExp
  UVSession -> pure $ fromResVars (PGTypeScalar PGJSON) ["session"]
  where
    fromResVars pgType jPath = addTypeAnnotation pgType $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIdentifier $ S.QIdentifier (S.QualifiedIdentifier (Identifier "_subs") Nothing) (Identifier "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]
    addTypeAnnotation pgType = flip S.SETyAnn (S.mkTypeAnn pgType) . case pgType of
      PGTypeScalar scalarType -> withConstructorFn scalarType
      PGTypeArray _           -> id

-- -----------------------------------------------------------------------------------
-- Live query plans

-- | A self-contained, ready-to-execute live query plan. Contains enough information
-- to find an existing poller that this can be added to /or/ to create a new poller
-- if necessary.
data LiveQueryPlan
  = LiveQueryPlan
  { _lqpParameterizedPlan :: !ParameterizedLiveQueryPlan
  , _lqpVariables         :: !CohortVariables
  }

data ParameterizedLiveQueryPlan
  = ParameterizedLiveQueryPlan
  { _plqpRole  :: !RoleName
  , _plqpQuery :: !MultiplexedQuery
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ParameterizedLiveQueryPlan)

data ReusableLiveQueryPlan
  = ReusableLiveQueryPlan
  { _rlqpParameterizedPlan        :: !ParameterizedLiveQueryPlan
  , _rlqpRequiredSessionVariables :: !(Set.HashSet SessionVariable)
  , _rlqpSyntheticVariableValues  :: !ValidatedSyntheticVariables
  , _rlqpQueryVariableTypes       :: HashMap G.Name PGColumnType
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ReusableLiveQueryPlan)

-- | Constructs a new execution plan for a live query and returns a reusable version
-- of the plan if possible.
buildLiveQueryPlan
  :: ( MonadError QErr m
     , MonadIO m
     )
  => PGExecCtx
  -> UserInfo
  -> InsOrdHashMap G.Name (SubscriptionRootField UnpreparedValue)
  -> m (LiveQueryPlan, Maybe ReusableLiveQueryPlan)
buildLiveQueryPlan pgExecCtx userInfo unpreparedAST = do
  (preparedAST, QueryParametersInfo{..}) <- flip runStateT mempty $
    for unpreparedAST \unpreparedQuery -> do
      resolvedRootField <- traverseQueryRootField resolveMultiplexedValue unpreparedQuery
      case resolvedRootField of
        RFDB qDB   -> do
          let remoteJoins = case qDB of
                QDBSimple s      -> snd $ RR.getRemoteJoins s
                QDBPrimaryKey s  -> snd $ RR.getRemoteJoins s
                QDBAggregation s -> snd $ RR.getRemoteJoinsAggregateSelect s
                QDBConnection s  -> snd $ RR.getRemoteJoinsConnectionSelect s
          when (remoteJoins /= mempty)
            $ throw400 NotSupported "Remote relationships are not allowed in subscriptions"
        _ -> pure ()
      traverseAction (DS.traverseAnnSimpleSelect resolveMultiplexedValue . resolveAsyncActionQuery userInfo) resolvedRootField

  let multiplexedQuery = mkMultiplexedQuery preparedAST
      roleName = _uiRole userInfo
      parameterizedPlan = ParameterizedLiveQueryPlan roleName multiplexedQuery

  -- We need to ensure that the values provided for variables are correct according to Postgres.
  -- Without this check an invalid value for a variable for one instance of the subscription will
  -- take down the entire multiplexed query.
  validatedQueryVars <- validateVariables pgExecCtx $ fmap pcvValue _qpiReusableVariableValues
  validatedSyntheticVars <- validateVariables pgExecCtx $ map pcvValue $ toList _qpiSyntheticVariableValues

  let -- TODO validatedQueryVars validatedSyntheticVars
      cohortVariables = mkCohortVariables _qpiReferencedSessionVariables
                        (_uiSession userInfo) validatedQueryVars validatedSyntheticVars

      plan = LiveQueryPlan parameterizedPlan cohortVariables
      -- See Note [Temporarily disabling query plan caching]
      -- varTypes = finalReusability ^? GV._Reusable
      reusablePlan =
        ReusableLiveQueryPlan parameterizedPlan
                              _qpiReferencedSessionVariables
                              validatedSyntheticVars
                              mempty {- <$> _varTypes -}
  pure (plan, Just reusablePlan)

data LiveQueryPlanExplanation
  = LiveQueryPlanExplanation
  { _lqpeSql       :: !Text
  , _lqpePlan      :: ![Text]
  , _lqpeVariables :: !CohortVariables
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''LiveQueryPlanExplanation)

explainLiveQueryPlan
  :: (MonadTx m, MonadIO m)
  => LiveQueryPlan -> m LiveQueryPlanExplanation
explainLiveQueryPlan plan = do
  let parameterizedPlan = _lqpParameterizedPlan plan
      queryText = Q.getQueryText . unMultiplexedQuery $ _plqpQuery parameterizedPlan
      -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
      -- query, maybe resulting in privilege escalation:
      explainQuery = Q.fromText $ "EXPLAIN (FORMAT TEXT) " <> queryText
  cohortId <- newCohortId
  explanationLines <- map runIdentity <$> executeQuery explainQuery
                      [(cohortId, _lqpVariables plan)]
  pure $ LiveQueryPlanExplanation queryText explanationLines $ _lqpVariables plan
