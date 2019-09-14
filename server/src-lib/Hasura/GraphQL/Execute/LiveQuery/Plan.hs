-- | Construction of multiplexed live query plans; see "Hasura.GraphQL.Execute.LiveQuery" for
-- details.
module Hasura.GraphQL.Execute.LiveQuery.Plan
  ( MultiplexedQuery
  , mkMultiplexedQuery
  , unMultiplexedQuery
  , toMultiplexedQueryVar

  , LiveQueryPlan(..)
  , ParameterizedLiveQueryPlan(..)
  , ReusableLiveQueryPlan
  , ValidatedQueryVariables
  , buildLiveQueryPlan
  , reuseLiveQueryPlan
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.Extended                    as J
import qualified Data.Aeson.TH                          as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Control.Lens
import           Data.Has

import qualified Hasura.GraphQL.Resolve                 as GR
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.SQL.DML                         as S

import           Hasura.Db
import           Hasura.RQL.Types
import           Hasura.SQL.Error
import           Hasura.SQL.Types
import           Hasura.SQL.Value

-- -------------------------------------------------------------------------------------------------
-- Multiplexed queries

newtype MultiplexedQuery = MultiplexedQuery { unMultiplexedQuery :: Q.Query }
  deriving (Show, Eq, Hashable, J.ToJSON)

mkMultiplexedQuery :: Q.Query -> MultiplexedQuery
mkMultiplexedQuery baseQuery =
  MultiplexedQuery . Q.fromText $ foldMap Q.getQueryText [queryPrefix, baseQuery, querySuffix]
  where
    queryPrefix =
      [Q.sql|
        select
          _subs.result_id, _fld_resp.root as result
          from
            unnest(
              $1::uuid[], $2::json[]
            ) _subs (result_id, result_vars)
          left outer join lateral
            (
        |]

    querySuffix =
      [Q.sql|
            ) _fld_resp ON ('true')
        |]

-- | converts the partial unresolved value containing
-- variables, session variables to an SQL expression
-- referring correctly to the values from '_subs' temporary table
-- The variables are at _subs.result_vars.variables and
-- session variables at _subs.result_vars.user
toMultiplexedQueryVar :: (MonadState GV.ReusableVariableValues m) => GR.UnresolvedVal -> m S.SQLExp
toMultiplexedQueryVar = \case
  GR.UVPG annPGVal ->
    let GR.AnnPGVal varM _ colVal = annPGVal
    in case varM of
      Just var -> do
        modify $ Map.insert var colVal
        pure $ fromResVars (PGTypeScalar $ pstType colVal)
          ["variables", G.unName $ G.unVariable var]
      Nothing -> return $ toTxtValue colVal
  GR.UVSessVar ty sessVar -> pure $ fromResVars ty ["user", T.toLower sessVar]
  GR.UVSQL sqlExp -> pure sqlExp
  where
    fromResVars ty jPath =
      flip S.SETyAnn (S.mkTypeAnn ty) $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIden $ S.QIden (S.QualIden $ Iden "_subs") (Iden "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]

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
newtype ValidatedQueryVariables = ValidatedQueryVariables (Map.HashMap G.Variable TxtEncodedPGVal)
  deriving (Show, Eq, Hashable, J.ToJSON)

-- | Checks if the provided arguments are valid values for their corresponding types.
-- Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateQueryVariables
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> GV.ReusableVariableValues
  -> m ValidatedQueryVariables
validateQueryVariables pgExecCtx annVarVals = do
  let valSel = mkValidationSel $ Map.elems annVarVals
  Q.Discard () <- runTx' $ liftTx $
    Q.rawQE dataExnErrHandler (Q.fromBuilder $ toSQL valSel) [] False
  pure . ValidatedQueryVariables $ fmap (txtEncodedPGVal . pstValue) annVarVals
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
  , _lqpSessionVariables  :: !UserVars
  , _lqpQueryVariables    :: !ValidatedQueryVariables
  }

data ParameterizedLiveQueryPlan
  = ParameterizedLiveQueryPlan
  { _plqpRole  :: !RoleName
  , _plqpAlias :: !G.Alias
  , _plqpQuery :: !MultiplexedQuery
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ParameterizedLiveQueryPlan)

data ReusableLiveQueryPlan
  = ReusableLiveQueryPlan
  { _rlqpParameterizedPlan  :: !ParameterizedLiveQueryPlan
  , _rlqpQueryVariableTypes :: !GV.ReusableVariableTypes
  } deriving (Show)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ReusableLiveQueryPlan)

-- | Constructs a new execution plan for a live query and returns a reusable version of the plan if
-- possible.
buildLiveQueryPlan
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , MonadIO m
     )
  => PGExecCtx
  -> G.Alias
  -> GR.QueryRootFldUnresolved
  -> Maybe GV.ReusableVariableTypes
  -> m (LiveQueryPlan, Maybe ReusableLiveQueryPlan)
buildLiveQueryPlan pgExecCtx fieldAlias astUnresolved varTypes = do
  userInfo <- asks getter

  (astResolved, annVarVals) <- flip runStateT mempty $
    GR.traverseQueryRootFldAST toMultiplexedQueryVar astUnresolved
  let pgQuery = mkMultiplexedQuery $ GR.toPGQuery astResolved
      parameterizedPlan = ParameterizedLiveQueryPlan (userRole userInfo) fieldAlias pgQuery

  -- We need to ensure that the values provided for variables
  -- are correct according to Postgres. Without this check
  -- an invalid value for a variable for one instance of the
  -- subscription will take down the entire multiplexed query
  validatedVars <- validateQueryVariables pgExecCtx annVarVals
  let plan = LiveQueryPlan parameterizedPlan (userVars userInfo) validatedVars
      reusablePlan = ReusableLiveQueryPlan parameterizedPlan <$> varTypes
  pure (plan, reusablePlan)

reuseLiveQueryPlan
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> UserVars
  -> Maybe GH.VariableValues
  -> ReusableLiveQueryPlan
  -> m LiveQueryPlan
reuseLiveQueryPlan pgExecCtx sessionVars queryVars reusablePlan = do
  let ReusableLiveQueryPlan parameterizedPlan varTypes = reusablePlan
  annVarVals <- GV.validateVariablesForReuse varTypes queryVars
  validatedVars <- validateQueryVariables pgExecCtx annVarVals
  pure $ LiveQueryPlan parameterizedPlan sessionVars validatedVars
