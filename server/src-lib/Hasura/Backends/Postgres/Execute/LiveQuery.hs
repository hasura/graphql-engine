module Hasura.Backends.Postgres.Execute.LiveQuery
  ( MultiplexedQuery (..)
  , QueryParametersInfo (..)
  , mkMultiplexedQuery
  , resolveMultiplexedValue
  , validateVariables
  , executeMultiplexedQuery
  , executeQuery
  ) where

import           Hasura.Prelude

import qualified Data.ByteString                           as B
import qualified Data.HashMap.Strict                       as Map
import qualified Data.HashMap.Strict.InsOrd                as OMap
import qualified Data.HashSet                              as Set
import qualified Database.PG.Query                         as Q
import qualified Language.GraphQL.Draft.Syntax             as G

import           Control.Lens
import           Data.Semigroup.Generic
import           Data.Text.Extended

import qualified Hasura.Backends.Postgres.SQL.DML          as S
import qualified Hasura.Backends.Postgres.Translate.Select as DS
import qualified Hasura.GraphQL.Parser.Schema              as PS

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.SQL.Error
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.Column (toTxtValue)
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.LiveQuery.Plan
import           Hasura.GraphQL.Parser
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Session


----------------------------------------------------------------------------------------------------
-- Variables

-- | Internal: Used to collect information about various parameters
-- of a subscription field's AST as we resolve them to SQL expressions.
data QueryParametersInfo
  = QueryParametersInfo
  { _qpiReusableVariableValues     :: !(HashMap G.Name (ColumnValue 'Postgres))
  , _qpiSyntheticVariableValues    :: !(Seq (ColumnValue 'Postgres))
  , _qpiReferencedSessionVariables :: !(Set.HashSet SessionVariable)
  -- ^ The session variables that are referenced in the query root fld's AST.
  -- This information is used to determine a cohort's required session
  -- variables
  } deriving (Generic)
    deriving (Semigroup, Monoid) via (GenericSemigroupMonoid QueryParametersInfo)

makeLenses ''QueryParametersInfo

-- | Checks if the provided arguments are valid values for their corresponding types.
-- | Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateVariables
  :: (Traversable f, MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> f (ColumnValue 'Postgres)
  -> m (ValidatedVariables f)
validateVariables pgExecCtx variableValues = do
  let valSel = mkValidationSel $ toList variableValues
  Q.Discard () <- runQueryTx_ $ liftTx $
    Q.rawQE dataExnErrHandler (Q.fromBuilder $ toSQL valSel) [] False
  pure . ValidatedVariables $ fmap (txtEncodedVal . cvValue) variableValues
  where
    mkExtr = flip S.Extractor Nothing . toTxtValue
    mkValidationSel vars =
      S.mkSelect { S.selExtr = map mkExtr vars }
    runQueryTx_ tx = do
      res <- liftIO $ runExceptT (runQueryTx pgExecCtx tx)
      liftEither res

    -- Explicitly look for the class of errors raised when the format of a value
    -- provided for a type is incorrect.
    dataExnErrHandler = mkTxErrorHandler (has _PGDataException)


----------------------------------------------------------------------------------------------------
-- Multiplexed queries

newtype MultiplexedQuery = MultiplexedQuery { unMultiplexedQuery :: Q.Query }
  deriving (Eq, Hashable)

instance ToTxt MultiplexedQuery where
  toTxt = Q.getQueryText . unMultiplexedQuery


toSQLFromItem :: S.Alias -> QueryDB 'Postgres S.SQLExp -> S.FromItem
toSQLFromItem = flip \case
  QDBSingleRow    s -> S.mkSelFromItem $ DS.mkSQLSelect JASSingleObject s
  QDBMultipleRows s -> S.mkSelFromItem $ DS.mkSQLSelect JASMultipleRows s
  QDBAggregation  s -> S.mkSelFromItem $ DS.mkAggregateSelect s
  QDBConnection   s -> S.mkSelectWithFromItem $ DS.mkConnectionSelect s

mkMultiplexedQuery :: OMap.InsOrdHashMap G.Name (QueryDB 'Postgres S.SQLExp) -> MultiplexedQuery
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
          OMap.toList rootFields <&> \(fieldAlias, resolvedAST) ->
            toSQLFromItem (S.Alias $ aliasToIdentifier fieldAlias) resolvedAST
      }

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    rootFieldsJsonAggregate = S.SEFnApp "json_build_object" rootFieldsJsonPairs Nothing
    rootFieldsJsonPairs = flip concatMap (OMap.keys rootFields) $ \fieldAlias ->
      [ S.SELit (G.unName fieldAlias)
      , mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "root") ]

    mkQualifiedIdentifier prefix = S.SEQIdentifier . S.QIdentifier (S.QualifiedIdentifier prefix Nothing) -- TODO fix this Nothing of course
    aliasToIdentifier = Identifier . G.unName

-- | Resolves an 'GR.UnresolvedVal' by converting 'GR.UVPG' values to SQL
-- expressions that refer to the @result_vars@ input object, collecting information
-- about various parameters of the query along the way.
resolveMultiplexedValue
  :: (MonadState QueryParametersInfo m)
  => UnpreparedValue 'Postgres -> m S.SQLExp
resolveMultiplexedValue = \case
  UVParameter varM colVal -> do
    varJsonPath <- case fmap PS.getName varM of
      Just varName -> do
        modifying qpiReusableVariableValues $ Map.insert varName colVal
        pure ["query", G.unName  varName]
      Nothing -> do
        syntheticVarIndex <- use (qpiSyntheticVariableValues . to length)
        modifying qpiSyntheticVariableValues (|> colVal)
        pure ["synthetic", tshow syntheticVarIndex]
    pure $ fromResVars (CollectableTypeScalar $ unsafePGColumnToBackend $ cvType colVal) varJsonPath
  UVSessionVar ty sessVar -> do
    modifying qpiReferencedSessionVariables (Set.insert sessVar)
    pure $ fromResVars ty ["session", sessionVariableToText sessVar]
  UVLiteral sqlExp -> pure sqlExp
  UVSession -> pure $ fromResVars (CollectableTypeScalar PGJSON) ["session"]
  where
    fromResVars pgType jPath = addTypeAnnotation pgType $ S.SEOpApp (S.SQLOp "#>>")
      [ S.SEQIdentifier $ S.QIdentifier (S.QualifiedIdentifier (Identifier "_subs") Nothing) (Identifier "result_vars")
      , S.SEArray $ map S.SELit jPath
      ]
    addTypeAnnotation pgType = flip S.SETyAnn (S.mkTypeAnn pgType) . case pgType of
      CollectableTypeScalar scalarType -> withConstructorFn scalarType
      CollectableTypeArray _           -> id


----------------------------------------------------------------------------------------------------
-- Execution

executeMultiplexedQuery
  :: (MonadTx m)
  => MultiplexedQuery -> [(CohortId, CohortVariables)] -> m [(CohortId, B.ByteString)]
executeMultiplexedQuery (MultiplexedQuery query) = executeQuery query

-- | Internal; used by both 'executeMultiplexedQuery' and 'pgDBLiveQueryExplain'.
executeQuery
  :: (MonadTx m, Q.FromRow a)
  => Q.Query -> [(CohortId, CohortVariables)] -> m [a]
executeQuery query cohorts =
  let (cohortIds, cohortVars) = unzip cohorts
      preparedArgs = (CohortIdArray cohortIds, CohortVariablesArray cohortVars)
  in liftTx $ Q.listQE defaultTxErrorHandler query preparedArgs True
