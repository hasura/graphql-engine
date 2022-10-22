{-# LANGUAGE TemplateHaskell #-}

-- | Postgres Execute subscription
--
-- Multiplex is an optimization which allows us to group similar queries into a
-- single query, and routing the response rows afterwards. See
-- https://hasura.io/docs/latest/graphql/core/databases/postgres/subscriptions/execution-and-performance.html
-- for more details
--
-- See 'Hasura.Backends.Postgres.Instances.Execute'.
module Hasura.Backends.Postgres.Execute.Subscription
  ( MultiplexedQuery (..),
    QueryParametersInfo (..),
    mkMultiplexedQuery,
    mkStreamingMultiplexedQuery,
    resolveMultiplexedValue,
    validateVariables,
    executeMultiplexedQuery,
    executeStreamingMultiplexedQuery,
    executeQuery,
    SubscriptionType (..),
  )
where

import Control.Lens
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.Semigroup.Generic
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Error
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Column (toTxtValue)
import Hasura.Backends.Postgres.Translate.Select qualified as DS
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Parser.Schema qualified as PS
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Subscription
import Hasura.SQL.Backend
import Hasura.SQL.Types
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------------------------------------------
-- Variables

-- | Internal: Used to collect information about various parameters
-- of a subscription field's AST as we resolve them to SQL expressions.
data QueryParametersInfo (b :: BackendType) = QueryParametersInfo
  { _qpiReusableVariableValues :: !(HashMap G.Name (ColumnValue b)),
    _qpiSyntheticVariableValues :: !(Seq (ColumnValue b)),
    -- | The session variables that are referenced in the query root fld's AST.
    -- This information is used to determine a cohort's required session
    -- variables
    _qpiReferencedSessionVariables :: !(Set.HashSet SessionVariable)
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid (QueryParametersInfo b))

makeLenses ''QueryParametersInfo

-- | Checks if the provided arguments are valid values for their corresponding types.
-- | Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateVariables ::
  forall pgKind f m.
  (Traversable f, MonadError QErr m, MonadIO m) =>
  PGExecCtx ->
  f (ColumnValue ('Postgres pgKind)) ->
  m (ValidatedVariables f)
validateVariables pgExecCtx variableValues = do
  let valSel = mkValidationSel $ toList variableValues
  Q.Discard () <-
    runQueryTx_ $
      liftTx $
        Q.rawQE dataExnErrHandler (Q.fromBuilder $ toSQL valSel) [] False
  pure . ValidatedVariables $ fmap (txtEncodedVal . cvValue) variableValues
  where
    mkExtr = flip S.Extractor Nothing . toTxtValue
    mkValidationSel vars =
      S.mkSelect {S.selExtr = map mkExtr vars}
    runQueryTx_ tx = do
      res <- liftIO $ runExceptT (runQueryTx pgExecCtx tx)
      liftEither res

    -- Explicitly look for the class of errors raised when the format of a value
    -- provided for a type is incorrect.
    dataExnErrHandler = mkTxErrorHandler (has _PGDataException)

----------------------------------------------------------------------------------------------------
-- Multiplexed queries

newtype MultiplexedQuery = MultiplexedQuery {unMultiplexedQuery :: Q.Query}
  deriving (Eq, Hashable)

instance ToTxt MultiplexedQuery where
  toTxt = Q.getQueryText . unMultiplexedQuery

toSQLFromItem ::
  ( Backend ('Postgres pgKind),
    DS.PostgresAnnotatedFieldJSON pgKind
  ) =>
  S.Alias ->
  QueryDB ('Postgres pgKind) Void S.SQLExp ->
  S.FromItem
toSQLFromItem = flip \case
  QDBSingleRow s -> S.mkSelFromItem $ DS.mkSQLSelect JASSingleObject s
  QDBMultipleRows s -> S.mkSelFromItem $ DS.mkSQLSelect JASMultipleRows s
  QDBAggregation s -> S.mkSelFromItem $ DS.mkAggregateSelect s
  QDBConnection s -> S.mkSelectWithFromItem $ DS.mkConnectionSelect s
  QDBStreamMultipleRows s -> S.mkSelFromItem $ DS.mkStreamSQLSelect s

mkMultiplexedQuery ::
  ( Backend ('Postgres pgKind),
    DS.PostgresAnnotatedFieldJSON pgKind
  ) =>
  OMap.InsOrdHashMap G.Name (QueryDB ('Postgres pgKind) Void S.SQLExp) ->
  MultiplexedQuery
mkMultiplexedQuery rootFields =
  MultiplexedQuery . Q.fromBuilder . toSQL $
    S.mkSelect
      { S.selExtr =
          -- SELECT _subs.result_id, _fld_resp.root AS result
          [ S.Extractor (mkQualifiedIdentifier (Identifier "_subs") (Identifier "result_id")) Nothing,
            S.Extractor (mkQualifiedIdentifier (Identifier "_fld_resp") (Identifier "root")) (Just . S.Alias $ Identifier "result")
          ],
        S.selFrom =
          Just $
            S.FromExp
              [ S.FIJoin $
                  S.JoinExpr subsInputFromItem S.LeftOuter responseLateralFromItem (S.JoinOn $ S.BELit True)
              ]
      }
  where
    -- FROM unnest($1::uuid[], $2::json[]) _subs (result_id, result_vars)
    subsInputFromItem =
      S.FIUnnest
        [S.SEPrep 1 `S.SETyAnn` S.TypeAnn "uuid[]", S.SEPrep 2 `S.SETyAnn` S.TypeAnn "json[]"]
        (S.Alias $ Identifier "_subs")
        [S.SEIdentifier $ Identifier "result_id", S.SEIdentifier $ Identifier "result_vars"]

    -- LEFT OUTER JOIN LATERAL ( ... ) _fld_resp
    responseLateralFromItem = S.mkLateralFromItem selectRootFields (S.Alias $ Identifier "_fld_resp")
    selectRootFields =
      S.mkSelect
        { S.selExtr = [S.Extractor rootFieldsJsonAggregate (Just . S.Alias $ Identifier "root")],
          S.selFrom =
            Just . S.FromExp $
              OMap.toList rootFields <&> \(fieldAlias, resolvedAST) ->
                toSQLFromItem (S.Alias $ aliasToIdentifier fieldAlias) resolvedAST
        }

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    rootFieldsJsonAggregate = S.SEFnApp "json_build_object" rootFieldsJsonPairs Nothing
    rootFieldsJsonPairs = flip concatMap (OMap.keys rootFields) $ \fieldAlias ->
      [ S.SELit (G.unName fieldAlias),
        mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "root")
      ]

    mkQualifiedIdentifier prefix = S.SEQIdentifier . S.QIdentifier (S.QualifiedIdentifier prefix Nothing)
    aliasToIdentifier = Identifier . G.unName

mkStreamingMultiplexedQuery ::
  ( Backend ('Postgres pgKind),
    DS.PostgresAnnotatedFieldJSON pgKind
  ) =>
  (G.Name, (QueryDB ('Postgres pgKind) Void S.SQLExp)) ->
  MultiplexedQuery
mkStreamingMultiplexedQuery (fieldAlias, resolvedAST) =
  MultiplexedQuery . Q.fromBuilder . toSQL $
    S.mkSelect
      { S.selExtr =
          -- SELECT _subs.result_id, _fld_resp.root, _fld_resp.cursor AS result
          [ S.Extractor (mkQualifiedIdentifier (Identifier "_subs") (Identifier "result_id")) Nothing,
            S.Extractor (mkQualifiedIdentifier (Identifier "_fld_resp") (Identifier "root")) (Just . S.Alias $ Identifier "result"),
            S.Extractor (mkQualifiedIdentifier (Identifier "_fld_resp") (Identifier "cursor")) (Just . S.Alias $ Identifier "cursor")
          ],
        S.selFrom =
          Just $
            S.FromExp
              [ S.FIJoin $
                  S.JoinExpr subsInputFromItem S.LeftOuter responseLateralFromItem (S.JoinOn $ S.BELit True)
              ]
      }
  where
    -- FROM unnest($1::uuid[], $2::json[]) _subs (result_id, result_vars)
    subsInputFromItem =
      S.FIUnnest
        [S.SEPrep 1 `S.SETyAnn` S.TypeAnn "uuid[]", S.SEPrep 2 `S.SETyAnn` S.TypeAnn "json[]"]
        (S.Alias $ Identifier "_subs")
        [S.SEIdentifier $ Identifier "result_id", S.SEIdentifier $ Identifier "result_vars"]

    -- LEFT OUTER JOIN LATERAL ( ... ) _fld_resp
    responseLateralFromItem = S.mkLateralFromItem selectRootFields (S.Alias $ Identifier "_fld_resp")
    selectRootFields =
      S.mkSelect
        { S.selExtr = [(S.Extractor rootFieldJsonAggregate (Just . S.Alias $ Identifier "root")), cursorExtractor],
          S.selFrom =
            Just . S.FromExp $
              pure $ toSQLFromItem (S.Alias $ aliasToIdentifier fieldAlias) resolvedAST
        }

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    rootFieldJsonAggregate = S.SEFnApp "json_build_object" rootFieldJsonPair Nothing
    rootFieldJsonPair =
      [ S.SELit (G.unName fieldAlias),
        mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "root")
      ]

    -- to_json("root"."cursor") AS "cursor"
    cursorSQLExp = S.SEFnApp "to_json" [mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "cursor")] Nothing
    cursorExtractor = S.Extractor cursorSQLExp (Just . S.Alias $ Identifier "cursor")
    mkQualifiedIdentifier prefix = S.SEQIdentifier . S.QIdentifier (S.QualifiedIdentifier prefix Nothing)
    aliasToIdentifier = Identifier . G.unName

-- | Resolves an 'GR.UnresolvedVal' by converting 'GR.UVPG' values to SQL
-- expressions that refer to the @result_vars@ input object, collecting information
-- about various parameters of the query along the way.
resolveMultiplexedValue ::
  ( MonadState (QueryParametersInfo ('Postgres pgKind)) m,
    MonadError QErr m
  ) =>
  SessionVariables ->
  UnpreparedValue ('Postgres pgKind) ->
  m S.SQLExp
resolveMultiplexedValue allSessionVars = \case
  UVParameter varM colVal -> do
    varJsonPath <- case fmap PS.getName varM of
      Just varName -> do
        modifying qpiReusableVariableValues $ Map.insert varName colVal
        pure ["query", G.unName varName]
      Nothing -> do
        syntheticVarIndex <- use (qpiSyntheticVariableValues . to length)
        modifying qpiSyntheticVariableValues (|> colVal)
        pure ["synthetic", tshow syntheticVarIndex]
    pure $ fromResVars (CollectableTypeScalar $ unsafePGColumnToBackend $ cvType colVal) varJsonPath
  UVSessionVar ty sessVar -> do
    _ <-
      getSessionVariableValue sessVar allSessionVars
        `onNothing` throw400
          NotFound
          ("missing session variable: " <>> sessionVariableToText sessVar)
    modifying qpiReferencedSessionVariables (Set.insert sessVar)
    pure $ fromResVars ty ["session", sessionVariableToText sessVar]
  UVLiteral sqlExp -> pure sqlExp
  UVSession -> do
    -- if the entire session is referenced, then add all session vars in referenced vars
    modifying qpiReferencedSessionVariables (const $ getSessionVariablesSet allSessionVars)
    pure $ fromResVars (CollectableTypeScalar PGJSON) ["session"]
  where
    fromResVars pgType jPath =
      addTypeAnnotation pgType $
        S.SEOpApp
          (S.SQLOp "#>>")
          [ S.SEQIdentifier $ S.QIdentifier (S.QualifiedIdentifier (Identifier "_subs") Nothing) (Identifier "result_vars"),
            S.SEArray $ map S.SELit jPath
          ]
    addTypeAnnotation pgType =
      flip S.SETyAnn (S.mkTypeAnn pgType) . case pgType of
        CollectableTypeScalar scalarType -> withConstructorFn scalarType
        CollectableTypeArray _ -> id

----------------------------------------------------------------------------------------------------
-- Execution

executeMultiplexedQuery ::
  (MonadTx m) =>
  MultiplexedQuery ->
  [(CohortId, CohortVariables)] ->
  m [(CohortId, B.ByteString)]
executeMultiplexedQuery (MultiplexedQuery query) cohorts =
  executeQuery query cohorts

executeStreamingMultiplexedQuery ::
  (MonadTx m) =>
  MultiplexedQuery ->
  [(CohortId, CohortVariables)] ->
  m [(CohortId, B.ByteString, Q.AltJ CursorVariableValues)]
executeStreamingMultiplexedQuery (MultiplexedQuery query) cohorts = do
  executeQuery query cohorts

-- | Internal; used by both 'executeMultiplexedQuery', 'executeStreamingMultiplexedQuery'
-- and 'pgDBSubscriptionExplain'.
executeQuery ::
  (MonadTx m, Q.FromRow a) =>
  Q.Query ->
  [(CohortId, CohortVariables)] ->
  m [a]
executeQuery query cohorts =
  let (cohortIds, cohortVars) = unzip cohorts
      preparedArgs = (CohortIdArray cohortIds, CohortVariablesArray cohortVars)
   in liftTx $ Q.listQE defaultTxErrorHandler query preparedArgs True
