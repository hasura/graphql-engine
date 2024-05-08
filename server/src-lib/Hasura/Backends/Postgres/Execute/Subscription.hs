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
    validateVariablesTx,
    executeMultiplexedQuery,
    executeStreamingMultiplexedQuery,
    executeQuery,
    SubscriptionType (..),
  )
where

import Control.Lens
import Control.Monad.Writer
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.Semigroup.Generic
import Data.Text.Extended
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Error
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.SQL.Value
import Hasura.Backends.Postgres.Translate.Column (toTxtValue)
import Hasura.Backends.Postgres.Translate.Select qualified as DS
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (customSQLToInnerCTEs, toQuery)
import Hasura.Backends.Postgres.Translate.Types (CustomSQLCTEs (..))
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Parser.Names
import Hasura.Prelude hiding (runWriterT)
import Hasura.RQL.IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Subscription
import Hasura.SQL.Types
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------------------------------------------
-- Variables

subsAlias :: S.TableAlias
subsAlias = S.mkTableAlias "_subs"

subsIdentifier :: TableIdentifier
subsIdentifier = S.tableAliasToIdentifier subsAlias

resultIdAlias, resultVarsAlias :: S.ColumnAlias
resultIdAlias = S.mkColumnAlias "result_id"
resultVarsAlias = S.mkColumnAlias "result_vars"

fldRespAlias :: S.TableAlias
fldRespAlias = S.mkTableAlias "_fld_resp"

fldRespIdentifier :: TableIdentifier
fldRespIdentifier = S.tableAliasToIdentifier fldRespAlias

-- | Internal: Used to collect information about various parameters
-- of a subscription field's AST as we resolve them to SQL expressions.
data QueryParametersInfo (b :: BackendType) = QueryParametersInfo
  { _qpiReusableVariableValues :: HashMap G.Name (ColumnValue b),
    _qpiSyntheticVariableValues :: Seq (ColumnValue b),
    -- | The session variables that are referenced in the query root fld's AST.
    -- This information is used to determine a cohort's required session
    -- variables
    _qpiReferencedSessionVariables :: Set.HashSet SessionVariable
  }
  deriving (Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid (QueryParametersInfo b))

makeLenses ''QueryParametersInfo

-- | Checks if the provided arguments are valid values for their corresponding types.
-- | Generates SQL of the format "select 'v1'::t1, 'v2'::t2 ..."
validateVariablesTx ::
  forall pgKind f m.
  (Traversable f, MonadTx m, MonadIO m) =>
  f (ColumnValue ('Postgres pgKind)) ->
  m (ValidatedVariables f)
validateVariablesTx variableValues = do
  -- no need to test the types when there are no variables to test.
  unless (null variableValues) do
    let valSel = mkValidationSel $ toList variableValues
    PG.Discard () <- liftTx $ PG.rawQE dataExnErrHandler (PG.fromBuilder $ toSQL valSel) [] False
    pure ()
  pure . ValidatedVariables $ fmap (txtEncodedVal . cvValue) variableValues
  where
    mkExtr = flip S.Extractor Nothing . toTxtValue
    mkValidationSel vars =
      S.mkSelect {S.selExtr = map mkExtr vars}
    -- Explicitly look for the class of errors raised when the format of a value
    -- provided for a type is incorrect.
    dataExnErrHandler = mkTxErrorHandler (has _PGDataException)

----------------------------------------------------------------------------------------------------
-- Multiplexed queries

newtype MultiplexedQuery = MultiplexedQuery {unMultiplexedQuery :: PG.Query}
  deriving (Eq, Hashable)

instance ToTxt MultiplexedQuery where
  toTxt = PG.getQueryText . unMultiplexedQuery

toSQLFromItem ::
  ( Backend ('Postgres pgKind),
    DS.PostgresTranslateSelect pgKind,
    MonadWriter CustomSQLCTEs m,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  S.TableAlias ->
  QueryDB ('Postgres pgKind) Void S.SQLExp ->
  m S.FromItem
toSQLFromItem userInfo tableAlias query = do
  -- Note: Remote relationship predicate in permission is not supported in subscriptions
  -- as current implementation of multiplexing doesn't refetch the SQL query and hence the
  -- query might be executed with stale values.
  throwErrorForRemoteRelationshipInPermissionPredicate query
  case query of
    QDBSingleRow s -> S.mkSelFromItem <$> DS.mkSQLSelect userInfo JASSingleObject s <*> pure tableAlias
    QDBMultipleRows s -> S.mkSelFromItem <$> DS.mkSQLSelect userInfo JASMultipleRows s <*> pure tableAlias
    QDBAggregation s -> S.mkSelFromItem <$> DS.mkAggregateSelect userInfo s <*> pure tableAlias
    QDBConnection s -> S.mkSelectWithFromItem <$> DS.mkConnectionSelect userInfo s <*> pure tableAlias
    QDBStreamMultipleRows s -> S.mkSelFromItem <$> DS.mkStreamSQLSelect userInfo s <*> pure tableAlias

throwErrorForRemoteRelationshipInPermissionPredicate ::
  ( MonadError QErr m
  ) =>
  QueryDB ('Postgres pgKind) Void S.SQLExp ->
  m ()
throwErrorForRemoteRelationshipInPermissionPredicate q = do
  case q of
    QDBSingleRow s -> do
      throwErrorForRemoteRelationshipInSelect (_tpFilter (_asnPerm s))
      for_ (_asnFields s) \(_, fld) ->
        throwErrorForRemoteRelationshipInPermissionPredicateInField fld
    QDBMultipleRows s -> do
      throwErrorForRemoteRelationshipInSelect (_tpFilter (_asnPerm s))
      for_ (_asnFields s) \(_, fld) ->
        throwErrorForRemoteRelationshipInPermissionPredicateInField fld
    QDBAggregation s -> do
      throwErrorForRemoteRelationshipInSelect (_tpFilter (_asnPerm s))
      throwErrorForRemoteRelationshipInPermissionPredicateInAggregateFields (_asnFields s)
    QDBConnection s -> do
      throwErrorForRemoteRelationshipInSelect (_tpFilter (_asnPerm (_csSelect s)))
      throwErrorForRemoteRelationshipInPermissionPredicateInConnectionFields (_asnFields (_csSelect s))
    QDBStreamMultipleRows s -> do
      throwErrorForRemoteRelationshipInSelect (_tpFilter (_assnPerm s))
      for_ (_assnFields s) \(_, fld) ->
        throwErrorForRemoteRelationshipInPermissionPredicateInField fld
  where
    throwErrorForRemoteRelationshipInSelect :: (MonadError QErr f) => GBoolExp backend1 (AnnBoolExpFld backend2 leaf) -> f ()
    throwErrorForRemoteRelationshipInSelect s = do
      when (haveRemoteRelationshipPredicate s)
        $ throw400 NotSupported "subscriptions on this field is not supported"

    haveRemoteRelationshipPredicate :: GBoolExp backend1 (AnnBoolExpFld backend2 leaf) -> Bool
    haveRemoteRelationshipPredicate (BoolAnd lst) = any haveRemoteRelationshipPredicate lst
    haveRemoteRelationshipPredicate (BoolOr lst) = any haveRemoteRelationshipPredicate lst
    haveRemoteRelationshipPredicate (BoolNot b) = haveRemoteRelationshipPredicate b
    haveRemoteRelationshipPredicate (BoolExists e) = haveRemoteRelationshipPredicate (_geWhere e)
    haveRemoteRelationshipPredicate (BoolField (AVRemoteRelationship _x)) = True
    haveRemoteRelationshipPredicate _ = False

    throwErrorForRemoteRelationshipInPermissionPredicateInField ::
      ( MonadError QErr m
      ) =>
      AnnFieldG ('Postgres pgKind) Void S.SQLExp ->
      m ()
    throwErrorForRemoteRelationshipInPermissionPredicateInField (AFObjectRelation objRel) = do
      throwErrorForRemoteRelationshipInSelect $ _aosTargetFilter (_aarAnnSelect objRel)
      for_ (_aosFields (_aarAnnSelect objRel)) \(_, fld) ->
        throwErrorForRemoteRelationshipInPermissionPredicateInField fld
    throwErrorForRemoteRelationshipInPermissionPredicateInField (AFArrayRelation arraySelectG) = do
      case arraySelectG of
        ASSimple s -> do
          throwErrorForRemoteRelationshipInSelect $ _tpFilter (_asnPerm (_aarAnnSelect s))
          for_ (_asnFields (_aarAnnSelect s)) \(_, fld) ->
            throwErrorForRemoteRelationshipInPermissionPredicateInField fld
        ASAggregate s -> do
          throwErrorForRemoteRelationshipInSelect $ _tpFilter (_asnPerm (_aarAnnSelect s))
          throwErrorForRemoteRelationshipInPermissionPredicateInAggregateFields (_asnFields (_aarAnnSelect s))
        ASConnection s -> do
          throwErrorForRemoteRelationshipInSelect $ _tpFilter (_asnPerm (_csSelect (_aarAnnSelect s)))
          throwErrorForRemoteRelationshipInPermissionPredicateInConnectionFields (_asnFields (_csSelect (_aarAnnSelect s)))
    throwErrorForRemoteRelationshipInPermissionPredicateInField (AFColumn _) = pure ()
    throwErrorForRemoteRelationshipInPermissionPredicateInField (AFComputedField {}) = pure ()
    throwErrorForRemoteRelationshipInPermissionPredicateInField (AFNodeId {}) = pure ()
    throwErrorForRemoteRelationshipInPermissionPredicateInField (AFExpression _) = pure ()

    throwErrorForRemoteRelationshipInPermissionPredicateInConnectionFields ::
      ( MonadError QErr m
      ) =>
      Fields (ConnectionField ('Postgres pgKind) Void S.SQLExp) ->
      m ()
    throwErrorForRemoteRelationshipInPermissionPredicateInConnectionFields connFields =
      for_ connFields \(_, fld) ->
        case fld of
          ConnectionEdges edgeFields ->
            for_ edgeFields \(_, fld') ->
              case fld' of
                EdgeNode fields ->
                  for_ fields \(_, fld'') ->
                    throwErrorForRemoteRelationshipInPermissionPredicateInField fld''
                EdgeTypename _ -> pure ()
                EdgeCursor -> pure ()
          ConnectionTypename _ -> pure ()
          ConnectionPageInfo _ -> pure ()

    throwErrorForRemoteRelationshipInPermissionPredicateInAggregateFields ::
      ( MonadError QErr m
      ) =>
      Fields (TableAggregateFieldG ('Postgres pgKind) Void S.SQLExp) ->
      m ()
    throwErrorForRemoteRelationshipInPermissionPredicateInAggregateFields aggFields =
      for_ aggFields \(_, fld) ->
        case fld of
          TAFNodes _ fields -> do
            for_ fields \(_, fld') ->
              throwErrorForRemoteRelationshipInPermissionPredicateInField fld'
          TAFGroupBy _ groupByField ->
            for_ (_gbgFields groupByField) \(_, fld') ->
              case fld' of
                GBFNodes fields ->
                  for_ fields \(_, fld'') ->
                    throwErrorForRemoteRelationshipInPermissionPredicateInField fld''
                GBFGroupKey _ -> pure ()
                GBFAggregate _ -> pure ()
                GBFExp _ -> pure ()
          TAFAgg _ -> pure ()
          TAFExp _ -> pure ()

mkMultiplexedQuery ::
  ( Backend ('Postgres pgKind),
    DS.PostgresTranslateSelect pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  InsOrdHashMap.InsOrdHashMap G.Name (QueryDB ('Postgres pgKind) Void S.SQLExp) ->
  m MultiplexedQuery
mkMultiplexedQuery userInfo rootFields = do
  (sqlFrom, customSQLCTEs) <-
    runWriterT
      $ traverse
        ( \(fieldAlias, resolvedAST) ->
            toSQLFromItem userInfo (S.mkTableAlias $ G.unName fieldAlias) resolvedAST
        )
        (InsOrdHashMap.toList rootFields)
  -- multiplexed queries may only contain read only raw queries
  let selectWith = S.SelectWith [] select
      select =
        S.mkSelect
          { S.selExtr =
              -- SELECT _subs.result_id, _fld_resp.root AS result
              [ S.Extractor (mkQualifiedIdentifier subsIdentifier (Identifier "result_id")) Nothing,
                S.Extractor (mkQualifiedIdentifier fldRespIdentifier (Identifier "root")) (Just $ S.toColumnAlias $ Identifier "result")
              ],
            S.selFrom =
              Just
                $ S.FromExp
                  [ S.FIJoin
                      $ S.JoinExpr subsInputFromItem S.LeftOuter responseLateralFromItem (S.JoinOn $ S.BELit True)
                  ]
          }
      -- LEFT OUTER JOIN LATERAL ( ... ) _fld_resp
      responseLateralFromItem = S.mkLateralFromItem selectRootFields fldRespAlias
      selectRootFields =
        S.mkSelect
          { S.selExtr = [S.Extractor rootFieldsJsonAggregate (Just $ S.toColumnAlias $ Identifier "root")],
            S.selCTEs = customSQLToInnerCTEs customSQLCTEs,
            S.selFrom =
              Just $ S.FromExp sqlFrom
          }
  pure $ MultiplexedQuery . toQuery $ selectWith
  where
    -- FROM unnest($1::uuid[], $2::json[]) _subs (result_id, result_vars)
    subsInputFromItem =
      S.FIUnnest
        [S.SEPrep 1 `S.SETyAnn` S.TypeAnn "uuid[]", S.SEPrep 2 `S.SETyAnn` S.TypeAnn "json[]"]
        subsAlias
        [S.toColumnAlias $ Identifier "result_id", S.toColumnAlias $ Identifier "result_vars"]

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    rootFieldsJsonAggregate = S.SEFnApp "json_build_object" rootFieldsJsonPairs Nothing
    rootFieldsJsonPairs = flip concatMap (InsOrdHashMap.keys rootFields) $ \fieldAlias ->
      [ S.SELit (G.unName fieldAlias),
        mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "root")
      ]

    mkQualifiedIdentifier prefix = S.SEQIdentifier . S.QIdentifier (S.QualifiedIdentifier prefix Nothing)
    aliasToIdentifier = TableIdentifier . G.unName

mkStreamingMultiplexedQuery ::
  ( Backend ('Postgres pgKind),
    DS.PostgresTranslateSelect pgKind,
    MonadIO m,
    MonadError QErr m
  ) =>
  UserInfo ->
  (G.Name, (QueryDB ('Postgres pgKind) Void S.SQLExp)) ->
  m MultiplexedQuery
mkStreamingMultiplexedQuery userInfo (fieldAlias, resolvedAST) = do
  (fromSQL, customSQLCTEs) <- runWriterT (toSQLFromItem userInfo (S.mkTableAlias $ G.unName fieldAlias) resolvedAST)
  let selectWith = S.SelectWith [] select
      select =
        S.mkSelect
          { S.selExtr =
              -- SELECT _subs.result_id, _fld_resp.root, _fld_resp.cursor AS result
              [ S.Extractor (mkQualifiedIdentifier subsIdentifier (Identifier "result_id")) Nothing,
                S.Extractor (mkQualifiedIdentifier fldRespIdentifier (Identifier "root")) (Just $ S.toColumnAlias $ Identifier "result"),
                S.Extractor (mkQualifiedIdentifier fldRespIdentifier (Identifier "cursor")) (Just $ S.toColumnAlias $ Identifier "cursor")
              ],
            S.selFrom =
              Just
                $ S.FromExp
                  [ S.FIJoin
                      $ S.JoinExpr subsInputFromItem S.LeftOuter responseLateralFromItem (S.JoinOn $ S.BELit True)
                  ]
          }
      -- LEFT OUTER JOIN LATERAL ( ... ) _fld_resp
      responseLateralFromItem = S.mkLateralFromItem selectRootFields fldRespAlias

      selectRootFields =
        S.mkSelect
          { S.selExtr = [(S.Extractor rootFieldJsonAggregate (Just $ S.toColumnAlias $ Identifier "root")), cursorExtractor],
            S.selCTEs = customSQLToInnerCTEs customSQLCTEs,
            S.selFrom =
              Just $ S.FromExp [fromSQL]
          }
  pure $ MultiplexedQuery . toQuery $ selectWith
  where
    -- FROM unnest($1::uuid[], $2::json[]) _subs (result_id, result_vars)
    subsInputFromItem =
      S.FIUnnest
        [S.SEPrep 1 `S.SETyAnn` S.TypeAnn "uuid[]", S.SEPrep 2 `S.SETyAnn` S.TypeAnn "json[]"]
        subsAlias
        [resultIdAlias, resultVarsAlias]

    -- json_build_object('field1', field1.root, 'field2', field2.root, ...)
    -- json_build_object should not be used without array https://postgrespro.com/list/thread-id/2486891
    -- Please someone with proper Haskel knowledge change this) 
    -- Postgres has a limitation of maximum 100 function arguments
    -- If you have a table with 50 columns this code produces 100 arguments in the  json_build_object and postgres throws error
     -- And you can't increase this limit in Postgres, the only way is to compile your own version of postgres which is stupid to do otherwise such configuraton can't be done
     -- Same change needs to be done not just in subscription but in othe rplaces as well where json_build is used
     -- From what I understood to fix this should be enough to do something like json_object(array[..,..,..])
     --- Found this in this discussion https://postgrespro.com/list/thread-id/2486891
     -- At the moment Hasura just can't be used with tables with more that 50 columns which is a huge limitation
     -- And you have to unecessarelly split tables, I don't even talk about adding computed fields which these also counts as arguments
    rootFieldJsonAggregate = S.SEFnApp "json_build_object" rootFieldJsonPair Nothing
    rootFieldJsonPair =
      [ S.SELit (G.unName fieldAlias),
        mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "root")
      ]

    -- to_json("root"."cursor") AS "cursor"
    cursorSQLExp = S.SEFnApp "to_json" [mkQualifiedIdentifier (aliasToIdentifier fieldAlias) (Identifier "cursor")] Nothing
    cursorExtractor = S.Extractor cursorSQLExp (Just $ S.toColumnAlias $ Identifier "cursor")
    mkQualifiedIdentifier prefix = S.SEQIdentifier . S.QIdentifier (S.QualifiedIdentifier prefix Nothing)
    aliasToIdentifier = TableIdentifier . G.unName

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
  UVParameter provenance colVal -> do
    varJsonPath <- case provenance of
      FromGraphQL varInfo -> do
        let varName = getName varInfo
        modifying qpiReusableVariableValues $ HashMap.insert varName colVal
        pure ["query", G.unName varName]
      _ -> do
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
      addTypeAnnotation pgType
        $ S.SEOpApp
          (S.SQLOp "#>>")
          [ S.SEQIdentifier $ S.QIdentifier (S.QualifiedIdentifier subsIdentifier Nothing) (Identifier "result_vars"),
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
  m [(CohortId, B.ByteString, PG.ViaJSON CursorVariableValues)]
executeStreamingMultiplexedQuery (MultiplexedQuery query) cohorts = do
  executeQuery query cohorts

-- | Internal; used by both 'executeMultiplexedQuery', 'executeStreamingMultiplexedQuery'
-- and 'pgDBSubscriptionExplain'.
executeQuery ::
  (MonadTx m, PG.FromRes a) =>
  PG.Query ->
  [(CohortId, CohortVariables)] ->
  m a
executeQuery query cohorts =
  let (cohortIds, cohortVars) = unzip cohorts
      preparedArgs = (CohortIdArray cohortIds, CohortVariablesArray cohortVars)
   in liftTx $ PG.withQE defaultTxErrorHandler query preparedArgs True
