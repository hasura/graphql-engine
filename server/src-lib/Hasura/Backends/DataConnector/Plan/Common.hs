{-# LANGUAGE DerivingStrategies #-}

-- | This module contains Data Connector request/response planning code and utility
-- functions and types that are common across the different categories of requests
-- (ie queries, mutations, etc). It contains code and concepts that are independent
-- of these different categories.
--
-- Both 'Hasura.Backends.DataConnector.Plan.QueryPlan' and
-- 'Hasura.Backends.DataConnector.Plan.MutationPlan' use the contents of this module,
-- for example 'Hasura.Backends.DataConnector.Plan.QueryPlan.mkQueryPlan`.
module Hasura.Backends.DataConnector.Plan.Common
  ( Plan (..),
    writeOutput,
    replaceOutput,
    TableRelationships (..),
    recordRelationship,
    recordTableRelationshipFromRelInfo,
    FieldPrefix,
    noPrefix,
    prefixWith,
    applyPrefix,
    Cardinality (..),
    RedactionExpressionState (..),
    recordRedactionExpression,
    translateRedactionExpressions,
    prepareLiteral,
    translateBoolExpToExpression,
    mkRelationshipName,
    mapFieldNameHashMap,
    encodeAssocListAsObject,
    targetToTargetName,
    recordNativeQuery,
    ColumnStack,
    emptyColumnStack,
    pushColumn,
    toColumnSelector,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Aeson.Types qualified as J
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (hash)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Data.Tuple (swap)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Backends.Postgres.Instances.API ()
import Hasura.Base.Error
import Hasura.NativeQuery.IR
import Hasura.NativeQuery.InterpolatedQuery as IQ
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend (Backend, SessionVarType, getColVals)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local (RelInfo (..), RelMapping (..), RelTarget (..))
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Session
import Numeric (showIntAtBase)
import Witch qualified

--------------------------------------------------------------------------------

-- | Represents a 'request' to be sent to a data connector agent ('_pRequest') and a function
-- that is capable of reshaping the response to that request into the final JSON form expected
-- to be returned by the GraphQL endpoint ('_pResponseReshaper').
data Plan request response = Plan
  { _pRequest :: request,
    _pResponseReshaper :: forall m. (MonadError QErr m) => response -> m J.Encoding
  }

--------------------------------------------------------------------------------

-- | Writes some output to state, like one might do if one was using a Writer monad.
-- The output is combined with the existing output using '<>' from 'Semigroup'
writeOutput :: (Semigroup output, MonadState state m, Has output state) => output -> m ()
writeOutput x = modify $ modifier (<> x)

-- | Replaces some output in the state with a new version of the output. Also, a value
-- can be returned from the replacement function.
--
-- This is useful if you need to inspect the existing state, make a decision, and update it
-- based on that decision. The result of the decision can be returned from the transformation
-- as your 'a' value.
replaceOutput :: (MonadState state m, Has output state) => (output -> (output, a)) -> m a
replaceOutput replace = do
  output <- gets getter
  let (newOutput, retval) = replace output
  modify (modifier (const newOutput))
  pure retval

--------------------------------------------------------------------------------

-- | A monoidal data structure used to record Table Relationships encountered during request
-- translation. Used with 'recordTableRelationship'.
newtype TableRelationships = TableRelationships
  {unTableRelationships :: HashMap API.TargetName (HashMap API.RelationshipName API.Relationship)}
  deriving stock (Eq, Show)

instance Semigroup TableRelationships where
  (TableRelationships l) <> (TableRelationships r) = TableRelationships $ HashMap.unionWith HashMap.union l r

instance Monoid TableRelationships where
  mempty = TableRelationships mempty

-- | Records a relationship encountered during request translation into the output of the current
-- 'CPS.WriterT'
recordRelationship ::
  ( MonadState state m,
    Has TableRelationships state
  ) =>
  API.TargetName ->
  API.RelationshipName ->
  API.Relationship ->
  m ()
recordRelationship sourceName relationshipName relationship =
  writeOutput $ TableRelationships $ HashMap.singleton sourceName (HashMap.singleton relationshipName relationship)

recordTableRelationshipFromRelInfo ::
  ( MonadState state m,
    Has TableRelationships state
  ) =>
  API.TargetName ->
  RelInfo 'DataConnector ->
  m (API.RelationshipName, API.Relationship)
recordTableRelationshipFromRelInfo sourceTableName RelInfo {..} = do
  let relationshipName = mkRelationshipName riName
  let relationshipType = case riType of
        ObjRel -> API.ObjectRelationship
        ArrRel -> API.ArrayRelationship
  case riTarget of
    RelTargetNativeQuery _ -> error "recordTableRelationshipFromRelInfo RelTargetNativeQuery"
    RelTargetTable targetTableName -> do
      let relationship =
            API.Relationship
              { _rTarget = API.TTable (API.TargetTable (Witch.from targetTableName)),
                _rRelationshipType = relationshipType,
                _rColumnMapping = API.ColumnPathMapping $ HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList (unRelMapping riMapping)
              }
      recordRelationship
        sourceTableName
        relationshipName
        relationship
      pure (relationshipName, relationship)

-- | Records a Native Query encountered during request translation into the output of the current
-- 'CPS.WriterT'
recordNativeQuery ::
  ( Has API.InterpolatedQueries state,
    Has API.ScalarTypesCapabilities r,
    MonadReader r m,
    MonadState state m,
    MonadError QErr m,
    Has SessionVariables r
  ) =>
  NativeQuery 'DataConnector (UnpreparedValue 'DataConnector) ->
  m API.InterpolatedQueryId
recordNativeQuery nq = do
  nqL <- traverse prepareLiteral nq
  let iq@(API.InterpolatedQuery i _) = apiInterpolateQuery nqL
      interpolatedQueries = API.InterpolatedQueries $ HashMap.singleton i iq
  writeOutput interpolatedQueries
  pure i

apiInterpolateQuery :: NativeQuery 'DataConnector Literal -> API.InterpolatedQuery
apiInterpolateQuery NativeQuery {nqRootFieldName, nqInterpolatedQuery} = API.InterpolatedQuery i interpolatedItems
  where
    -- NOTE: An alternative hash mechanism could be explored if any issues are found with this implementation.
    i = API.InterpolatedQueryId (toTxt nqRootFieldName <> "_" <> qh)
    qh = T.pack $ showIntAtBase 16 intToDigit (abs $ hash nqInterpolatedQuery) ""
    interpolatedItems = interpolateItem <$> IQ.getInterpolatedQuery nqInterpolatedQuery

interpolateItem :: IQ.InterpolatedItem Literal -> API.InterpolatedItem
interpolateItem = \case
  IQ.IIText t -> API.InterpolatedText t
  IQ.IIVariable l -> case l of
    ValueLiteral st v -> API.InterpolatedScalar (API.ScalarValue v (Witch.from st)) -- TODO: Witchify?
    _ -> error "array literals not yet implemented"

--------------------------------------------------------------------------------

-- | Collects encountered redaction expressions on a per table/function basis.
-- Expressions are deduplicated and assigned a unique name (within that table/function)
-- that is then used to reference the expression inside the query.
newtype RedactionExpressionState = RedactionExpressionState
  {unRedactionExpressionState :: HashMap API.TargetName (HashMap API.RedactionExpression API.RedactionExpressionName)}
  deriving stock (Eq, Show)

recordRedactionExpression ::
  ( MonadState state m,
    Has TableRelationships state,
    Has RedactionExpressionState state,
    Has API.InterpolatedQueries state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  API.TargetName ->
  AnnRedactionExp 'DataConnector (UnpreparedValue 'DataConnector) ->
  m (Maybe API.RedactionExpressionName)
recordRedactionExpression target = \case
  NoRedaction -> pure Nothing
  RedactIfFalse boolExp -> runMaybeT $ do
    expression <- MaybeT $ fmap API.RedactionExpression <$> translateBoolExpToExpression target boolExp
    replaceOutput $ \existingState@(RedactionExpressionState recordedExps) ->
      let targetRecordedExps = fromMaybe mempty $ HashMap.lookup target recordedExps
       in case HashMap.lookup expression targetRecordedExps of
            Just existingName -> (existingState, existingName)
            Nothing ->
              -- A unique name is generated by counting up from zero as redaction expressions are added
              -- by using the size of the HashMap they are placed into
              let newName = API.RedactionExpressionName $ "RedactionExp" <> tshow (HashMap.size targetRecordedExps)
                  newTargetRecordedExps = HashMap.insert expression newName targetRecordedExps
                  newState = RedactionExpressionState $ HashMap.insert target newTargetRecordedExps recordedExps
               in (newState, newName)

translateRedactionExpressions :: RedactionExpressionState -> Set API.TargetRedactionExpressions
translateRedactionExpressions (RedactionExpressionState redactionsByTarget) =
  redactionsByTarget
    & HashMap.toList
    <&> ( \(targetKey, redactionExps) ->
            API.TargetRedactionExpressions
              { _treTarget = targetKey,
                _treExpressions = HashMap.fromList $ swap <$> HashMap.toList redactionExps
              }
        )
    & Set.fromList

--------------------------------------------------------------------------------

-- | Represents a potential prefix that can be applied to a field name, useful for
-- namespacing field names that may be otherwise duplicated.
newtype FieldPrefix = FieldPrefix (Maybe FieldName)
  deriving stock (Show, Eq)

instance Semigroup FieldPrefix where
  (FieldPrefix Nothing) <> (FieldPrefix something) = FieldPrefix something
  (FieldPrefix something) <> (FieldPrefix Nothing) = FieldPrefix something
  (FieldPrefix (Just l)) <> (FieldPrefix (Just r)) = FieldPrefix . Just $ l <> "_" <> r

instance Monoid FieldPrefix where
  mempty = FieldPrefix Nothing

noPrefix :: FieldPrefix
noPrefix = FieldPrefix Nothing

prefixWith :: FieldName -> FieldPrefix
prefixWith = FieldPrefix . Just

applyPrefix :: FieldPrefix -> FieldName -> FieldName
applyPrefix (FieldPrefix fieldNamePrefix) fieldName = maybe fieldName (\prefix -> prefix <> "_" <> fieldName) fieldNamePrefix

--------------------------------------------------------------------------------

data Cardinality
  = Single
  | Many

--------------------------------------------------------------------------------

prepareLiteral ::
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r
  ) =>
  UnpreparedValue 'DataConnector ->
  m Literal
prepareLiteral = \case
  UVLiteral literal -> pure $ literal
  UVParameter _ e -> pure (ValueLiteral (columnTypeToScalarType $ cvType e) (cvValue e))
  UVSession -> throw400 NotSupported "prepareLiteral: UVSession"
  UVSessionVar sessionVarType sessionVar -> do
    sessionVariables <- asks getter
    textValue <-
      getSessionVariableValue sessionVar sessionVariables
        `onNothing` throw400 NotSupported ("prepareLiteral: session var not found: " <>> sessionVar)
    parseSessionVariable sessionVar sessionVarType textValue

parseSessionVariable ::
  forall m r.
  (MonadError QErr m, MonadReader r m, Has API.ScalarTypesCapabilities r) =>
  SessionVariable ->
  SessionVarType 'DataConnector ->
  Text ->
  m Literal
parseSessionVariable varName varType varValue = do
  scalarTypesCapabilities <- asks getter
  case varType of
    CollectableTypeScalar scalarType@(ScalarType customTypeName) ->
      parseCustomValue scalarTypesCapabilities scalarType (customTypeName <> " JSON value")
    CollectableTypeArray scalarType@(ScalarType customTypeName) ->
      parseCustomArray scalarTypesCapabilities scalarType ("JSON array of " <> customTypeName <> " JSON values")
  where
    parseCustomValue :: API.ScalarTypesCapabilities -> ScalarType -> Text -> m Literal
    parseCustomValue scalarTypesCapabilities scalarType description = do
      case lookupGraphQLType scalarTypesCapabilities scalarType of
        Just GraphQLString ->
          -- Special case for string: uses literal session variable value rather than trying to parse a JSON string
          pure . ValueLiteral scalarType $ J.String varValue
        _ ->
          parseValue' (parseValue scalarTypesCapabilities scalarType) (ValueLiteral scalarType) description

    parseCustomArray :: API.ScalarTypesCapabilities -> ScalarType -> Text -> m Literal
    parseCustomArray scalarTypesCapabilities scalarType =
      parseValue' parser (ArrayLiteral scalarType)
      where
        parser :: (J.Value -> J.Parser [J.Value])
        parser = J.withArray "array of JSON values" (fmap toList . traverse (parseValue scalarTypesCapabilities scalarType))

    parseValue' :: (J.Value -> J.Parser a) -> (a -> Literal) -> Text -> m Literal
    parseValue' parser toLiteral description =
      toLiteral
        <$> (J.eitherDecodeStrict' valValueBS >>= J.parseEither parser)
        `onLeft` (\err -> throw400 ParseFailed ("Expected " <> description <> " for session variable " <> varName <<> ". " <> T.pack err))

    valValueBS :: BS.ByteString
    valValueBS = TE.encodeUtf8 varValue

--------------------------------------------------------------------------------

newtype ColumnStack = ColumnStack [ColumnName]

emptyColumnStack :: ColumnStack
emptyColumnStack = ColumnStack []

pushColumn :: ColumnStack -> ColumnName -> ColumnStack
pushColumn (ColumnStack stack) columnName = ColumnStack $ columnName : stack

toColumnSelector :: ColumnStack -> ColumnName -> API.ColumnSelector
toColumnSelector (ColumnStack []) columnName =
  API.ColumnSelectorColumn $ Witch.from columnName
toColumnSelector (ColumnStack stack) columnName =
  API.ColumnSelectorPath $ NonEmpty.reverse $ Witch.from columnName :| fmap Witch.from stack

--------------------------------------------------------------------------------

translateBoolExpToExpression ::
  ( MonadState state m,
    Has TableRelationships state,
    Has API.InterpolatedQueries state,
    Has RedactionExpressionState state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  API.TargetName ->
  AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
  m (Maybe API.Expression)
translateBoolExpToExpression sourceName boolExp = do
  removeAlwaysTrueExpression <$> translateBoolExp sourceName emptyColumnStack boolExp

translateBoolExp ::
  ( MonadState state m,
    Has TableRelationships state,
    Has API.InterpolatedQueries state,
    Has RedactionExpressionState state,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r,
    MonadIO m
  ) =>
  API.TargetName ->
  ColumnStack ->
  AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
  m API.Expression
translateBoolExp sourceName columnStack = \case
  BoolAnd xs ->
    mkIfZeroOrMany API.And . mapMaybe removeAlwaysTrueExpression <$> traverse (translateBoolExp sourceName columnStack) xs
  BoolOr xs ->
    mkIfZeroOrMany API.Or . mapMaybe removeAlwaysFalseExpression <$> traverse (translateBoolExp sourceName columnStack) xs
  BoolNot x ->
    API.Not <$> (translateBoolExp sourceName columnStack) x
  BoolField (AVColumn c redactionExp opExps) -> do
    let columnSelector = toColumnSelector columnStack $ ciColumn c
    redactionExpName <- recordRedactionExpression sourceName redactionExp
    mkIfZeroOrMany API.And <$> traverse (translateOp columnSelector (Witch.from . columnTypeToScalarType $ ciType c) redactionExpName) opExps
  BoolField (AVNestedObject NestedObjectInfo {..} nestedExp) ->
    translateBoolExp sourceName (pushColumn columnStack _noiColumn) nestedExp
  BoolField (AVRelationship relationshipInfo (RelationshipFilters {rfTargetTablePermissions, rfFilter})) -> do
    (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceName relationshipInfo
    -- TODO: How does this function keep track of the root table?
    API.Exists (API.RelatedTable relationshipName) <$> translateBoolExp (targetToTargetName _rTarget) emptyColumnStack (BoolAnd [rfTargetTablePermissions, rfFilter])
  BoolField (AVRemoteRelationship (RemoteRelPermBoolExp _rawRelBoolExp (lhsCol, lhsColType) rhsFetchInfo)) -> do
    let colFld = fromCol @('DataConnector) $ lhsCol
    lookupLst <-
      AB.dispatchAnyBackend @Backend rhsFetchInfo \((RemoteRelRHSFetchInfo (colType, col) tableName filterExp remoteSourceName sourceConfig) :: RemoteRelRHSFetchInfo f b) -> do
        let colFieldName = rrrfweColumnFieldName filterExp
            colFieldBoolExp = rrrfweBoolExp filterExp
        sessionVariables <- asks getter
        result <- getColVals @b sessionVariables remoteSourceName sourceConfig tableName (colType, col) (colFieldName, colFieldBoolExp)
        pure (fmap (J.String) result)
    pure
      $ API.ApplyBinaryArrayComparisonOperator API.In (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector (API.ColumnName (toTxt colFld))) (API.ScalarType (toTxt lhsColType)) Nothing) lookupLst (API.ScalarType (toTxt lhsColType))
  BoolExists GExists {..} ->
    let tableName = Witch.from _geTable
     in API.Exists (API.UnrelatedTable tableName) <$> translateBoolExp (API.TNTable tableName) emptyColumnStack _geWhere
  where
    -- Makes an 'API.Expression' like 'API.And' if there is zero or many input expressions otherwise
    -- just returns the singleton expression. This helps remove redundant 'API.And' etcs from the expression.
    mkIfZeroOrMany :: (Set API.Expression -> API.Expression) -> [API.Expression] -> API.Expression
    mkIfZeroOrMany mk = \case
      [singleExp] -> singleExp
      zeroOrManyExps -> mk $ Set.fromList zeroOrManyExps

-- | Helper function to convert targets into Keys
targetToTargetName :: API.Target -> API.TargetName
targetToTargetName = \case
  (API.TTable (API.TargetTable tn)) -> API.TNTable tn
  (API.TFunction (API.TargetFunction n _)) -> API.TNFunction n
  (API.TInterpolated (API.TargetInterpolatedQuery qId)) -> API.TNInterpolatedQuery qId

removeAlwaysTrueExpression :: API.Expression -> Maybe API.Expression
removeAlwaysTrueExpression = \case
  API.And exprs | exprs == mempty -> Nothing
  API.Not (API.Or exprs) | exprs == mempty -> Nothing
  other -> Just other

removeAlwaysFalseExpression :: API.Expression -> Maybe API.Expression
removeAlwaysFalseExpression = \case
  API.Or exprs | exprs == mempty -> Nothing
  API.Not (API.And exprs) | exprs == mempty -> Nothing
  other -> Just other

translateOp ::
  ( MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r,
    Has SessionVariables r
  ) =>
  API.ColumnSelector ->
  API.ScalarType ->
  Maybe API.RedactionExpressionName ->
  OpExpG 'DataConnector (UnpreparedValue 'DataConnector) ->
  m API.Expression
translateOp columnName columnType redactionExpName opExp = do
  preparedOpExp <- traverse prepareLiteral $ opExp
  case preparedOpExp of
    AEQ _ (ValueLiteral scalarType value) ->
      pure $ mkApplyBinaryComparisonOperatorToScalar API.Equal value scalarType
    AEQ _ (ArrayLiteral _scalarType _array) ->
      throw400 NotSupported "Array literals not supported for AEQ operator"
    ANE _ (ValueLiteral scalarType value) ->
      pure . API.Not $ mkApplyBinaryComparisonOperatorToScalar API.Equal value scalarType
    ANE _ (ArrayLiteral _scalarType _array) ->
      throw400 NotSupported "Array literals not supported for ANE operator"
    AGT (ValueLiteral scalarType value) ->
      pure $ mkApplyBinaryComparisonOperatorToScalar API.GreaterThan value scalarType
    AGT (ArrayLiteral _scalarType _array) ->
      throw400 NotSupported "Array literals not supported for AGT operator"
    ALT (ValueLiteral scalarType value) ->
      pure $ mkApplyBinaryComparisonOperatorToScalar API.LessThan value scalarType
    ALT (ArrayLiteral _scalarType _array) ->
      throw400 NotSupported "Array literals not supported for ALT operator"
    AGTE (ValueLiteral scalarType value) ->
      pure $ mkApplyBinaryComparisonOperatorToScalar API.GreaterThanOrEqual value scalarType
    AGTE (ArrayLiteral _scalarType _array) ->
      throw400 NotSupported "Array literals not supported for AGTE operator"
    ALTE (ValueLiteral scalarType value) ->
      pure $ mkApplyBinaryComparisonOperatorToScalar API.LessThanOrEqual value scalarType
    ALTE (ArrayLiteral _scalarType _array) ->
      throw400 NotSupported "Array literals not supported for ALTE operator"
    ANISNULL ->
      pure $ API.ApplyUnaryComparisonOperator API.IsNull currentComparisonColumn
    ANISNOTNULL ->
      pure $ API.Not (API.ApplyUnaryComparisonOperator API.IsNull currentComparisonColumn)
    AIN literal -> pure $ inOperator literal
    ANIN literal -> pure . API.Not $ inOperator literal
    CEQ rootOrCurrentColumn ->
      pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.Equal rootOrCurrentColumn
    CNE rootOrCurrentColumn ->
      pure $ API.Not $ mkApplyBinaryComparisonOperatorToAnotherColumn API.Equal rootOrCurrentColumn
    CGT rootOrCurrentColumn ->
      pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.GreaterThan rootOrCurrentColumn
    CLT rootOrCurrentColumn ->
      pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.LessThan rootOrCurrentColumn
    CGTE rootOrCurrentColumn ->
      pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.GreaterThanOrEqual rootOrCurrentColumn
    CLTE rootOrCurrentColumn ->
      pure $ mkApplyBinaryComparisonOperatorToAnotherColumn API.LessThanOrEqual rootOrCurrentColumn
    ALIKE _literal ->
      throw400 NotSupported "The ALIKE operator is not supported by the Data Connector backend"
    ANLIKE _literal ->
      throw400 NotSupported "The ANLIKE operator is not supported by the Data Connector backend"
    ACast _literal ->
      throw400 NotSupported "The ACast operator is not supported by the Data Connector backend"
    ABackendSpecific CustomBooleanOperator {..} -> case _cboRHS of
      Nothing -> pure $ API.ApplyUnaryComparisonOperator (API.CustomUnaryComparisonOperator _cboName) currentComparisonColumn
      Just (ValueLiteral scalarType value) ->
        pure $ mkApplyBinaryComparisonOperatorToScalar (API.CustomBinaryComparisonOperator _cboName) value scalarType
      Just (ArrayLiteral scalarType array) ->
        pure $ API.ApplyBinaryArrayComparisonOperator (API.CustomBinaryArrayComparisonOperator _cboName) currentComparisonColumn array (Witch.from scalarType)
  where
    currentComparisonColumn :: API.ComparisonColumn
    currentComparisonColumn = API.ComparisonColumn API.CurrentTable columnName columnType redactionExpName

    mkApplyBinaryComparisonOperatorToAnotherColumn :: API.BinaryComparisonOperator -> RootOrCurrentColumn 'DataConnector -> API.Expression
    mkApplyBinaryComparisonOperatorToAnotherColumn operator (RootOrCurrentColumn rootOrCurrent otherColumnName) =
      let columnPath = case rootOrCurrent of
            IsRoot -> API.QueryTable
            IsCurrent -> API.CurrentTable
          otherColumnSelector = API.mkColumnSelector $ Witch.from otherColumnName
       in -- TODO(dmoverton): allow otherColumnName to refer to nested object fields.
          API.ApplyBinaryComparisonOperator operator currentComparisonColumn (API.AnotherColumnComparison $ API.ComparisonColumn columnPath otherColumnSelector columnType Nothing)

    inOperator :: Literal -> API.Expression
    inOperator literal =
      let (values, scalarType) = case literal of
            ArrayLiteral scalarType' array -> (array, scalarType')
            ValueLiteral scalarType' value -> ([value], scalarType')
       in API.ApplyBinaryArrayComparisonOperator API.In currentComparisonColumn values (Witch.from scalarType)

    mkApplyBinaryComparisonOperatorToScalar :: API.BinaryComparisonOperator -> J.Value -> ScalarType -> API.Expression
    mkApplyBinaryComparisonOperatorToScalar operator value scalarType =
      API.ApplyBinaryComparisonOperator operator currentComparisonColumn (API.ScalarValueComparison $ API.ScalarValue value (Witch.from scalarType))

--------------------------------------------------------------------------------

mkRelationshipName :: RelName -> API.RelationshipName
mkRelationshipName relName = API.RelationshipName $ toTxt relName

mapFieldNameHashMap :: HashMap FieldName v -> HashMap API.FieldName v
mapFieldNameHashMap = HashMap.mapKeys (API.FieldName . getFieldNameTxt)

--------------------------------------------------------------------------------

encodeAssocListAsObject :: [(Text, J.Encoding)] -> J.Encoding
encodeAssocListAsObject =
  JE.dict
    JE.text
    id
    (\fn -> foldr (uncurry fn))
