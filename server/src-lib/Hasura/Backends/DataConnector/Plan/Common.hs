{-# LANGUAGE DeriveAnyClass #-}
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
    TableRelationships (..),
    TableRelationshipsKey (..),
    FieldPrefix,
    noPrefix,
    prefixWith,
    applyPrefix,
    Cardinality (..),
    recordTableRelationship,
    recordTableRelationshipFromRelInfo,
    prepareLiteral,
    translateBoolExpToExpression,
    mkRelationshipName,
    mapFieldNameHashMap,
    encodeAssocListAsObject,
  )
where

import Control.Monad.Trans.Writer.CPS qualified as CPS
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.Aeson.Types qualified as J
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as BS
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Extended (toTxt, (<<>), (<>>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend
import Hasura.Backends.DataConnector.Adapter.Types
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend (SessionVarType)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local (RelInfo (..), RelTarget (..))
import Hasura.SQL.Types (CollectableType (..))
import Hasura.Session
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

-- | Key datatype for TableRelationships to avoid having an Either directly as the key,
--   and make extending the types of relationships easier in future.
data TableRelationshipsKey
  = FunctionNameKey API.FunctionName
  | TableNameKey API.TableName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | A monoidal data structure used to record Table Relationships encountered during request
-- translation. Used with 'recordTableRelationship'.
newtype TableRelationships = TableRelationships
  {unTableRelationships :: HashMap TableRelationshipsKey (HashMap API.RelationshipName API.Relationship)}
  deriving stock (Eq, Show)

instance Semigroup TableRelationships where
  (TableRelationships l) <> (TableRelationships r) = TableRelationships $ HashMap.unionWith HashMap.union l r

instance Monoid TableRelationships where
  mempty = TableRelationships mempty

-- | Records a table relationship encountered during request translation into the output of the current
-- 'CPS.WriterT'
recordTableRelationship ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  TableRelationshipsKey ->
  API.RelationshipName ->
  API.Relationship ->
  CPS.WriterT writerOutput m ()
recordTableRelationship sourceName relationshipName relationship =
  let newRelationship = TableRelationships $ HashMap.singleton sourceName (HashMap.singleton relationshipName relationship)
   in CPS.tell $ modifier (const newRelationship) mempty

recordTableRelationshipFromRelInfo ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m
  ) =>
  TableRelationshipsKey ->
  RelInfo 'DataConnector ->
  CPS.WriterT writerOutput m (API.RelationshipName, API.Relationship)
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
              { _rTargetTable = Witch.from targetTableName,
                _rRelationshipType = relationshipType,
                _rColumnMapping = HashMap.fromList $ bimap Witch.from Witch.from <$> HashMap.toList riMapping
              }
      recordTableRelationship
        sourceTableName
        relationshipName
        relationship
      pure (relationshipName, relationship)

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
  (MonadError QErr m, MonadReader r m, Has API.ScalarTypesCapabilities r) =>
  SessionVariables ->
  UnpreparedValue 'DataConnector ->
  m Literal
prepareLiteral sessionVariables = \case
  UVLiteral literal -> pure $ literal
  UVParameter _ e -> pure (ValueLiteral (columnTypeToScalarType $ cvType e) (cvValue e))
  UVSession -> throw400 NotSupported "prepareLiteral: UVSession"
  UVSessionVar sessionVarType sessionVar -> do
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

translateBoolExpToExpression ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r
  ) =>
  SessionVariables ->
  TableRelationshipsKey ->
  AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m (Maybe API.Expression)
translateBoolExpToExpression sessionVariables sourceName boolExp = do
  removeAlwaysTrueExpression <$> translateBoolExp sessionVariables sourceName boolExp

translateBoolExp ::
  ( Has TableRelationships writerOutput,
    Monoid writerOutput,
    MonadError QErr m,
    MonadReader r m,
    Has API.ScalarTypesCapabilities r
  ) =>
  SessionVariables ->
  TableRelationshipsKey ->
  AnnBoolExp 'DataConnector (UnpreparedValue 'DataConnector) ->
  CPS.WriterT writerOutput m API.Expression
translateBoolExp sessionVariables sourceName = \case
  BoolAnd xs ->
    mkIfZeroOrMany API.And . mapMaybe removeAlwaysTrueExpression <$> traverse (translateBoolExp' sourceName) xs
  BoolOr xs ->
    mkIfZeroOrMany API.Or . mapMaybe removeAlwaysFalseExpression <$> traverse (translateBoolExp' sourceName) xs
  BoolNot x ->
    API.Not <$> (translateBoolExp' sourceName) x
  BoolField (AVColumn c _redactionExp opExps) ->
    -- TODO(redactionExp): Deal with the redaction expression
    lift $ mkIfZeroOrMany API.And <$> traverse (translateOp sessionVariables (Witch.from $ ciColumn c) (Witch.from . columnTypeToScalarType $ ciType c)) opExps
  BoolField (AVRelationship relationshipInfo (RelationshipFilters {rfTargetTablePermissions, rfFilter})) -> do
    (relationshipName, API.Relationship {..}) <- recordTableRelationshipFromRelInfo sourceName relationshipInfo
    -- TODO: How does this function keep track of the root table?
    API.Exists (API.RelatedTable relationshipName) <$> translateBoolExp' (TableNameKey _rTargetTable) (BoolAnd [rfTargetTablePermissions, rfFilter])
  BoolExists GExists {..} ->
    let tableName = Witch.from _geTable
     in API.Exists (API.UnrelatedTable tableName) <$> translateBoolExp' (TableNameKey tableName) _geWhere
  where
    translateBoolExp' = translateBoolExp sessionVariables

    -- Makes an 'API.Expression' like 'API.And' if there is zero or many input expressions otherwise
    -- just returns the singleton expression. This helps remove redundant 'API.And' etcs from the expression.
    mkIfZeroOrMany :: (Set API.Expression -> API.Expression) -> [API.Expression] -> API.Expression
    mkIfZeroOrMany mk = \case
      [singleExp] -> singleExp
      zeroOrManyExps -> mk $ Set.fromList zeroOrManyExps

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
  (MonadError QErr m, MonadReader r m, Has API.ScalarTypesCapabilities r) =>
  SessionVariables ->
  API.ColumnName ->
  API.ScalarType ->
  OpExpG 'DataConnector (UnpreparedValue 'DataConnector) ->
  m API.Expression
translateOp sessionVariables columnName columnType opExp = do
  preparedOpExp <- traverse (prepareLiteral sessionVariables) $ opExp
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
    currentComparisonColumn = API.ComparisonColumn API.CurrentTable columnName columnType

    mkApplyBinaryComparisonOperatorToAnotherColumn :: API.BinaryComparisonOperator -> RootOrCurrentColumn 'DataConnector -> API.Expression
    mkApplyBinaryComparisonOperatorToAnotherColumn operator (RootOrCurrentColumn rootOrCurrent otherColumnName) =
      let columnPath = case rootOrCurrent of
            IsRoot -> API.QueryTable
            IsCurrent -> API.CurrentTable
       in API.ApplyBinaryComparisonOperator operator currentComparisonColumn (API.AnotherColumnComparison $ API.ComparisonColumn columnPath (Witch.from otherColumnName) columnType)

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
