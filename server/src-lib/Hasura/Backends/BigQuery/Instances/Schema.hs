{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Schema () where

import Data.Aeson qualified as J
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.GraphQL.Parser hiding (EnumValueInfo, field)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Internal.Parser hiding (field)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Select
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'BigQuery where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields
  buildTableRelayQueryFields = bqBuildTableRelayQueryFields
  buildTableInsertMutationFields = bqBuildTableInsertMutationFields
  buildTableUpdateMutationFields = bqBuildTableUpdateMutationFields
  buildTableDeleteMutationFields = bqBuildTableDeleteMutationFields
  buildFunctionQueryFields = bqBuildFunctionQueryFields
  buildFunctionRelayQueryFields = bqBuildFunctionRelayQueryFields
  buildFunctionMutationFields = bqBuildFunctionMutationFields

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()

  -- table arguments
  tableArguments = bqTableArgs

  -- indivdual components
  columnParser = bqColumnParser
  jsonPathArg = bqJsonPathArg
  orderByOperators = bqOrderByOperators
  comparisonExps = bqComparisonExps
  mkCountType = bqMkCountType
  aggregateOrderByCountType = BigQuery.IntegerScalarType
  computedField = bqComputedField
  node = bqNode

  -- SQL literals
  columnDefaultValue = error "TODO: Make impossible by the type system. BigQuery doesn't support insertions."

----------------------------------------------------------------
-- Top level parsers

bqBuildTableRelayQueryFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  NESeq (ColumnInfo 'BigQuery) ->
  SelPermInfo 'BigQuery ->
  m [a]
bqBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure []

bqBuildTableInsertMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  InsPermInfo 'BigQuery ->
  Maybe (SelPermInfo 'BigQuery) ->
  Maybe (UpdPermInfo 'BigQuery) ->
  m [a]
bqBuildTableInsertMutationFields _sourceName _tableName _tableInfo _gqlName _insPerms _selPerms _updPerms =
  pure []

bqBuildTableUpdateMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  UpdPermInfo 'BigQuery ->
  Maybe (SelPermInfo 'BigQuery) ->
  m [a]
bqBuildTableUpdateMutationFields _sourceName _tableName _tableInfo _gqlName _updPerns _selPerms =
  pure []

bqBuildTableDeleteMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  DelPermInfo 'BigQuery ->
  Maybe (SelPermInfo 'BigQuery) ->
  m [a]
bqBuildTableDeleteMutationFields _sourceName _tableName _tableInfo _gqlName _delPerns _selPerms =
  pure []

bqBuildFunctionQueryFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  FunctionName 'BigQuery ->
  FunctionInfo 'BigQuery ->
  TableName 'BigQuery ->
  SelPermInfo 'BigQuery ->
  m [a]
bqBuildFunctionQueryFields _ _ _ _ _ =
  pure []

bqBuildFunctionRelayQueryFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  FunctionName 'BigQuery ->
  FunctionInfo 'BigQuery ->
  TableName 'BigQuery ->
  NESeq (ColumnInfo 'BigQuery) ->
  SelPermInfo 'BigQuery ->
  m [a]
bqBuildFunctionRelayQueryFields _sourceName _functionName _functionInfo _tableName _pkeyColumns _selPerms =
  pure []

bqBuildFunctionMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  FunctionName 'BigQuery ->
  FunctionInfo 'BigQuery ->
  TableName 'BigQuery ->
  SelPermInfo 'BigQuery ->
  m [a]
bqBuildFunctionMutationFields _ _ _ _ _ =
  pure []

----------------------------------------------------------------
-- Table arguments

bqTableArgs ::
  forall r m n.
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableInfo 'BigQuery ->
  SelPermInfo 'BigQuery ->
  m (InputFieldsParser n (IR.SelectArgsG 'BigQuery (UnpreparedValue 'BigQuery)))
bqTableArgs sourceName tableInfo selectPermissions = do
  whereParser <- tableWhereArg sourceName tableInfo selectPermissions
  orderByParser <- tableOrderByArg sourceName tableInfo selectPermissions
  pure do
    whereArg <- whereParser
    orderByArg <- orderByParser
    limitArg <- tableLimitArg
    offsetArg <- tableOffsetArg
    pure $
      IR.SelectArgs
        { IR._saWhere = whereArg,
          IR._saOrderBy = orderByArg,
          IR._saLimit = limitArg,
          IR._saOffset = offsetArg,
          -- not supported on BigQuery for now
          IR._saDistinct = Nothing
        }

----------------------------------------------------------------
-- Individual components

bqColumnParser ::
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  ColumnType 'BigQuery ->
  G.Nullability ->
  m (Parser 'Both n (ValueWithOrigin (ColumnValue 'BigQuery)))
bqColumnParser columnType (G.Nullability isNullable) =
  peelWithOrigin . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType -> case scalarType of
      -- bytestrings
      -- we only accept string literals
      BigQuery.BytesScalarType -> pure $ possiblyNullable scalarType $ BigQuery.StringValue <$> P.string
      -- text
      BigQuery.StringScalarType -> pure $ possiblyNullable scalarType $ BigQuery.StringValue <$> P.string
      -- floating point values
      -- TODO: we do not perform size checks here, meaning we would accept an
      -- out-of-bounds value as long as it can be represented by a GraphQL float; this
      -- will in all likelihood error on the BigQuery side. Do we want to handle those
      -- properly here?
      BigQuery.FloatScalarType -> pure $ possiblyNullable scalarType $ BigQuery.FloatValue . BigQuery.doubleToFloat64 <$> P.float
      BigQuery.IntegerScalarType -> pure $ possiblyNullable scalarType $ BigQuery.IntegerValue . BigQuery.intToInt64 . fromIntegral <$> P.int
      BigQuery.DecimalScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DecimalValue . BigQuery.doubleToDecimal <$> P.float
      BigQuery.BigDecimalScalarType -> pure $ possiblyNullable scalarType $ BigQuery.BigDecimalValue . BigQuery.doubleToBigDecimal <$> P.float
      -- boolean type
      BigQuery.BoolScalarType -> pure $ possiblyNullable scalarType $ BigQuery.BoolValue <$> P.boolean
      BigQuery.DateScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DateValue . BigQuery.Date <$> P.string
      BigQuery.TimeScalarType -> pure $ possiblyNullable scalarType $ BigQuery.TimeValue . BigQuery.Time <$> P.string
      BigQuery.DatetimeScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DatetimeValue . BigQuery.Datetime <$> P.string
      BigQuery.GeographyScalarType -> pure $ possiblyNullable scalarType $ BigQuery.GeographyValue . BigQuery.Geography <$> P.string
      BigQuery.TimestampScalarType -> do
        let schemaType = P.TNamed P.Nullable $ P.Definition stringScalar Nothing P.TIScalar
        pure $
          possiblyNullable scalarType $
            Parser
              { pType = schemaType,
                pParser =
                  valueToJSON (P.toGraphQLType schemaType)
                    >=> fmap (BigQuery.StringValue . BigQuery.utctimeToISO8601Text)
                      . either (parseErrorWith ParseFailed . qeError) pure
                      . runAesonParser (J.withText "TimestampColumn" BigQuery.textToUTCTime)
              }
      ty -> throwError $ internalError $ T.pack $ "Type currently unsupported for BigQuery: " ++ show ty
    ColumnEnumReference (EnumReference tableName enumValues) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          tableGQLName <- tableGraphQLName @'BigQuery tableName `onLeft` throwError
          enumName <- P.mkTypename $ tableGQLName <> $$(G.litName "_enum")
          pure $ possiblyNullable BigQuery.StringScalarType $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe BigQuery.NullValue) . P.nullable
      | otherwise = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'BigQuery)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) P.EnumValueInfo,
        BigQuery.StringValue $ G.unName value
      )
    throughJSON scalarName =
      let schemaType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing P.TIScalar
       in Parser
            { pType = schemaType,
              pParser =
                valueToJSON (P.toGraphQLType schemaType)
                  >=> either (parseErrorWith ParseFailed . qeError) pure . runAesonParser J.parseJSON
            }

bqJsonPathArg ::
  MonadParse n =>
  ColumnType 'BigQuery ->
  InputFieldsParser n (Maybe (IR.ColumnOp 'BigQuery))
bqJsonPathArg _columnType = pure Nothing

bqOrderByOperators ::
  NonEmpty
    ( Definition P.EnumValueInfo,
      (BasicOrderType 'BigQuery, NullsOrderType 'BigQuery)
    )
bqOrderByOperators =
  NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order, nulls first",
        (BigQuery.AscOrder, BigQuery.NullsFirst)
      ),
      ( define $$(G.litName "asc_nulls_first") "in ascending order, nulls first",
        (BigQuery.AscOrder, BigQuery.NullsFirst)
      ),
      ( define $$(G.litName "asc_nulls_last") "in ascending order, nulls last",
        (BigQuery.AscOrder, BigQuery.NullsLast)
      ),
      ( define $$(G.litName "desc") "in descending order, nulls last",
        (BigQuery.DescOrder, BigQuery.NullsLast)
      ),
      ( define $$(G.litName "desc_nulls_first") "in descending order, nulls first",
        (BigQuery.DescOrder, BigQuery.NullsFirst)
      ),
      ( define $$(G.litName "desc_nulls_last") "in descending order, nulls last",
        (BigQuery.DescOrder, BigQuery.NullsLast)
      )
    ]
  where
    define name desc = P.Definition name (Just desc) P.EnumValueInfo

bqComparisonExps ::
  forall m n r.
  (BackendSchema 'BigQuery, MonadSchema n m, MonadError QErr m, MonadReader r m, Has QueryContext r, Has MkTypename r) =>
  ColumnType 'BigQuery ->
  m (Parser 'Input n [ComparisonExp 'BigQuery])
bqComparisonExps = P.memoize 'comparisonExps $ \columnType -> do
  collapseIfNull <- asks $ qcDangerousBooleanCollapse . getter
  -- see Note [Columns in comparison expression are never nullable]
  typedParser <- columnParser columnType (G.Nullability False)
  nullableTextParser <- columnParser (ColumnScalar @'BigQuery BigQuery.StringScalarType) (G.Nullability True)
  -- textParser <- columnParser (ColumnScalar @'BigQuery BigQuery.StringScalarType) (G.Nullability False)
  let name = P.getName typedParser <> $$(G.litName "_BigQuery_comparison_exp")
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      -- textListParser = fmap openValueOrigin <$> P.list textParser
      columnListParser = fmap openValueOrigin <$> P.list typedParser
      mkListLiteral :: [ColumnValue 'BigQuery] -> UnpreparedValue 'BigQuery
      mkListLiteral =
        P.UVLiteral . BigQuery.ListExpression . fmap (BigQuery.ValueExpression . cvValue)
  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ equalityOperators
                collapseIfNull
                (mkParameter <$> typedParser)
                (mkListLiteral <$> columnListParser),
              comparisonOperators
                collapseIfNull
                (mkParameter <$> typedParser)
            ]

bqMkCountType ::
  -- | distinct values
  Maybe Bool ->
  Maybe [Column 'BigQuery] ->
  CountType 'BigQuery
bqMkCountType _ Nothing = BigQuery.StarCountable
bqMkCountType (Just True) (Just cols) =
  maybe BigQuery.StarCountable BigQuery.DistinctCountable $ nonEmpty cols
bqMkCountType _ (Just cols) =
  maybe BigQuery.StarCountable BigQuery.NonNullFieldCountable $ nonEmpty cols

-- | Computed field parser.
-- Currently unsupported: returns Nothing for now.
bqComputedField ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  ComputedFieldInfo 'BigQuery ->
  TableName 'BigQuery ->
  SelPermInfo 'BigQuery ->
  m (Maybe (FieldParser n (AnnotatedField 'BigQuery)))
bqComputedField _sourceName _fieldInfo _table _selectPemissions = pure Nothing

-- | Remote join field parser.
-- Currently unsupported: returns Nothing for now.
bqRemoteRelationshipField ::
  MonadBuildSchema 'BigQuery r m n =>
  RemoteFieldInfo 'BigQuery ->
  m (Maybe [FieldParser n (AnnotatedField 'BigQuery)])
bqRemoteRelationshipField _remoteFieldInfo = pure Nothing

-- | The 'node' root field of a Relay request. Relay is currently unsupported on BigQuery,
-- meaning this parser will never be called: any attempt to create this parser should
-- therefore fail.
bqNode ::
  MonadBuildSchema 'BigQuery r m n =>
  m
    ( Parser
        'Output
        n
        ( HashMap
            (TableName 'BigQuery)
            ( SourceName,
              SourceConfig 'BigQuery,
              SelPermInfo 'BigQuery,
              PrimaryKeyColumns 'BigQuery,
              AnnotatedFields 'BigQuery
            )
        )
    )
bqNode = throw500 "BigQuery does not support relay; `node` should never be exposed in the schema."
