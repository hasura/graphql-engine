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
  tableArguments = defaultTableArgs

  -- indivdual components
  columnParser = bqColumnParser
  jsonPathArg = bqJsonPathArg
  orderByOperators = bqOrderByOperators
  comparisonExps = bqComparisonExps
  countTypeInput = bqCountTypeInput
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
  m [a]
bqBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns =
  pure []

bqBuildTableInsertMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  Scenario ->
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  m [a]
bqBuildTableInsertMutationFields _scenario _sourceName _tableName _tableInfo _gqlName =
  pure []

bqBuildTableUpdateMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  m [a]
bqBuildTableUpdateMutationFields _sourceName _tableName _tableInfo _gqlName =
  pure []

bqBuildTableDeleteMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  G.Name ->
  m [a]
bqBuildTableDeleteMutationFields _sourceName _tableName _tableInfo _gqlName =
  pure []

bqBuildFunctionQueryFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  FunctionName 'BigQuery ->
  FunctionInfo 'BigQuery ->
  TableName 'BigQuery ->
  m [a]
bqBuildFunctionQueryFields _ _ _ _ =
  pure []

bqBuildFunctionRelayQueryFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  FunctionName 'BigQuery ->
  FunctionInfo 'BigQuery ->
  TableName 'BigQuery ->
  NESeq (ColumnInfo 'BigQuery) ->
  m [a]
bqBuildFunctionRelayQueryFields _sourceName _functionName _functionInfo _tableName _pkeyColumns =
  pure []

bqBuildFunctionMutationFields ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  FunctionName 'BigQuery ->
  FunctionInfo 'BigQuery ->
  TableName 'BigQuery ->
  m [a]
bqBuildFunctionMutationFields _ _ _ _ =
  pure []

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
      BigQuery.BytesScalarType -> pure $ possiblyNullable scalarType $ BigQuery.StringValue <$> stringBased $$(G.litName "Bytes")
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
      BigQuery.DateScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DateValue . BigQuery.Date <$> stringBased $$(G.litName "Date")
      BigQuery.TimeScalarType -> pure $ possiblyNullable scalarType $ BigQuery.TimeValue . BigQuery.Time <$> stringBased $$(G.litName "Time")
      BigQuery.DatetimeScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DatetimeValue . BigQuery.Datetime <$> stringBased $$(G.litName "Datetime")
      BigQuery.GeographyScalarType ->
        pure $ possiblyNullable scalarType $ BigQuery.GeographyValue . BigQuery.Geography <$> throughJSON $$(G.litName "Geography")
      BigQuery.TimestampScalarType ->
        pure $ possiblyNullable scalarType $ BigQuery.TimestampValue . BigQuery.Timestamp <$> stringBased $$(G.litName "Timestamp")
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
    stringBased :: MonadParse m => G.Name -> Parser 'Both m Text
    stringBased scalarName =
      P.string {pType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing P.TIScalar}

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
  dWithinGeogOpParser <- geographyWithinDistanceInput
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
            [ -- from https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types:
              -- GEOGRAPHY comparisons are not supported. To compare GEOGRAPHY values, use ST_Equals.
              guard (isScalarColumnWhere (/= BigQuery.GeographyScalarType) columnType)
                *> equalityOperators
                  collapseIfNull
                  (mkParameter <$> typedParser)
                  (mkListLiteral <$> columnListParser),
              guard (isScalarColumnWhere (/= BigQuery.GeographyScalarType) columnType)
                *> comparisonOperators
                  collapseIfNull
                  (mkParameter <$> typedParser),
              -- Ops for String type
              guard (isScalarColumnWhere (== BigQuery.StringScalarType) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_like")
                       (Just "does the column match the given pattern")
                       (ALIKE . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_nlike")
                       (Just "does the column NOT match the given pattern")
                       (ANLIKE . mkParameter <$> typedParser)
                   ],
              -- Ops for Bytes type
              guard (isScalarColumnWhere (== BigQuery.BytesScalarType) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_like")
                       (Just "does the column match the given pattern")
                       (ALIKE . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_nlike")
                       (Just "does the column NOT match the given pattern")
                       (ANLIKE . mkParameter <$> typedParser)
                   ],
              -- Ops for Geography type
              guard (isScalarColumnWhere (== BigQuery.GeographyScalarType) columnType)
                *> [ mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_contains")
                       (Just "does the column contain the given geography value")
                       (ABackendSpecific . BigQuery.ASTContains . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_equals")
                       (Just "is the column equal to given geography value (directionality is ignored)")
                       (ABackendSpecific . BigQuery.ASTEquals . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_touches")
                       (Just "does the column have at least one point in common with the given geography value")
                       (ABackendSpecific . BigQuery.ASTTouches . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_within")
                       (Just "is the column contained in the given geography value")
                       (ABackendSpecific . BigQuery.ASTWithin . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_intersects")
                       (Just "does the column spatially intersect the given geography value")
                       (ABackendSpecific . BigQuery.ASTIntersects . mkParameter <$> typedParser),
                     mkBoolOperator
                       collapseIfNull
                       $$(G.litName "_st_d_within")
                       (Just "is the column within a given distance from the given geometry value")
                       (ABackendSpecific . BigQuery.ASTDWithin <$> dWithinGeogOpParser)
                   ]
            ]

bqCountTypeInput ::
  MonadParse n =>
  Maybe (Parser 'Both n (Column 'BigQuery)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'BigQuery)
bqCountTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional $$(G.litName "columns") Nothing $ P.list columnEnum
    pure $ flip mkCountType columns
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe [Column 'BigQuery] -> CountType 'BigQuery
    mkCountType _ Nothing = BigQuery.StarCountable
    mkCountType IR.SelectCountDistinct (Just cols) =
      maybe BigQuery.StarCountable BigQuery.DistinctCountable $ nonEmpty cols
    mkCountType IR.SelectCountNonDistinct (Just cols) =
      maybe BigQuery.StarCountable BigQuery.NonNullFieldCountable $ nonEmpty cols

geographyWithinDistanceInput ::
  forall m n r.
  (MonadSchema n m, MonadError QErr m, MonadReader r m, Has MkTypename r) =>
  m (Parser 'Input n (DWithinGeogOp (UnpreparedValue 'BigQuery)))
geographyWithinDistanceInput = do
  geographyParser <- columnParser (ColumnScalar BigQuery.GeographyScalarType) (G.Nullability False)
  -- practically BigQuery (as of 2021-11-19) doesn't support TRUE as use_spheroid parameter for ST_DWITHIN
  booleanParser <- columnParser (ColumnScalar BigQuery.BoolScalarType) (G.Nullability True)
  floatParser <- columnParser (ColumnScalar BigQuery.FloatScalarType) (G.Nullability False)
  pure $
    P.object $$(G.litName "st_dwithin_input") Nothing $
      DWithinGeogOp <$> (mkParameter <$> P.field $$(G.litName "distance") Nothing floatParser)
        <*> (mkParameter <$> P.field $$(G.litName "from") Nothing geographyParser)
        <*> (mkParameter <$> P.fieldWithDefault $$(G.litName "use_spheroid") Nothing (G.VBoolean False) booleanParser)

-- | Computed field parser.
-- Currently unsupported: returns Nothing for now.
bqComputedField ::
  MonadBuildSchema 'BigQuery r m n =>
  SourceName ->
  ComputedFieldInfo 'BigQuery ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  m (Maybe (FieldParser n (AnnotatedField 'BigQuery)))
bqComputedField _sourceName _fieldInfo _table _tableInfo = pure Nothing

-- | Remote join field parser.
-- Currently unsupported: returns Nothing for now.
bqRemoteRelationshipField ::
  MonadBuildSchema 'BigQuery r m n =>
  RemoteFieldInfo (DBJoinField 'BigQuery) ->
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
