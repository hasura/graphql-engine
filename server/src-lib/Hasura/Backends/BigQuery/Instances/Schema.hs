{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Schema () where

import Data.Aeson qualified as J
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Backends.BigQuery.DDL (scalarTypeFromColumnType)
import Hasura.Backends.BigQuery.Name
import Hasura.Backends.BigQuery.Parser.Scalars qualified as BQP
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.Function.Cache
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    MonadParse,
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.LogicalModel.Schema (defaultLogicalModelArgs, defaultLogicalModelSelectionSet)
import Hasura.Name qualified as Name
import Hasura.NativeQuery.Schema qualified as NativeQueries
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'BigQuery where
  -- top level parsers
  buildTableQueryAndSubscriptionFields = GSB.buildTableQueryAndSubscriptionFields
  buildTableRelayQueryFields _ _ _ _ _ = pure []
  buildTableStreamingSubscriptionFields = GSB.buildTableStreamingSubscriptionFields
  buildTableInsertMutationFields _ _ _ _ _ = pure []
  buildTableUpdateMutationFields _ _ _ = pure []
  buildTableDeleteMutationFields _ _ _ _ _ = pure []
  buildFunctionQueryFields _ _ _ _ = pure []
  buildFunctionRelayQueryFields _ _ _ _ _ = pure []
  buildFunctionMutationFields _ _ _ _ = pure []
  buildNativeQueryRootFields = NativeQueries.defaultBuildNativeQueryRootFields

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Nothing

  -- individual components
  columnParser = bqColumnParser
  enumParser = bqEnumParser
  possiblyNullable = const bqPossiblyNullable
  scalarSelectionArgumentsParser _ = pure Nothing
  orderByOperators _sourceInfo = bqOrderByOperators
  comparisonExps = bqComparisonExps
  countTypeInput = bqCountTypeInput
  aggregateOrderByCountType = BigQuery.IntegerScalarType
  computedField = bqComputedField

instance BackendTableSelectSchema 'BigQuery where
  tableArguments = defaultTableArgs
  selectTable = defaultSelectTable
  selectTableAggregate = defaultSelectTableAggregate
  tableSelectionSet = defaultTableSelectionSet

instance BackendNativeQuerySelectSchema 'BigQuery where
  selectNativeQuery = NativeQueries.defaultSelectNativeQuery
  selectNativeQueryObject = NativeQueries.defaultSelectNativeQueryObject

instance BackendLogicalModelSelectSchema 'BigQuery where
  logicalModelArguments = defaultLogicalModelArgs
  logicalModelSelectionSet = defaultLogicalModelSelectionSet

----------------------------------------------------------------
-- Individual components

bqColumnParser ::
  (MonadBuildSchema 'BigQuery r m n) =>
  ColumnType 'BigQuery ->
  G.Nullability ->
  SchemaT r m (Parser 'Both n (IR.ValueWithOrigin (ColumnValue 'BigQuery)))
bqColumnParser columnType nullability = case columnType of
  ColumnScalar scalarType -> P.memoizeOn 'bqColumnParser (columnType, nullability) do
    Options.SchemaOptions {soBigQueryStringNumericInput} <- asks getter
    let numericInputParser :: forall a. a -> a -> a
        numericInputParser builtin custom =
          case soBigQueryStringNumericInput of
            Options.EnableBigQueryStringNumericInput -> custom
            Options.DisableBigQueryStringNumericInput -> builtin
    peelWithOrigin
      . fmap (ColumnValue columnType)
      . bqPossiblyNullable nullability
      <$> case scalarType of
        -- bytestrings
        -- we only accept string literals
        BigQuery.BytesScalarType -> pure $ BigQuery.StringValue <$> stringBased _Bytes
        -- text
        BigQuery.StringScalarType -> pure $ BigQuery.StringValue <$> P.string
        -- floating point values

        BigQuery.FloatScalarType ->
          pure
            $ BigQuery.FloatValue
            <$> numericInputParser (BigQuery.doubleToFloat64 <$> P.float) BQP.bqFloat64
        BigQuery.IntegerScalarType ->
          pure
            $ BigQuery.IntegerValue
            <$> numericInputParser (BigQuery.intToInt64 . fromIntegral <$> P.int) BQP.bqInt64
        BigQuery.DecimalScalarType ->
          pure
            $ BigQuery.DecimalValue
            <$> numericInputParser
              (BigQuery.Decimal . BigQuery.scientificToText <$> P.scientific)
              BQP.bqDecimal
        BigQuery.BigDecimalScalarType ->
          pure
            $ BigQuery.BigDecimalValue
            <$> numericInputParser
              (BigQuery.BigDecimal . BigQuery.scientificToText <$> P.scientific)
              BQP.bqBigDecimal
        -- boolean type
        BigQuery.BoolScalarType -> pure $ BigQuery.BoolValue <$> P.boolean
        BigQuery.DateScalarType -> pure $ BigQuery.DateValue . BigQuery.Date <$> stringBased _Date
        BigQuery.TimeScalarType -> pure $ BigQuery.TimeValue . BigQuery.Time <$> stringBased _Time
        BigQuery.JsonScalarType -> pure $ BigQuery.JsonValue <$> P.json
        BigQuery.DatetimeScalarType -> pure $ BigQuery.DatetimeValue . BigQuery.Datetime <$> stringBased _Datetime
        BigQuery.GeographyScalarType ->
          pure $ BigQuery.GeographyValue . BigQuery.Geography <$> throughJSON _Geography
        BigQuery.TimestampScalarType ->
          pure $ BigQuery.TimestampValue . BigQuery.Timestamp <$> stringBased _Timestamp
        ty -> throwError $ internalError $ T.pack $ "Type currently unsupported for BigQuery: " ++ show ty
  ColumnEnumReference (EnumReference tableName enumValues customTableName) ->
    case nonEmpty (HashMap.toList enumValues) of
      Just enumValuesList ->
        peelWithOrigin
          . fmap (ColumnValue columnType)
          . bqPossiblyNullable nullability
          <$> bqEnumParser tableName enumValuesList customTableName nullability
      Nothing -> throw400 ValidationFailed "empty enum values"
  where
    throughJSON scalarName =
      let schemaType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing Nothing [] P.TIScalar
       in P.Parser
            { pType = schemaType,
              pParser =
                P.valueToJSON (P.toGraphQLType schemaType)
                  >=> either (P.parseErrorWith P.ParseFailed . toErrorMessage . qeError) pure
                  . runAesonParser J.parseJSON
            }
    stringBased :: (MonadParse m) => G.Name -> Parser 'Both m Text
    stringBased scalarName =
      P.string {P.pType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing Nothing [] P.TIScalar}

bqEnumParser ::
  (MonadBuildSchema 'BigQuery r m n) =>
  TableName 'BigQuery ->
  NonEmpty (EnumValue, EnumValueInfo) ->
  Maybe G.Name ->
  G.Nullability ->
  SchemaT r m (Parser 'Both n (ScalarValue 'BigQuery))
bqEnumParser tableName enumValues customTableName nullability = do
  enumName <- mkEnumTypeName @'BigQuery tableName customTableName
  pure $ bqPossiblyNullable nullability $ P.enum enumName Nothing (mkEnumValue <$> enumValues)
  where
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'BigQuery)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) Nothing [] P.EnumValueInfo,
        BigQuery.StringValue $ G.unName value
      )

bqPossiblyNullable ::
  (MonadParse m) =>
  G.Nullability ->
  Parser 'Both m (ScalarValue 'BigQuery) ->
  Parser 'Both m (ScalarValue 'BigQuery)
bqPossiblyNullable (G.Nullability isNullable)
  | isNullable = fmap (fromMaybe BigQuery.NullValue) . P.nullable
  | otherwise = id

bqOrderByOperators ::
  NamingCase ->
  ( G.Name,
    NonEmpty
      ( P.Definition P.EnumValueInfo,
        (BasicOrderType 'BigQuery, NullsOrderType 'BigQuery)
      )
  )
bqOrderByOperators _tCase =
  (Name._order_by,)
    $
    -- NOTE: NamingCase is not being used here as we don't support naming conventions for this DB
    NE.fromList
      [ ( define Name._asc "in ascending order, nulls first",
          (BigQuery.AscOrder, BigQuery.NullsFirst)
        ),
        ( define Name._asc_nulls_first "in ascending order, nulls first",
          (BigQuery.AscOrder, BigQuery.NullsFirst)
        ),
        ( define Name._asc_nulls_last "in ascending order, nulls last",
          (BigQuery.AscOrder, BigQuery.NullsLast)
        ),
        ( define Name._desc "in descending order, nulls last",
          (BigQuery.DescOrder, BigQuery.NullsLast)
        ),
        ( define Name._desc_nulls_first "in descending order, nulls first",
          (BigQuery.DescOrder, BigQuery.NullsFirst)
        ),
        ( define Name._desc_nulls_last "in descending order, nulls last",
          (BigQuery.DescOrder, BigQuery.NullsLast)
        )
      ]
  where
    define name desc = P.Definition name (Just desc) Nothing [] P.EnumValueInfo

bqComparisonExps ::
  forall m n r.
  (MonadBuildSchema 'BigQuery r m n) =>
  ColumnType 'BigQuery ->
  SchemaT r m (Parser 'Input n [ComparisonExp 'BigQuery])
bqComparisonExps = P.memoize 'comparisonExps $ \columnType -> do
  collapseIfNull <- retrieve Options.soDangerousBooleanCollapse

  dWithinGeogOpParser <- geographyWithinDistanceInput
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @'BigQuery
  -- see Note [Columns in comparison expression are never nullable]
  typedParser <- columnParser columnType (G.Nullability False)
  -- textParser <- columnParser (ColumnScalar @'BigQuery BigQuery.StringScalarType) (G.Nullability False)
  let name = P.getName typedParser <> Name.__BigQuery_comparison_exp
      desc =
        G.Description
          $ "Boolean expression to compare columns of type "
          <> P.getName typedParser
          <<> ". All fields are combined with logical 'AND'."
      -- textListParser = fmap openValueOrigin <$> P.list textParser
      columnListParser = fmap IR.openValueOrigin <$> P.list typedParser
      mkListLiteral :: [ColumnValue 'BigQuery] -> IR.UnpreparedValue 'BigQuery
      mkListLiteral =
        IR.UVLiteral
          . BigQuery.ListExpression
          . fmap
            ( \columnValue ->
                BigQuery.ValueExpression
                  ( BigQuery.TypedValue
                      (scalarTypeFromColumnType (cvType columnValue))
                      (cvValue columnValue)
                  )
            )
  pure
    $ P.object name (Just desc)
    $ fmap catMaybes
    $ sequenceA
    $ concat
      [ -- from https://cloud.google.com/bigquery/docs/reference/standard-sql/data-types:
        -- GEOGRAPHY comparisons are not supported. To compare GEOGRAPHY values, use ST_Equals.
        guard (isScalarColumnWhere (/= BigQuery.GeographyScalarType) columnType)
          *> equalityOperators
            tCase
            collapseIfNull
            (IR.mkParameter <$> typedParser)
            (mkListLiteral <$> columnListParser),
        guard (isScalarColumnWhere (/= BigQuery.GeographyScalarType) columnType)
          *> comparisonOperators
            tCase
            collapseIfNull
            (IR.mkParameter <$> typedParser),
        -- Ops for String type
        guard (isScalarColumnWhere (== BigQuery.StringScalarType) columnType)
          *> [ mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedName Name.__like)
                 (Just "does the column match the given pattern")
                 (ALIKE . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedName Name.__nlike)
                 (Just "does the column NOT match the given pattern")
                 (ANLIKE . IR.mkParameter <$> typedParser)
             ],
        -- Ops for Bytes type
        guard (isScalarColumnWhere (== BigQuery.BytesScalarType) columnType)
          *> [ mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedName Name.__like)
                 (Just "does the column match the given pattern")
                 (ALIKE . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedName Name.__nlike)
                 (Just "does the column NOT match the given pattern")
                 (ANLIKE . IR.mkParameter <$> typedParser)
             ],
        -- Ops for Geography type
        guard (isScalarColumnWhere (== BigQuery.GeographyScalarType) columnType)
          *> [ mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "contains"]))
                 (Just "does the column contain the given geography value")
                 (ABackendSpecific . BigQuery.ASTContains . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "equals"]))
                 (Just "is the column equal to given geography value (directionality is ignored)")
                 (ABackendSpecific . BigQuery.ASTEquals . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "touches"]))
                 (Just "does the column have at least one point in common with the given geography value")
                 (ABackendSpecific . BigQuery.ASTTouches . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "within"]))
                 (Just "is the column contained in the given geography value")
                 (ABackendSpecific . BigQuery.ASTWithin . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "intersects"]))
                 (Just "does the column spatially intersect the given geography value")
                 (ABackendSpecific . BigQuery.ASTIntersects . IR.mkParameter <$> typedParser),
               mkBoolOperator
                 tCase
                 collapseIfNull
                 (C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["_st", "d", "within"]))
                 (Just "is the column within a given distance from the given geometry value")
                 (ABackendSpecific . BigQuery.ASTDWithin <$> dWithinGeogOpParser)
             ]
      ]

bqCountTypeInput ::
  (MonadParse n) =>
  Maybe (Parser 'Both n (Column 'BigQuery, AnnRedactionExpUnpreparedValue 'BigQuery)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'BigQuery (IR.UnpreparedValue 'BigQuery))
bqCountTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional Name._columns Nothing $ P.list columnEnum
    pure $ flip mkCountType columns
  Nothing -> pure $ flip mkCountType Nothing
  where
    mkCountType :: IR.CountDistinct -> Maybe [(Column 'BigQuery, AnnRedactionExpUnpreparedValue 'BigQuery)] -> CountType 'BigQuery (IR.UnpreparedValue 'BigQuery)
    mkCountType IR.SelectCountDistinct (Just (col : cols)) = BigQuery.CountType (BigQuery.DistinctCountable (col :| cols))
    mkCountType IR.SelectCountNonDistinct (Just (col : cols)) = BigQuery.CountType (BigQuery.NonNullFieldCountable (col :| cols))
    mkCountType _ _ = BigQuery.CountType BigQuery.StarCountable

geographyWithinDistanceInput ::
  forall m n r.
  (MonadBuildSchema 'BigQuery r m n) =>
  SchemaT r m (Parser 'Input n (DWithinGeogOp (IR.UnpreparedValue 'BigQuery)))
geographyWithinDistanceInput = do
  geographyParser <- columnParser (ColumnScalar BigQuery.GeographyScalarType) (G.Nullability False)
  -- practically BigQuery (as of 2021-11-19) doesn't support TRUE as use_spheroid parameter for ST_DWITHIN
  booleanParser <- columnParser (ColumnScalar BigQuery.BoolScalarType) (G.Nullability True)
  floatParser <- columnParser (ColumnScalar BigQuery.FloatScalarType) (G.Nullability False)
  pure
    $ P.object Name._st_dwithin_input Nothing
    $ DWithinGeogOp
    <$> (IR.mkParameter <$> P.field Name._distance Nothing floatParser)
    <*> (IR.mkParameter <$> P.field Name._from Nothing geographyParser)
    <*> (IR.mkParameter <$> P.fieldWithDefault Name._use_spheroid Nothing (G.VBoolean False) booleanParser)

-- | Computed field parser.
bqComputedField ::
  forall r m n.
  (MonadBuildSchema 'BigQuery r m n) =>
  ComputedFieldInfo 'BigQuery ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  SchemaT r m (Maybe (FieldParser n (AnnotatedField 'BigQuery)))
bqComputedField ComputedFieldInfo {..} tableName tableInfo = runMaybeT do
  sourceInfo :: SourceInfo 'BigQuery <- asks getter
  let customization = _siCustomization sourceInfo
      mkTypename = runMkTypename $ _rscTypeNames customization
  stringifyNumbers <- retrieve Options.soStringifyNumbers
  roleName <- retrieve scRole
  fieldName <- lift $ textToName $ computedFieldNameToText _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs mkTypename _cfiFunction
  case _cfiReturnType of
    BigQuery.ReturnExistingTable returnTable -> do
      returnTableInfo <- lift $ askTableInfo returnTable
      returnTablePermissions <- hoistMaybe $ tableSelectPermissions roleName returnTableInfo
      selectionSetParser <- MaybeT (fmap (P.multiple . P.nonNullableParser) <$> tableSelectionSet returnTableInfo)
      selectArgsParser <- lift $ tableArguments returnTableInfo
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure
        $ P.subselection fieldName fieldDescription fieldArgsParser selectionSetParser
        <&> \((functionArgs', args), fields) ->
          IR.AFComputedField _cfiXComputedFieldInfo _cfiName
            $ IR.CFSTable JASMultipleRows
            $ IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom = IR.FromFunction (_cffName _cfiFunction) functionArgs' Nothing,
                IR._asnPerm = tablePermissionsInfo returnTablePermissions,
                IR._asnArgs = args,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Nothing
              }
    BigQuery.ReturnTableSchema returnFields -> do
      -- Check if the computed field is available in the select permission
      selectPermissions <- hoistMaybe $ tableSelectPermissions roleName tableInfo
      guard $ HashMap.member _cfiName $ spiComputedFields selectPermissions
      objectTypeName <-
        mkTypename <$> do
          computedFieldGQLName <- textToName $ computedFieldNameToText _cfiName
          pure $ computedFieldGQLName <> Name.__ <> Name.__fields
      selectionSetParser <- do
        fieldParsers <- lift $ for returnFields selectArbitraryField
        let description = G.Description $ "column fields returning by " <>> _cfiName
        pure
          $ P.selectionSetObject objectTypeName (Just description) fieldParsers []
          <&> parsedSelectionsToFields IR.AFExpression
      pure
        $ P.subselection fieldName fieldDescription functionArgsParser selectionSetParser
        <&> \(functionArgs', fields) ->
          IR.AFComputedField _cfiXComputedFieldInfo _cfiName
            $ IR.CFSTable JASMultipleRows
            $ IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom = IR.FromFunction (_cffName _cfiFunction) functionArgs' Nothing,
                IR._asnPerm = IR.noTablePermissions,
                IR._asnArgs = IR.noSelectArgs,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Nothing
              }
  where
    fieldDescription :: Maybe G.Description
    fieldDescription = G.Description <$> _cfiDescription

    selectArbitraryField ::
      (BigQuery.ColumnName, G.Name, BigQuery.ScalarType) ->
      SchemaT r m (FieldParser n (AnnotatedField 'BigQuery))
    selectArbitraryField (columnName, graphQLName, columnType) = do
      field <- columnParser @'BigQuery (ColumnScalar columnType) (G.Nullability True)
      pure
        $ P.selection_ graphQLName Nothing field
        $> IR.mkAnnColumnField columnName (ColumnScalar columnType) NoRedaction Nothing

    computedFieldFunctionArgs ::
      (G.Name -> G.Name) ->
      ComputedFieldFunction 'BigQuery ->
      SchemaT r m (InputFieldsParser n (FunctionArgsExp 'BigQuery (IR.UnpreparedValue 'BigQuery)))
    computedFieldFunctionArgs mkTypename ComputedFieldFunction {..} = do
      objectName <-
        mkTypename <$> do
          computedFieldGQLName <- textToName $ computedFieldNameToText _cfiName
          tableGQLName <- getTableGQLName @'BigQuery tableInfo
          pure $ computedFieldGQLName <> Name.__ <> tableGQLName <> Name.__args

      let userInputArgs = filter (not . flip HashMap.member _cffComputedFieldImplicitArgs . BigQuery._faName) (toList _cffInputArgs)

      argumentParsers <- sequenceA <$> forM userInputArgs parseArgument

      let userArgsParser = P.object objectName Nothing argumentParsers

      let fieldDesc =
            G.Description
              $ "input parameters for computed field "
              <> _cfiName
              <<> " defined on table "
              <>> tableName

          argsField
            | null userInputArgs = P.fieldOptional Name._args (Just fieldDesc) userArgsParser
            | otherwise = Just <$> P.field Name._args (Just fieldDesc) userArgsParser

      pure
        $ argsField
        `P.bindFields` \maybeInputArguments -> do
          let tableColumnInputs = HashMap.map BigQuery.AETableColumn $ HashMap.mapKeys getFuncArgNameTxt _cffComputedFieldImplicitArgs
          pure $ FunctionArgsExp mempty $ maybe mempty HashMap.fromList maybeInputArguments <> tableColumnInputs

    parseArgument :: BigQuery.FunctionArgument -> SchemaT r m (InputFieldsParser n (Text, BigQuery.ArgumentExp (IR.UnpreparedValue 'BigQuery)))
    parseArgument arg = do
      typedParser <- columnParser (ColumnScalar $ BigQuery._faType arg) (G.Nullability False)
      let argumentName = getFuncArgNameTxt $ BigQuery._faName arg
      fieldName <- textToName argumentName
      let argParser = P.field fieldName Nothing typedParser
      pure $ argParser `P.bindFields` \inputValue -> pure ((argumentName, BigQuery.AEInput $ IR.mkParameter inputValue))
