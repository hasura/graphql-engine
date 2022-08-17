{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.BigQuery.Instances.Schema () where

import Data.Aeson qualified as J
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Backends.BigQuery.Name
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    MonadMemoize,
    MonadParse,
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select
import Hasura.GraphQL.Schema.Table
import Hasura.GraphQL.Schema.Typename
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Source (SourceInfo)
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

----------------------------------------------------------------
-- BackendSchema instance

instance BackendSchema 'BigQuery where
  -- top level parsers
  buildTableQueryAndSubscriptionFields = GSB.buildTableQueryAndSubscriptionFields
  buildTableRelayQueryFields _ _ _ _ _ _ = pure []
  buildTableStreamingSubscriptionFields = GSB.buildTableStreamingSubscriptionFields
  buildTableInsertMutationFields _ _ _ _ _ _ = pure []
  buildTableUpdateMutationFields _ _ _ _ _ _ = pure []
  buildTableDeleteMutationFields _ _ _ _ _ _ = pure []
  buildFunctionQueryFields _ _ _ _ _ = pure []
  buildFunctionRelayQueryFields _ _ _ _ _ _ = pure []
  buildFunctionMutationFields _ _ _ _ _ = pure []

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Nothing

  -- individual components
  columnParser = bqColumnParser
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

----------------------------------------------------------------
-- Individual components

bqColumnParser ::
  (MonadParse n, MonadError QErr m, MonadReader r m, Has MkTypename r, Has NamingCase r) =>
  ColumnType 'BigQuery ->
  G.Nullability ->
  m (Parser 'Both n (IR.ValueWithOrigin (ColumnValue 'BigQuery)))
bqColumnParser columnType (G.Nullability isNullable) =
  peelWithOrigin . fmap (ColumnValue columnType) <$> case columnType of
    ColumnScalar scalarType -> case scalarType of
      -- bytestrings
      -- we only accept string literals
      BigQuery.BytesScalarType -> pure $ possiblyNullable scalarType $ BigQuery.StringValue <$> stringBased _Bytes
      -- text
      BigQuery.StringScalarType -> pure $ possiblyNullable scalarType $ BigQuery.StringValue <$> P.string
      -- floating point values
      -- TODO: we do not perform size checks here, meaning we would accept an
      -- out-of-bounds value as long as it can be represented by a GraphQL float; this
      -- will in all likelihood error on the BigQuery side. Do we want to handle those
      -- properly here?
      BigQuery.FloatScalarType -> pure $ possiblyNullable scalarType $ BigQuery.FloatValue . BigQuery.doubleToFloat64 <$> P.float
      BigQuery.IntegerScalarType -> pure $ possiblyNullable scalarType $ BigQuery.IntegerValue . BigQuery.intToInt64 . fromIntegral <$> P.int
      BigQuery.DecimalScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DecimalValue . BigQuery.Decimal . BigQuery.scientificToText <$> P.scientific
      BigQuery.BigDecimalScalarType -> pure $ possiblyNullable scalarType $ BigQuery.BigDecimalValue . BigQuery.BigDecimal . BigQuery.scientificToText <$> P.scientific
      -- boolean type
      BigQuery.BoolScalarType -> pure $ possiblyNullable scalarType $ BigQuery.BoolValue <$> P.boolean
      BigQuery.DateScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DateValue . BigQuery.Date <$> stringBased _Date
      BigQuery.TimeScalarType -> pure $ possiblyNullable scalarType $ BigQuery.TimeValue . BigQuery.Time <$> stringBased _Time
      BigQuery.DatetimeScalarType -> pure $ possiblyNullable scalarType $ BigQuery.DatetimeValue . BigQuery.Datetime <$> stringBased _Datetime
      BigQuery.GeographyScalarType ->
        pure $ possiblyNullable scalarType $ BigQuery.GeographyValue . BigQuery.Geography <$> throughJSON _Geography
      BigQuery.TimestampScalarType ->
        pure $ possiblyNullable scalarType $ BigQuery.TimestampValue . BigQuery.Timestamp <$> stringBased _Timestamp
      ty -> throwError $ internalError $ T.pack $ "Type currently unsupported for BigQuery: " ++ show ty
    ColumnEnumReference enumRef@(EnumReference _ enumValues _) ->
      case nonEmpty (Map.toList enumValues) of
        Just enumValuesList -> do
          enumName <- mkEnumTypeName enumRef
          pure $ possiblyNullable BigQuery.StringScalarType $ P.enum enumName Nothing (mkEnumValue <$> enumValuesList)
        Nothing -> throw400 ValidationFailed "empty enum values"
  where
    possiblyNullable _scalarType
      | isNullable = fmap (fromMaybe BigQuery.NullValue) . P.nullable
      | otherwise = id
    mkEnumValue :: (EnumValue, EnumValueInfo) -> (P.Definition P.EnumValueInfo, ScalarValue 'BigQuery)
    mkEnumValue (EnumValue value, EnumValueInfo description) =
      ( P.Definition value (G.Description <$> description) Nothing [] P.EnumValueInfo,
        BigQuery.StringValue $ G.unName value
      )
    throughJSON scalarName =
      let schemaType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing Nothing [] P.TIScalar
       in P.Parser
            { pType = schemaType,
              pParser =
                P.valueToJSON (P.toGraphQLType schemaType)
                  >=> either (P.parseErrorWith P.ParseFailed . toErrorMessage . qeError) pure . runAesonParser J.parseJSON
            }
    stringBased :: MonadParse m => G.Name -> Parser 'Both m Text
    stringBased scalarName =
      P.string {P.pType = P.TNamed P.NonNullable $ P.Definition scalarName Nothing Nothing [] P.TIScalar}

bqOrderByOperators ::
  NamingCase ->
  ( G.Name,
    NonEmpty
      ( P.Definition P.EnumValueInfo,
        (BasicOrderType 'BigQuery, NullsOrderType 'BigQuery)
      )
  )
bqOrderByOperators _tCase =
  (Name._order_by,) $
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
  m (Parser 'Input n [ComparisonExp 'BigQuery])
bqComparisonExps = P.memoize 'comparisonExps $ \columnType -> do
  collapseIfNull <- retrieve Options.soDangerousBooleanCollapse

  dWithinGeogOpParser <- geographyWithinDistanceInput
  tCase <- asks getter
  -- see Note [Columns in comparison expression are never nullable]
  typedParser <- columnParser columnType (G.Nullability False)
  -- textParser <- columnParser (ColumnScalar @'BigQuery BigQuery.StringScalarType) (G.Nullability False)
  let name = P.getName typedParser <> Name.__BigQuery_comparison_exp
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      -- textListParser = fmap openValueOrigin <$> P.list textParser
      columnListParser = fmap IR.openValueOrigin <$> P.list typedParser
      mkListLiteral :: [ColumnValue 'BigQuery] -> IR.UnpreparedValue 'BigQuery
      mkListLiteral =
        IR.UVLiteral . BigQuery.ListExpression . fmap (BigQuery.ValueExpression . cvValue)
  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
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
  MonadParse n =>
  Maybe (Parser 'Both n (Column 'BigQuery)) ->
  InputFieldsParser n (IR.CountDistinct -> CountType 'BigQuery)
bqCountTypeInput = \case
  Just columnEnum -> do
    columns <- P.fieldOptional Name._columns Nothing $ P.list columnEnum
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
  (MonadMemoize m, MonadBuildSchema 'BigQuery r m n) =>
  m (Parser 'Input n (DWithinGeogOp (IR.UnpreparedValue 'BigQuery)))
geographyWithinDistanceInput = do
  geographyParser <- columnParser (ColumnScalar BigQuery.GeographyScalarType) (G.Nullability False)
  -- practically BigQuery (as of 2021-11-19) doesn't support TRUE as use_spheroid parameter for ST_DWITHIN
  booleanParser <- columnParser (ColumnScalar BigQuery.BoolScalarType) (G.Nullability True)
  floatParser <- columnParser (ColumnScalar BigQuery.FloatScalarType) (G.Nullability False)
  pure $
    P.object Name._st_dwithin_input Nothing $
      DWithinGeogOp <$> (IR.mkParameter <$> P.field Name._distance Nothing floatParser)
        <*> (IR.mkParameter <$> P.field Name._from Nothing geographyParser)
        <*> (IR.mkParameter <$> P.fieldWithDefault Name._use_spheroid Nothing (G.VBoolean False) booleanParser)

-- | Computed field parser.
bqComputedField ::
  forall r m n.
  MonadBuildSchema 'BigQuery r m n =>
  SourceInfo 'BigQuery ->
  ComputedFieldInfo 'BigQuery ->
  TableName 'BigQuery ->
  TableInfo 'BigQuery ->
  m (Maybe (FieldParser n (AnnotatedField 'BigQuery)))
bqComputedField sourceName ComputedFieldInfo {..} tableName tableInfo = runMaybeT do
  stringifyNumbers <- retrieve Options.soStringifyNumbers
  roleName <- retrieve scRole
  fieldName <- lift $ textToName $ computedFieldNameToText _cfiName
  functionArgsParser <- lift $ computedFieldFunctionArgs _cfiFunction
  case _cfiReturnType of
    BigQuery.ReturnExistingTable returnTable -> do
      returnTableInfo <- lift $ askTableInfo sourceName returnTable
      returnTablePermissions <- hoistMaybe $ tableSelectPermissions roleName returnTableInfo
      selectionSetParser <- MaybeT (fmap (P.multiple . P.nonNullableParser) <$> tableSelectionSet sourceName returnTableInfo)
      selectArgsParser <- lift $ tableArguments sourceName returnTableInfo
      let fieldArgsParser = liftA2 (,) functionArgsParser selectArgsParser
      pure $
        P.subselection fieldName fieldDescription fieldArgsParser selectionSetParser
          <&> \((functionArgs', args), fields) ->
            IR.AFComputedField _cfiXComputedFieldInfo _cfiName $
              IR.CFSTable JASMultipleRows $
                IR.AnnSelectG
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
      guard $ Map.member _cfiName $ spiComputedFields selectPermissions
      objectTypeName <-
        mkTypename =<< do
          computedFieldGQLName <- textToName $ computedFieldNameToText _cfiName
          pure $ computedFieldGQLName <> Name.__ <> Name.__fields
      selectionSetParser <- do
        fieldParsers <- lift $ for returnFields selectArbitraryField
        let description = G.Description $ "column fields returning by " <>> _cfiName
        pure $
          P.selectionSetObject objectTypeName (Just description) fieldParsers []
            <&> parsedSelectionsToFields IR.AFExpression
      pure $
        P.subselection fieldName fieldDescription functionArgsParser selectionSetParser
          <&> \(functionArgs', fields) ->
            IR.AFComputedField _cfiXComputedFieldInfo _cfiName $
              IR.CFSTable JASMultipleRows $
                IR.AnnSelectG
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
      m (FieldParser n (AnnotatedField 'BigQuery))
    selectArbitraryField (columnName, graphQLName, columnType) = do
      field <- columnParser @'BigQuery (ColumnScalar columnType) (G.Nullability True)
      pure $
        P.selection_ graphQLName Nothing field
          $> IR.mkAnnColumnField columnName (ColumnScalar columnType) Nothing Nothing

    computedFieldFunctionArgs ::
      ComputedFieldFunction 'BigQuery ->
      m (InputFieldsParser n (FunctionArgsExp 'BigQuery (IR.UnpreparedValue 'BigQuery)))
    computedFieldFunctionArgs ComputedFieldFunction {..} = do
      let fieldName = Name._args
          fieldDesc =
            G.Description $
              "input parameters for computed field "
                <> _cfiName <<> " defined on table " <>> tableName

      objectName <-
        mkTypename =<< do
          computedFieldGQLName <- textToName $ computedFieldNameToText _cfiName
          tableGQLName <- getTableGQLName @'BigQuery tableInfo
          pure $ computedFieldGQLName <> Name.__ <> tableGQLName <> Name.__args

      let userInputArgs = filter (not . flip Map.member _cffComputedFieldImplicitArgs . BigQuery._faName) (toList _cffInputArgs)

      argumentParsers <- sequenceA <$> forM userInputArgs parseArgument

      let objectParser =
            P.object objectName Nothing argumentParsers `P.bind` \inputArguments -> do
              let tableColumnInputs = Map.map BigQuery.AETableColumn $ Map.mapKeys getFuncArgNameTxt _cffComputedFieldImplicitArgs
              pure $ FunctionArgsExp mempty $ Map.fromList inputArguments <> tableColumnInputs

      pure $ P.field fieldName (Just fieldDesc) objectParser

    parseArgument :: BigQuery.FunctionArgument -> m (InputFieldsParser n (Text, BigQuery.ArgumentExp (IR.UnpreparedValue 'BigQuery)))
    parseArgument arg = do
      typedParser <- columnParser (ColumnScalar $ BigQuery._faType arg) (G.Nullability False)
      let argumentName = getFuncArgNameTxt $ BigQuery._faName arg
      fieldName <- textToName argumentName
      let argParser = P.field fieldName Nothing typedParser
      pure $ argParser `P.bindFields` \inputValue -> pure ((argumentName, BigQuery.AEInput $ IR.mkParameter inputValue))
