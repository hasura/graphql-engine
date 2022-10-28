{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Schema () where

--------------------------------------------------------------------------------

import Control.Lens ((^.))
import Data.Aeson qualified as J
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text.Casing (GQLNameIdentifier, fromCustomName)
import Data.Text.Extended ((<<>))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Adapter.Backend (CustomBooleanOperator (..), columnTypeToScalarType)
import Hasura.Backends.DataConnector.Adapter.Types qualified as DC
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend (BackendSchema (..), BackendTableSelectSchema (..), ComparisonExp, MonadBuildSchema)
import Hasura.GraphQL.Schema.BoolExp qualified as GS.BE
import Hasura.GraphQL.Schema.Build qualified as GS.B
import Hasura.GraphQL.Schema.Common qualified as GS.C
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select qualified as GS.S
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Backend qualified as RQL
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.RQL.Types.Source qualified as RQL
import Hasura.RQL.Types.SourceCustomization qualified as RQL
import Hasura.RQL.Types.Table qualified as RQL
import Hasura.SQL.Backend (BackendType (..))
import Language.GraphQL.Draft.Syntax qualified as GQL
import Witch qualified

--------------------------------------------------------------------------------

instance BackendSchema 'DataConnector where
  -- top level parsers
  buildTableQueryAndSubscriptionFields = GS.B.buildTableQueryAndSubscriptionFields

  buildTableRelayQueryFields = experimentalBuildTableRelayQueryFields

  buildFunctionQueryFields _ _ _ _ _ = pure []
  buildFunctionRelayQueryFields _ _ _ _ _ _ = pure []
  buildFunctionMutationFields _ _ _ _ _ = pure []
  buildTableInsertMutationFields _ _ _ _ _ _ = pure []
  buildTableUpdateMutationFields _ _ _ _ _ _ = pure []
  buildTableDeleteMutationFields _ _ _ _ _ _ = pure []
  buildTableStreamingSubscriptionFields _ _ _ _ _ = pure []

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Just ()
  streamSubscriptionExtension = Nothing

  -- individual components
  columnParser = columnParser'
  enumParser = enumParser'
  possiblyNullable = possiblyNullable'
  scalarSelectionArgumentsParser _ = pure Nothing
  orderByOperators = orderByOperators'
  comparisonExps = comparisonExps'

  countTypeInput = countTypeInput'
  aggregateOrderByCountType = DC.NumberTy
  computedField =
    error "computedField: not implemented for the Data Connector backend."

instance BackendTableSelectSchema 'DataConnector where
  tableArguments = tableArgs'
  selectTable = GS.S.defaultSelectTable
  selectTableAggregate = GS.S.defaultSelectTableAggregate
  tableSelectionSet = GS.S.defaultTableSelectionSet

--------------------------------------------------------------------------------

experimentalBuildTableRelayQueryFields ::
  MonadBuildSchema 'DataConnector r m n =>
  RQL.MkRootFieldName ->
  RQL.SourceInfo 'DataConnector ->
  RQL.TableName 'DataConnector ->
  RQL.TableInfo 'DataConnector ->
  GQLNameIdentifier ->
  NESeq (RQL.ColumnInfo 'DataConnector) ->
  GS.C.SchemaT r m [P.FieldParser n a]
experimentalBuildTableRelayQueryFields _mkRootFieldName _sourceName _tableName _tableInfo _gqlName _pkeyColumns =
  pure []

columnParser' ::
  MonadBuildSchema 'DataConnector r m n =>
  RQL.ColumnType 'DataConnector ->
  GQL.Nullability ->
  GS.C.SchemaT r m (P.Parser 'P.Both n (IR.ValueWithOrigin (RQL.ColumnValue 'DataConnector)))
columnParser' columnType nullability = case columnType of
  RQL.ColumnScalar scalarType ->
    P.memoizeOn 'columnParser' (scalarType, nullability) $
      GS.C.peelWithOrigin . fmap (RQL.ColumnValue columnType) . possiblyNullable' scalarType nullability
        <$> case scalarType of
          DC.StringTy -> pure $ J.String <$> P.string
          DC.NumberTy -> pure $ J.Number <$> P.scientific
          DC.BoolTy -> pure $ J.Bool <$> P.boolean
          DC.CustomTy name -> do
            gqlName <-
              GQL.mkName name
                `onNothing` throw400 ValidationFailed ("The column type name " <> name <<> " is not a valid GraphQL name")
            pure $ P.jsonScalar gqlName (Just "A custom scalar type")
  RQL.ColumnEnumReference (RQL.EnumReference tableName enumValues customTableName) ->
    case nonEmpty (Map.toList enumValues) of
      Just enumValuesList ->
        GS.C.peelWithOrigin . fmap (RQL.ColumnValue columnType)
          <$> enumParser' tableName enumValuesList customTableName nullability
      Nothing -> throw400 ValidationFailed "empty enum values"

enumParser' ::
  MonadError QErr m =>
  RQL.TableName 'DataConnector ->
  NonEmpty (RQL.EnumValue, RQL.EnumValueInfo) ->
  Maybe GQL.Name ->
  GQL.Nullability ->
  GS.C.SchemaT r m (P.Parser 'P.Both n (RQL.ScalarValue 'DataConnector))
enumParser' _tableName _enumValues _customTableName _nullability =
  throw400 NotSupported "This column type is unsupported by the Data Connector backend"

possiblyNullable' ::
  MonadParse m =>
  RQL.ScalarType 'DataConnector ->
  GQL.Nullability ->
  P.Parser 'P.Both m J.Value ->
  P.Parser 'P.Both m J.Value
possiblyNullable' _scalarType (GQL.Nullability isNullable)
  | isNullable = fmap (fromMaybe J.Null) . P.nullable
  | otherwise = id

orderByOperators' :: RQL.SourceInfo 'DataConnector -> NamingCase -> (GQL.Name, NonEmpty (P.Definition P.EnumValueInfo, (RQL.BasicOrderType 'DataConnector, RQL.NullsOrderType 'DataConnector)))
orderByOperators' RQL.SourceInfo {_siConfiguration} _tCase =
  let dcName = DC._scDataConnectorName _siConfiguration
      orderBy = GQL.addSuffixes (DC.unDataConnectorName dcName) [$$(GQL.litSuffix "_order_by")]
   in (orderBy,) $
        -- NOTE: NamingCase is not being used here as we don't support naming conventions for this DB
        NE.fromList
          [ ( define $$(GQL.litName "asc") "in ascending order",
              (DC.Ascending, ())
            ),
            ( define $$(GQL.litName "desc") "in descending order",
              (DC.Descending, ())
            )
          ]
  where
    define name desc = P.Definition name (Just desc) Nothing [] P.EnumValueInfo

comparisonExps' ::
  forall m n r.
  MonadBuildSchema 'DataConnector r m n =>
  RQL.SourceInfo 'DataConnector ->
  RQL.ColumnType 'DataConnector ->
  GS.C.SchemaT r m (P.Parser 'P.Input n [ComparisonExp 'DataConnector])
comparisonExps' sourceInfo columnType = P.memoizeOn 'comparisonExps' (dataConnectorName, columnType) $ do
  tCase <- asks getter
  collapseIfNull <- GS.C.retrieve Options.soDangerousBooleanCollapse

  typedParser <- columnParser' columnType (GQL.Nullability False)
  let name = GQL.addSuffixes (P.getName typedParser) [$$(GQL.litSuffix "_"), GQL.convertNameToSuffix (DC.unDataConnectorName dataConnectorName), $$(GQL.litSuffix "_comparison_exp")]
      desc =
        GQL.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      columnListParser = fmap IR.openValueOrigin <$> P.list typedParser
  customOperators <- (fmap . fmap . fmap) IR.ABackendSpecific <$> mkCustomOperators tCase collapseIfNull (P.getName typedParser)
  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ GS.BE.equalityOperators
                tCase
                collapseIfNull
                (IR.mkParameter <$> typedParser)
                (mkListLiteral <$> columnListParser),
              GS.BE.comparisonOperators
                tCase
                collapseIfNull
                (IR.mkParameter <$> typedParser),
              customOperators
            ]
  where
    dataConnectorName = sourceInfo ^. RQL.siConfiguration . DC.scDataConnectorName

    mkListLiteral :: [RQL.ColumnValue 'DataConnector] -> IR.UnpreparedValue 'DataConnector
    mkListLiteral columnValues =
      IR.UVLiteral $ DC.ArrayLiteral (columnTypeToScalarType columnType) (RQL.cvValue <$> columnValues)

    mkCustomOperators ::
      NamingCase ->
      Options.DangerouslyCollapseBooleans ->
      GQL.Name ->
      GS.C.SchemaT r m [P.InputFieldsParser n (Maybe (CustomBooleanOperator (IR.UnpreparedValue 'DataConnector)))]
    mkCustomOperators tCase collapseIfNull typeName = do
      let capabilities = sourceInfo ^. RQL.siConfiguration . DC.scCapabilities
      case Map.lookup (Witch.from $ DC.fromGQLType typeName) (API.unScalarTypesCapabilities $ API._cScalarTypes capabilities) of
        Nothing -> pure []
        Just API.ScalarTypeCapabilities {..} -> do
          traverse (mkCustomOperator tCase collapseIfNull) $ Map.toList $ fmap Witch.from $ API.unComparisonOperators $ _stcComparisonOperators

    mkCustomOperator ::
      NamingCase ->
      Options.DangerouslyCollapseBooleans ->
      (GQL.Name, DC.ScalarType) ->
      GS.C.SchemaT r m (P.InputFieldsParser n (Maybe (CustomBooleanOperator (IR.UnpreparedValue 'DataConnector))))
    mkCustomOperator tCase collapseIfNull (operatorName, argType) = do
      argParser <- mkArgParser argType
      pure $
        GS.BE.mkBoolOperator tCase collapseIfNull (fromCustomName operatorName) Nothing $
          CustomBooleanOperator (GQL.unName operatorName) . Just . Right <$> argParser

    mkArgParser :: DC.ScalarType -> GS.C.SchemaT r m (P.Parser 'P.Both n (IR.UnpreparedValue 'DataConnector))
    mkArgParser argType =
      fmap IR.mkParameter
        <$> columnParser'
          (RQL.ColumnScalar argType)
          (GQL.Nullability True)

tableArgs' ::
  forall r m n.
  MonadBuildSchema 'DataConnector r m n =>
  RQL.SourceInfo 'DataConnector ->
  RQL.TableInfo 'DataConnector ->
  GS.C.SchemaT r m (P.InputFieldsParser n (IR.SelectArgsG 'DataConnector (IR.UnpreparedValue 'DataConnector)))
tableArgs' sourceName tableInfo = do
  whereParser <- GS.S.tableWhereArg sourceName tableInfo
  orderByParser <- GS.S.tableOrderByArg sourceName tableInfo
  let mkSelectArgs whereArg orderByArg limitArg offsetArg =
        IR.SelectArgs
          { _saWhere = whereArg,
            _saOrderBy = orderByArg,
            _saLimit = limitArg,
            _saOffset = offsetArg,
            _saDistinct = Nothing
          }
  pure $
    mkSelectArgs
      <$> whereParser
      <*> orderByParser
      <*> GS.S.tableLimitArg
      <*> GS.S.tableOffsetArg

countTypeInput' ::
  MonadParse n =>
  Maybe (P.Parser 'P.Both n DC.ColumnName) ->
  P.InputFieldsParser n (IR.CountDistinct -> DC.CountAggregate)
countTypeInput' = \case
  Just columnEnum -> mkCountAggregate <$> P.fieldOptional Name._column Nothing columnEnum
  Nothing -> pure $ mkCountAggregate Nothing
  where
    mkCountAggregate :: Maybe DC.ColumnName -> IR.CountDistinct -> DC.CountAggregate
    mkCountAggregate Nothing _ = DC.StarCount
    mkCountAggregate (Just column) IR.SelectCountDistinct = DC.ColumnDistinctCount column
    mkCountAggregate (Just column) IR.SelectCountNonDistinct = DC.ColumnCount column
