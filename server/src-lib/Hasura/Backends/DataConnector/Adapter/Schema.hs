{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Schema () where

--------------------------------------------------------------------------------

import Control.Lens ((^.))
import Data.Aeson qualified as J
import Data.Has
import Data.List.NonEmpty qualified as NE
import Data.Text.Casing (GQLNameIdentifier, fromCustomName)
import Data.Text.Extended ((<<>))
import Data.Text.NonEmpty qualified as NET
import Hasura.Backends.DataConnector.API.V0.Capabilities (lookupComparisonInputObjectDefinition)
import Hasura.Backends.DataConnector.Adapter.Backend (CustomBooleanOperator (..))
import Hasura.Backends.DataConnector.Adapter.Types qualified as Adapter
import Hasura.Backends.DataConnector.IR.Aggregate qualified as IR.A
import Hasura.Backends.DataConnector.IR.Column qualified as IR.C
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S.V
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
  scalarSelectionArgumentsParser _ = pure Nothing
  orderByOperators = orderByOperators'
  comparisonExps = comparisonExps'

  countTypeInput = countTypeInput'
  aggregateOrderByCountType = IR.S.T.Number
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
  (MonadParse n, MonadError QErr m) =>
  RQL.ColumnType 'DataConnector ->
  GQL.Nullability ->
  GS.C.SchemaT r m (P.Parser 'P.Both n (IR.ValueWithOrigin (RQL.ColumnValue 'DataConnector)))
columnParser' columnType (GQL.Nullability isNullable) = do
  parser <- case columnType of
    RQL.ColumnScalar IR.S.T.String -> pure (J.String <$> P.string)
    RQL.ColumnScalar IR.S.T.Number -> pure (J.Number <$> P.scientific)
    RQL.ColumnScalar IR.S.T.Bool -> pure (J.Bool <$> P.boolean)
    RQL.ColumnScalar (IR.S.T.Custom name) -> do
      gqlName <-
        GQL.mkName name
          `onNothing` throw400 ValidationFailed ("The column type name " <> name <<> " is not a valid GraphQL name")
      pure $ P.jsonScalar gqlName (Just "A custom scalar type")
    RQL.ColumnEnumReference {} ->
      throw400 NotSupported "Enum column type is unsupported by the Data Connector backend"
  pure . GS.C.peelWithOrigin . fmap (RQL.ColumnValue columnType) . possiblyNullable $ parser
  where
    possiblyNullable ::
      MonadParse m =>
      P.Parser 'P.Both m J.Value ->
      P.Parser 'P.Both m J.Value
    possiblyNullable
      | isNullable = fmap (fromMaybe J.Null) . P.nullable
      | otherwise = id

orderByOperators' :: RQL.SourceInfo 'DataConnector -> NamingCase -> (GQL.Name, NonEmpty (P.Definition P.EnumValueInfo, (RQL.BasicOrderType 'DataConnector, RQL.NullsOrderType 'DataConnector)))
orderByOperators' RQL.SourceInfo {_siConfiguration} _tCase =
  let dcName = Adapter._scDataConnectorName _siConfiguration
      orderBy = fromMaybe Name._order_by $ GQL.mkName $ NET.unNonEmptyText (Adapter.unDataConnectorName dcName) <> "_order_by"
   in (orderBy,) $
        -- NOTE: NamingCase is not being used here as we don't support naming conventions for this DB
        NE.fromList
          [ ( define $$(GQL.litName "asc") "in ascending order",
              (IR.O.Ascending, ())
            ),
            ( define $$(GQL.litName "desc") "in descending order",
              (IR.O.Descending, ())
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
  let name = P.getName typedParser <> $$(GQL.litName "_Dynamic_comparison_exp")
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
    dataConnectorName = sourceInfo ^. RQL.siConfiguration . Adapter.scDataConnectorName

    mkListLiteral :: [RQL.ColumnValue 'DataConnector] -> IR.UnpreparedValue 'DataConnector
    mkListLiteral columnValues =
      IR.UVLiteral . IR.S.V.ArrayLiteral $ RQL.cvValue <$> columnValues

    mkCustomOperators ::
      NamingCase ->
      Options.DangerouslyCollapseBooleans ->
      GQL.Name ->
      GS.C.SchemaT r m [P.InputFieldsParser n (Maybe (CustomBooleanOperator (IR.UnpreparedValue 'DataConnector)))]
    mkCustomOperators tCase collapseIfNull typeName = do
      let capabilities = sourceInfo ^. RQL.siConfiguration . Adapter.scCapabilities
      case lookupComparisonInputObjectDefinition capabilities typeName of
        Nothing -> pure []
        Just GQL.InputObjectTypeDefinition {..} -> do
          traverse (mkCustomOperator tCase collapseIfNull) _iotdValueDefinitions

    mkCustomOperator ::
      NamingCase ->
      Options.DangerouslyCollapseBooleans ->
      GQL.InputValueDefinition ->
      GS.C.SchemaT r m (P.InputFieldsParser n (Maybe (CustomBooleanOperator (IR.UnpreparedValue 'DataConnector))))
    mkCustomOperator tCase collapseIfNull GQL.InputValueDefinition {..} = do
      argParser <- mkArgParser _ivdType
      pure $
        GS.BE.mkBoolOperator tCase collapseIfNull (fromCustomName _ivdName) _ivdDescription $
          CustomBooleanOperator (GQL.unName _ivdName) . Just . Right <$> argParser

    mkArgParser :: GQL.GType -> GS.C.SchemaT r m (P.Parser 'P.Both n (IR.UnpreparedValue 'DataConnector))
    mkArgParser argType =
      fmap IR.mkParameter
        <$> columnParser'
          (RQL.ColumnScalar $ IR.S.T.fromGQLType $ GQL.getBaseType argType)
          (GQL.Nullability $ GQL.isNotNull argType)

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
  Maybe (P.Parser 'P.Both n IR.C.Name) ->
  P.InputFieldsParser n (IR.CountDistinct -> IR.A.CountAggregate)
countTypeInput' = \case
  Just columnEnum -> mkCountAggregate <$> P.fieldOptional Name._column Nothing columnEnum
  Nothing -> pure $ mkCountAggregate Nothing
  where
    mkCountAggregate :: Maybe IR.C.Name -> IR.CountDistinct -> IR.A.CountAggregate
    mkCountAggregate Nothing _ = IR.A.StarCount
    mkCountAggregate (Just column) IR.SelectCountDistinct = IR.A.ColumnDistinctCount column
    mkCountAggregate (Just column) IR.SelectCountNonDistinct = IR.A.ColumnCount column
