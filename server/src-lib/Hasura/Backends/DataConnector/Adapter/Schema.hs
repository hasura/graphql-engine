{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataConnector.Adapter.Schema () where

--------------------------------------------------------------------------------

import Data.Has
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended ((<<>))
import Hasura.Backends.DataConnector.IR.Expression qualified as IR.E
import Hasura.Backends.DataConnector.IR.OrderBy qualified as IR.O
import Hasura.Backends.DataConnector.IR.Scalar.Type qualified as IR.S.T
import Hasura.Backends.DataConnector.IR.Scalar.Value qualified as IR.S.V
import Hasura.Base.Error
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend (BackendSchema (..), ComparisonExp, MonadBuildSchema)
import Hasura.GraphQL.Schema.BoolExp qualified as GS.BE
import Hasura.GraphQL.Schema.Build qualified as GS.B
import Hasura.GraphQL.Schema.Common qualified as GS.C
import Hasura.GraphQL.Schema.Select qualified as GS.S
import Hasura.Prelude
import Hasura.RQL.IR.Select (SelectArgsG (..))
import Hasura.RQL.Types.Backend qualified as RQL
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.RQL.Types.Table qualified as RQL
import Hasura.SQL.Backend (BackendType (..))
import Language.GraphQL.Draft.Syntax qualified as GQL

--------------------------------------------------------------------------------

instance BackendSchema 'DataConnector where
  -- top level parsers
  buildTableQueryFields = GS.B.buildTableQueryFields

  buildTableRelayQueryFields = experimentalBuildTableRelayQueryFields

  buildFunctionQueryFields _ _ _ _ = pure []
  buildFunctionRelayQueryFields _ _ _ _ _ = pure []
  buildFunctionMutationFields _ _ _ _ = pure []
  buildTableInsertMutationFields _ _ _ _ _ = pure []
  buildTableUpdateMutationFields _ _ _ _ = pure []
  buildTableDeleteMutationFields _ _ _ _ = pure []
  buildTableStreamingSubscriptionFields _ _ _ _ = pure []

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Nothing
  streamSubscriptionExtension = Nothing

  -- table arguments
  tableArguments = tableArgs'

  -- individual components
  columnParser = columnParser'
  scalarSelectionArgumentsParser _ = pure Nothing
  orderByOperators = orderByOperators'
  comparisonExps = comparisonExps'

  countTypeInput =
    error "countTypeInput: not implemented for the Data Connector backend."
  aggregateOrderByCountType =
    error "aggregateOrderByCountType: not implemented for the Data Connector backend."
  computedField =
    error "computedField: not implemented for the Data Connector backend."
  node =
    error "node: not implemented for the Data Connector backend."

--------------------------------------------------------------------------------

experimentalBuildTableRelayQueryFields ::
  MonadBuildSchema 'DataConnector r m n =>
  RQL.SourceName ->
  RQL.TableName 'DataConnector ->
  RQL.TableInfo 'DataConnector ->
  GQL.Name ->
  NESeq (RQL.ColumnInfo 'DataConnector) ->
  m [a]
experimentalBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns =
  pure []

columnParser' ::
  (MonadSchema n m, MonadError QErr m) =>
  RQL.ColumnType 'DataConnector ->
  GQL.Nullability ->
  m (P.Parser 'P.Both n (P.ValueWithOrigin (RQL.ColumnValue 'DataConnector)))
columnParser' columnType (GQL.Nullability isNullable) = do
  parser <- case columnType of
    RQL.ColumnScalar IR.S.T.String -> pure (IR.S.V.String <$> P.string)
    RQL.ColumnScalar IR.S.T.Number -> pure (IR.S.V.Number <$> P.scientific)
    RQL.ColumnScalar IR.S.T.Bool -> pure (IR.S.V.Boolean <$> P.boolean)
    _ -> throw400 NotSupported "This column type is unsupported by the Data Connector backend"
  pure . P.peelWithOrigin . fmap (RQL.ColumnValue columnType) . possiblyNullable $ parser
  where
    possiblyNullable ::
      MonadParse m =>
      P.Parser 'P.Both m IR.S.V.Value ->
      P.Parser 'P.Both m IR.S.V.Value
    possiblyNullable
      | isNullable = fmap (fromMaybe IR.S.V.Null) . P.nullable
      | otherwise = id

orderByOperators' :: NonEmpty (P.Definition P.EnumValueInfo, (RQL.BasicOrderType 'DataConnector, RQL.NullsOrderType 'DataConnector))
orderByOperators' =
  NE.fromList
    [ ( define $$(GQL.litName "asc") "in ascending order",
        (IR.O.Ascending, ())
      ),
      ( define $$(GQL.litName "desc") "in descending order",
        (IR.O.Descending, ())
      )
    ]
  where
    define name desc = P.Definition name (Just desc) P.EnumValueInfo

comparisonExps' ::
  forall m n r.
  ( BackendSchema 'DataConnector,
    MonadSchema n m,
    MonadError QErr m,
    MonadReader r m,
    Has GS.C.QueryContext r
  ) =>
  RQL.ColumnType 'DataConnector ->
  m (P.Parser 'P.Input n [ComparisonExp 'DataConnector])
comparisonExps' = P.memoize 'comparisonExps' $ \columnType -> do
  collapseIfNull <- asks $ GS.C.qcDangerousBooleanCollapse . getter
  typedParser <- columnParser' columnType (GQL.Nullability False)
  nullableTextParser <- columnParser' (RQL.ColumnScalar IR.S.T.String) (GQL.Nullability True)
  textParser <- columnParser' (RQL.ColumnScalar IR.S.T.String) (GQL.Nullability False)
  let name = P.getName typedParser <> $$(GQL.litName "_Dynamic_comparison_exp")
      desc =
        GQL.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      textListParser = fmap P.openValueOrigin <$> P.list textParser
      columnListParser = fmap P.openValueOrigin <$> P.list typedParser
  pure $
    P.object name (Just desc) $
      fmap catMaybes $
        sequenceA $
          concat
            [ GS.BE.equalityOperators
                collapseIfNull
                (P.mkParameter <$> typedParser)
                (mkListLiteral <$> columnListParser),
              GS.BE.comparisonOperators
                collapseIfNull
                (P.mkParameter <$> typedParser)
            ]
  where
    mkListLiteral :: [RQL.ColumnValue 'DataConnector] -> P.UnpreparedValue 'DataConnector
    mkListLiteral columnValues =
      P.UVLiteral $ IR.E.Array $ mapMaybe (extractLiteral . IR.E.Literal . RQL.cvValue) columnValues

    extractLiteral :: IR.E.Expression -> Maybe IR.S.V.Value
    extractLiteral (IR.E.Literal lit) = Just lit
    extractLiteral _ = Nothing

tableArgs' ::
  forall r m n.
  MonadBuildSchema 'DataConnector r m n =>
  RQL.SourceName ->
  RQL.TableInfo 'DataConnector ->
  m (P.InputFieldsParser n (SelectArgsG 'DataConnector (P.UnpreparedValue 'DataConnector)))
tableArgs' sourceName tableInfo = do
  whereParser <- GS.S.tableWhereArg sourceName tableInfo
  orderByParser <- GS.S.tableOrderByArg sourceName tableInfo
  let mkSelectArgs whereArg orderByArg limitArg offsetArg =
        SelectArgs
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
