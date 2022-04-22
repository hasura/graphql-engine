{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Schema () where

--------------------------------------------------------------------------------

import Data.List.NonEmpty qualified as NE
import Data.Text.Extended ((<<>))
import Hasura.Backends.DataWrapper.IR.OrderBy qualified as IR
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as Scalar.Type (Type (..))
import Hasura.Backends.DataWrapper.IR.Scalar.Value qualified as Scalar.Value (Value (..))
import Hasura.Base.Error
import Hasura.GraphQL.Parser (Definition (..), Kind (..), Parser, ValueWithOrigin)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Schema.Backend (BackendSchema (..), ComparisonExp, MonadBuildSchema)
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.GraphQL.Schema.Select qualified as GSS
import Hasura.Prelude
import Hasura.RQL.IR.Select (SelectArgsG (..))
import Hasura.RQL.Types qualified as RQL
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax qualified as GraphQL

--------------------------------------------------------------------------------

instance BackendSchema 'DataWrapper where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields

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
  jsonPathArg _ = pure Nothing
  orderByOperators = orderByOperators'
  comparisonExps = comparisonExps'

  countTypeInput =
    error "countTypeInput: not implemented for GraphQL Data Wrappers."
  aggregateOrderByCountType =
    error "aggregateOrderByCountType: not implemented for GraphQL Data Wrappers."
  computedField =
    error "computedField: not implemented for GraphQL Data Wrappers."
  node =
    error "node: not implemented for GraphQL Data Wrappers."

--------------------------------------------------------------------------------

experimentalBuildTableRelayQueryFields ::
  MonadBuildSchema 'DataWrapper r m n =>
  RQL.SourceName ->
  RQL.TableName 'DataWrapper ->
  RQL.TableInfo 'DataWrapper ->
  GraphQL.Name ->
  NESeq (RQL.ColumnInfo 'DataWrapper) ->
  m [a]
experimentalBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns =
  pure []

columnParser' ::
  (MonadSchema n m, MonadError QErr m) =>
  RQL.ColumnType 'DataWrapper ->
  G.Nullability ->
  m (Parser 'Both n (ValueWithOrigin (RQL.ColumnValue 'DataWrapper)))
columnParser' columnType (G.Nullability isNullable) = do
  parser <- case columnType of
    RQL.ColumnScalar Scalar.Type.String -> pure (Scalar.Value.String <$> P.string)
    RQL.ColumnScalar Scalar.Type.Number -> pure (Scalar.Value.Number <$> P.scientific)
    RQL.ColumnScalar Scalar.Type.Bool -> pure (Scalar.Value.Boolean <$> P.boolean)
    _ -> throw400 NotSupported "This column type is unsupported by the dynamic backend"
  pure . P.peelWithOrigin . fmap (RQL.ColumnValue columnType) . possiblyNullable $ parser
  where
    possiblyNullable ::
      MonadParse m =>
      Parser 'Both m Scalar.Value.Value ->
      Parser 'Both m Scalar.Value.Value
    possiblyNullable
      | isNullable = fmap (fromMaybe Scalar.Value.Null) . P.nullable
      | otherwise = id

orderByOperators' :: NonEmpty (Definition P.EnumValueInfo, (RQL.BasicOrderType 'DataWrapper, RQL.NullsOrderType 'DataWrapper))
orderByOperators' =
  NE.fromList
    [ ( define $$(G.litName "asc") "in ascending order",
        (IR.Ascending, ())
      ),
      ( define $$(G.litName "desc") "in descending order",
        (IR.Descending, ())
      )
    ]
  where
    define name desc = P.Definition name (Just desc) P.EnumValueInfo

comparisonExps' ::
  forall m n.
  ( BackendSchema 'DataWrapper,
    MonadSchema n m,
    MonadError QErr m
  ) =>
  RQL.ColumnType 'DataWrapper ->
  m (Parser 'Input n [ComparisonExp 'DataWrapper])
comparisonExps' = P.memoize 'comparisonExps' \columnType -> do
  typedParser <- columnParser' columnType (G.Nullability False)
  nullableTextParser <- columnParser' (RQL.ColumnScalar Scalar.Type.String) (G.Nullability True)
  textParser <- columnParser' (RQL.ColumnScalar Scalar.Type.String) (G.Nullability False)
  let name = P.getName typedParser <> $$(G.litName "_Dynamic_comparison_exp")
      desc =
        G.Description $
          "Boolean expression to compare columns of type "
            <> P.getName typedParser
            <<> ". All fields are combined with logical 'AND'."
      textListParser = fmap P.openValueOrigin <$> P.list textParser
      columnListParser = fmap P.openValueOrigin <$> P.list typedParser
  pure $
    P.object name (Just desc) $
      catMaybes
        <$> sequenceA
          [ P.fieldOptional $$(G.litName "_is_null") Nothing (bool RQL.ANISNOTNULL RQL.ANISNULL <$> P.boolean),
            P.fieldOptional $$(G.litName "_eq") Nothing (RQL.AEQ True . P.mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_neq") Nothing (RQL.ANE True . P.mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_gt") Nothing (RQL.AGT . P.mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_lt") Nothing (RQL.ALT . P.mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_gte") Nothing (RQL.AGTE . P.mkParameter <$> typedParser),
            P.fieldOptional $$(G.litName "_lte") Nothing (RQL.ALTE . P.mkParameter <$> typedParser)
          ]

tableArgs' ::
  forall r m n.
  MonadBuildSchema 'DataWrapper r m n =>
  RQL.SourceName ->
  RQL.TableInfo 'DataWrapper ->
  m (P.InputFieldsParser n (SelectArgsG 'DataWrapper (P.UnpreparedValue 'DataWrapper)))
tableArgs' sourceName tableInfo = do
  whereParser <- GSS.tableWhereArg sourceName tableInfo
  orderByParser <- GSS.tableOrderByArg sourceName tableInfo
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
      <*> GSS.tableLimitArg
      <*> GSS.tableOffsetArg
