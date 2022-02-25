{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.DataWrapper.Adapter.Schema () where

--------------------------------------------------------------------------------

import Hasura.GraphQL.Schema.Backend (BackendSchema (..), MonadBuildSchema)
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.Prelude
import Hasura.RQL.Types qualified as RQL
import Hasura.SQL.Backend (BackendType (DataWrapper))
import Language.GraphQL.Draft.Syntax qualified as GraphQL

--------------------------------------------------------------------------------

instance BackendSchema 'DataWrapper where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields

  buildTableRelayQueryFields = experimentalBuildTableRelayQueryFields

  buildFunctionQueryFields =
    error "buildFunctionQueryFields: not implemented for GraphQL Data Wrappers."
  buildFunctionRelayQueryFields =
    error "buildFunctionRelayQueryFields: not implemented for GraphQL Data Wrappers."
  buildFunctionMutationFields =
    error "buildFunctionMutationFields: not implemented for GraphQL Data Wrappers."
  buildTableInsertMutationFields =
    error "buildTableInsertMutationFields: not implemented for GraphQL Data Wrappers."
  buildTableUpdateMutationFields =
    error "buildTableUpdateMutationFields: not implemented for GraphQL Data Wrappers."
  buildTableDeleteMutationFields =
    error "buildTableDeleteMutationFields: not implemented for GraphQL Data Wrappers."

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Nothing

  -- table arguments
  tableArguments =
    error "tableArguments: not implemented for GraphQL Data Wrappers."

  -- indivdual components
  columnParser =
    error "columnParser: not implemented for GraphQL Data Wrappers."
  jsonPathArg _ = pure Nothing
  orderByOperators =
    error "orderByOperators: not implemented for GraphQL Data Wrappers."
  comparisonExps =
    error "comparisonExps: not implemented for GraphQL Data Wrappers."

  countTypeInput =
    error "countTypeInput: not implemented for GraphQL Data Wrappers."
  aggregateOrderByCountType =
    error "aggregateOrderByCountType: not implemented for GraphQL Data Wrappers."
  computedField =
    error "computedField: not implemented for GraphQL Data Wrappers."
  node =
    error "node: not implemented for GraphQL Data Wrappers."
  columnDefaultValue =
    error "columnDefaultValue: not implemented for GraphQL Data Wrappers."

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
