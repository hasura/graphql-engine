{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Experimental.Adapter.Schema () where

--------------------------------------------------------------------------------

import Hasura.GraphQL.Schema.Backend (BackendSchema (..), MonadBuildSchema)
import Hasura.GraphQL.Schema.Build qualified as GSB
import Hasura.Prelude
import Hasura.RQL.Types qualified as RQL
import Hasura.SQL.Backend (BackendType (Experimental))
import Language.GraphQL.Draft.Syntax qualified as GraphQL

--------------------------------------------------------------------------------

instance BackendSchema 'Experimental where
  -- top level parsers
  buildTableQueryFields = GSB.buildTableQueryFields

  buildTableRelayQueryFields = experimentalBuildTableRelayQueryFields

  buildFunctionQueryFields =
    error "buildFunctionQueryFields: Unimplemented for Experimental backend."
  buildFunctionRelayQueryFields =
    error "buildFunctionRelayQueryFields: Unimplemented for Experimental backend."
  buildFunctionMutationFields =
    error "buildFunctionMutationFields: Unimplemented for Experimental backend."
  buildTableInsertMutationFields =
    error "buildTableInsertMutationFields: Unimplemented for Experimental backend."
  buildTableUpdateMutationFields =
    error "buildTableUpdateMutationFields: Unimplemented for Experimental backend."
  buildTableDeleteMutationFields =
    error "buildTableDeleteMutationFields: Unimplemented for Experimental backend."

  -- backend extensions
  relayExtension = Nothing
  nodesAggExtension = Nothing

  -- table arguments
  tableArguments =
    error "tableArguments: Unimplemented for Experimental backend."

  -- indivdual components
  columnParser =
    error "columnParser: Unimplemented for Experimental backend."
  jsonPathArg _ = pure Nothing
  orderByOperators =
    error "orderByOperators: Unimplemented for Experimental backend."
  comparisonExps =
    error "comparisonExps: Unimplemented for Experimental backend."

  countTypeInput =
    error "countTypeInput: Unimplemented for Experimental backend."
  aggregateOrderByCountType =
    error "aggregateOrderByCountType: Unimplemented for Experimental backend."
  computedField =
    error "computedField: Unimplemented for Experimental backend."
  node =
    error "node: Unimplemented for Experimental backend."
  columnDefaultValue =
    error "columnDefaultValue: Unimplemented for Experimental backend."

--------------------------------------------------------------------------------

experimentalBuildTableRelayQueryFields ::
  MonadBuildSchema 'Experimental r m n =>
  RQL.SourceName ->
  RQL.TableName 'Experimental ->
  RQL.TableInfo 'Experimental ->
  GraphQL.Name ->
  NESeq (RQL.ColumnInfo 'Experimental) ->
  RQL.SelPermInfo 'Experimental ->
  m [a]
experimentalBuildTableRelayQueryFields _sourceName _tableName _tableInfo _gqlName _pkeyColumns _selPerms =
  pure []
