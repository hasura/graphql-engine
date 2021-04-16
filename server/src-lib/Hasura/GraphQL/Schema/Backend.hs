module Hasura.GraphQL.Schema.Backend where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Has
import           Language.GraphQL.Draft.Syntax (Nullability)

import qualified Hasura.RQL.IR.Select          as IR
import qualified Hasura.RQL.IR.Update          as IR

import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Common
import           Hasura.RQL.Types              hiding (EnumValueInfo)


-- TODO: it might make sense to add those constraints to MonadSchema directly?
type MonadBuildSchema b r m n =
  ( Backend b
  , BackendSchema b
  , MonadError QErr m
  , MonadSchema n m
  , MonadTableInfo r m
  , MonadRole r m
  , Has QueryContext r
  , Has (BackendExtension b) r
  )

class Backend b => BackendSchema (b :: BackendType) where
  -- top level parsers
  buildTableQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> SelPermInfo b
    -> m [FieldParser n (QueryRootField UnpreparedValue)]
  buildTableRelayQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> NESeq (ColumnInfo b)
    -> SelPermInfo b
    -> m (Maybe (FieldParser n (QueryRootField UnpreparedValue)))
  buildTableInsertMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> InsPermInfo b
    -> Maybe (SelPermInfo b)
    -> Maybe (UpdPermInfo b)
    -> m [FieldParser n (MutationRootField UnpreparedValue)]
  buildTableUpdateMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> UpdPermInfo b
    -> Maybe (SelPermInfo b)
    -> m [FieldParser n (MutationRootField UnpreparedValue)]
  buildTableDeleteMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> DelPermInfo b
    -> Maybe (SelPermInfo b)
    -> m [FieldParser n (MutationRootField UnpreparedValue)]
  buildFunctionQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> FunctionName b
    -> FunctionInfo b
    -> TableName b
    -> SelPermInfo b
    -> m [FieldParser n (QueryRootField UnpreparedValue)]
  buildFunctionRelayQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> FunctionName b
    -> FunctionInfo b
    -> TableName b
    -> NESeq (ColumnInfo b)
    -> SelPermInfo b
    -> m (Maybe (FieldParser n (QueryRootField UnpreparedValue)))
  buildFunctionMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> FunctionName b
    -> FunctionInfo b
    -> TableName b
    -> SelPermInfo b
    -> m [FieldParser n (MutationRootField UnpreparedValue)]

  -- backend extensions
  relayExtension    :: SourceConfig b -> Maybe (XRelay b)
  nodesAggExtension :: SourceConfig b -> Maybe (XNodesAgg b)

  -- individual components
  columnParser
    :: (MonadSchema n m, MonadError QErr m)
    => ColumnType b
    -> Nullability
    -> m (Parser 'Both n (Opaque (ColumnValue b)))
  -- | The "path" argument for json column fields
  jsonPathArg
    :: MonadParse n
    => ColumnType b
    -> InputFieldsParser n (Maybe (IR.ColumnOp b))
  orderByOperators
    :: NonEmpty (Definition EnumValueInfo, (BasicOrderType b, NullsOrderType b))
  comparisonExps
    :: MonadBuildSchema b r m n
    => ColumnType b
    -> m (Parser 'Input n [ComparisonExp b])
  updateOperators
    :: (MonadSchema n m, MonadTableInfo r m)
    => TableName b
    -> UpdPermInfo b
    -> m (Maybe (InputFieldsParser n [(Column b, IR.UpdOpExpG (UnpreparedValue b))]))
  -- TODO: THIS IS A TEMPORARY FIX
  -- while offset is exposed in the schema as a GraphQL Int, which
  -- is a bounded Int32, previous versions of the code used to also
  -- silently accept a string as an input for the offset as a way to
  -- support int64 values (postgres bigint)
  -- a much better way of supporting this would be to expose the
  -- offset in the code as a postgres bigint, but for now, to avoid
  -- a breaking change, we are defining a custom parser that also
  -- accepts a string
  offsetParser :: MonadParse n => Parser 'Both n (SQLExpression b)
  mkCountType :: Maybe Bool -> Maybe [Column b] -> CountType b
  aggregateOrderByCountType :: ScalarType b
  -- | Argument to distinct select on columns returned from table selection
  -- > distinct_on: [table_select_column!]
  tableDistinctOn
    :: forall m n r. (BackendSchema b, MonadSchema n m, MonadTableInfo r m, MonadRole r m)
    => TableName b
    -> SelPermInfo b
    -> m (InputFieldsParser n (Maybe (XDistinct b, NonEmpty (Column b))))
  -- | Computed field parser
  computedField
    :: MonadBuildSchema b r m n
    => ComputedFieldInfo b
    -> SelPermInfo b
    -> m (Maybe (FieldParser n (AnnotatedField b)))
  -- | The 'node' root field of a Relay request.
  node
    :: MonadBuildSchema b r m n
    => m (Parser 'Output n (HashMap (TableName b) (SourceName, SourceConfig b, SelPermInfo b, PrimaryKeyColumns b, AnnotatedFields b)))
  remoteRelationshipField
    :: MonadBuildSchema b r m n
    => RemoteFieldInfo b
    -> m (Maybe [FieldParser n (AnnotatedField b)])

  -- SQL literals
  columnDefaultValue :: Column b -> SQLExpression b

type ComparisonExp b = OpExpG b (UnpreparedValue b)

data BackendExtension b = BackendExtension
  { backendRelay    :: Maybe (XRelay b)
  , backendNodesAgg :: Maybe (XNodesAgg b)
  }
