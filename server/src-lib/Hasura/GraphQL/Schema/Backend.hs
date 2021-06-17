{- | This module defines the BackendSchema class, that a backend must implement
   for its schema to be generated. From the top level of the schema down to its
   leaf values, every component has a matching function in the
   schema. Combinators in other modules provide a default implementation at all
   layers.

   Consider, for example, the following query, for a given table "author":

       query {
         author(where: {id: {_eq: 2}}) {
           name
         }
       }

   The chain of functions leading to a parser for this RootField will be along
   the lines of:

       > buildTableQueryFields
         > selectTable
           > tableArgs
             > tableWhere
               > boolExp
                 > comparisonExp
                   > columnParser
           > tableSelectionSet
             > fieldSelection

   Several of those steps are part of the class, meaning that a backend can
   customize part of this tree without having to reimplement all of it. For
   instance, a backend that supports a different set ot table arguments can
   choose to reimplement @tableArgs@, but can still use @tableWhere@ in its
   custom implementation.
-}

module Hasura.GraphQL.Schema.Backend where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Has
import           Language.GraphQL.Draft.Syntax (Nullability)

import qualified Hasura.RQL.IR.Select          as IR
import qualified Hasura.RQL.IR.Update          as IR

import           Hasura.Base.Error
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Common
import           Hasura.RQL.IR
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
    -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
  buildTableRelayQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> NESeq (ColumnInfo b)
    -> SelPermInfo b
    -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
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
    -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
  buildTableUpdateMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> UpdPermInfo b
    -> Maybe (SelPermInfo b)
    -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
  buildTableDeleteMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> TableName b
    -> TableInfo b
    -> G.Name
    -> DelPermInfo b
    -> Maybe (SelPermInfo b)
    -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]
  buildFunctionQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> FunctionName b
    -> FunctionInfo b
    -> TableName b
    -> SelPermInfo b
    -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
  buildFunctionRelayQueryFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> FunctionName b
    -> FunctionInfo b
    -> TableName b
    -> NESeq (ColumnInfo b)
    -> SelPermInfo b
    -> m [FieldParser n (QueryRootField UnpreparedValue UnpreparedValue)]
  buildFunctionMutationFields
    :: MonadBuildSchema b r m n
    => SourceName
    -> SourceConfig b
    -> FunctionName b
    -> FunctionInfo b
    -> TableName b
    -> SelPermInfo b
    -> m [FieldParser n (MutationRootField UnpreparedValue UnpreparedValue)]

  -- table components
  tableArguments
    :: MonadBuildSchema b r m n
    => SourceName
    -> TableInfo b
    -> SelPermInfo b
    -> m (InputFieldsParser n (IR.SelectArgsG b (UnpreparedValue b)))

  -- backend extensions
  relayExtension    :: Maybe (XRelay b)
  nodesAggExtension :: Maybe (XNodesAgg b)

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
    => TableInfo b
    -> UpdPermInfo b
    -> m (Maybe (InputFieldsParser n [(Column b, IR.UpdOpExpG (UnpreparedValue b))]))
  mkCountType :: Maybe Bool -> Maybe [Column b] -> CountType b
  aggregateOrderByCountType :: ScalarType b
  -- | Computed field parser
  computedField
    :: MonadBuildSchema b r m n
    => SourceName
    -> ComputedFieldInfo b
    -> TableName b
    -> SelPermInfo b
    -> m (Maybe (FieldParser n (AnnotatedField b)))
  -- | The 'node' root field of a Relay request.
  node
    :: MonadBuildSchema b r m n
    => m (Parser 'Output n (HashMap (TableName b) (SourceName, SourceConfig b, SelPermInfo b, PrimaryKeyColumns b, AnnotatedFields b)))

  -- SQL literals
  columnDefaultValue :: Column b -> SQLExpression b

type ComparisonExp b = OpExpG b (UnpreparedValue b)

data BackendExtension b = BackendExtension
  { backendRelay    :: Maybe (XRelay b)
  , backendNodesAgg :: Maybe (XNodesAgg b)
  }
