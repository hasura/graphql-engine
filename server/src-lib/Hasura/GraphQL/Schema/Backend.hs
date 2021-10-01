-- | This module defines the BackendSchema class, that a backend must implement
--   for its schema to be generated. From the top level of the schema down to its
--   leaf values, every component has a matching function in the
--   schema. Combinators in other modules provide a default implementation at all
--   layers.
--
--   Consider, for example, the following query, for a given table "author":
--
--       query {
--         author(where: {id: {_eq: 2}}) {
--           name
--         }
--       }
--
--   The chain of functions leading to a parser for this RootField will be along
--   the lines of:
--
--       > buildTableQueryFields
--         > selectTable
--           > tableArgs
--             > tableWhere
--               > boolExp
--                 > comparisonExp
--                   > columnParser
--           > tableSelectionSet
--             > fieldSelection
--
--   Several of those steps are part of the class, meaning that a backend can
--   customize part of this tree without having to reimplement all of it. For
--   instance, a backend that supports a different set ot table arguments can
--   choose to reimplement @tableArgs@, but can still use @tableWhere@ in its
--   custom implementation.
module Hasura.GraphQL.Schema.Backend where

import Data.Has
import Hasura.Base.Error
import Hasura.GraphQL.Parser hiding (Type)
import Hasura.GraphQL.Schema.Common
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Update qualified as IR
import Hasura.RQL.Types hiding (EnumValueInfo)
import Language.GraphQL.Draft.Syntax (Nullability)
import Language.GraphQL.Draft.Syntax qualified as G

-- TODO: it might make sense to add those constraints to MonadSchema directly?
type MonadBuildSchema b r m n =
  ( Backend b,
    BackendSchema b,
    MonadError QErr m,
    MonadSchema n m,
    MonadTableInfo r m,
    MonadRole r m,
    Has QueryContext r
  )

class Backend b => BackendSchema (b :: BackendType) where
  -- top level parsers
  buildTableQueryFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    TableName b ->
    TableInfo b ->
    G.Name ->
    SelPermInfo b ->
    m [FieldParser n (QueryRootField UnpreparedValue)]
  buildTableRelayQueryFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    TableName b ->
    TableInfo b ->
    G.Name ->
    NESeq (ColumnInfo b) ->
    SelPermInfo b ->
    m [FieldParser n (QueryRootField UnpreparedValue)]
  buildTableInsertMutationFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    TableName b ->
    TableInfo b ->
    G.Name ->
    InsPermInfo b ->
    Maybe (SelPermInfo b) ->
    Maybe (UpdPermInfo b) ->
    m [FieldParser n (MutationRootField UnpreparedValue)]
  buildTableUpdateMutationFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    TableName b ->
    TableInfo b ->
    G.Name ->
    UpdPermInfo b ->
    Maybe (SelPermInfo b) ->
    m [FieldParser n (MutationRootField UnpreparedValue)]
  buildTableDeleteMutationFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    TableName b ->
    TableInfo b ->
    G.Name ->
    DelPermInfo b ->
    Maybe (SelPermInfo b) ->
    m [FieldParser n (MutationRootField UnpreparedValue)]
  buildFunctionQueryFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    FunctionName b ->
    FunctionInfo b ->
    TableName b ->
    SelPermInfo b ->
    m [FieldParser n (QueryRootField UnpreparedValue)]
  buildFunctionRelayQueryFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    FunctionName b ->
    FunctionInfo b ->
    TableName b ->
    NESeq (ColumnInfo b) ->
    SelPermInfo b ->
    m [FieldParser n (QueryRootField UnpreparedValue)]
  buildFunctionMutationFields ::
    MonadBuildSchema b r m n =>
    SourceName ->
    SourceConfig b ->
    Maybe QueryTagsConfig ->
    FunctionName b ->
    FunctionInfo b ->
    TableName b ->
    SelPermInfo b ->
    m [FieldParser n (MutationRootField UnpreparedValue)]

  -- table components
  tableArguments ::
    MonadBuildSchema b r m n =>
    SourceName ->
    TableInfo b ->
    SelPermInfo b ->
    m (InputFieldsParser n (IR.SelectArgsG b (UnpreparedValue b)))

  -- | Make a parser for relationships. Default implementaton elides
  -- relationships altogether.
  mkRelationshipParser ::
    MonadBuildSchema b r m n =>
    SourceName ->
    RelInfo b ->
    m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsert b (UnpreparedValue b)))))
  mkRelationshipParser _ _ = pure Nothing

  -- backend extensions
  relayExtension :: Maybe (XRelay b)
  nodesAggExtension :: Maybe (XNodesAgg b)

  -- individual components
  columnParser ::
    (MonadSchema n m, MonadError QErr m) =>
    ColumnType b ->
    Nullability ->
    m (Parser 'Both n (ValueWithOrigin (ColumnValue b)))

  -- | Creates a parser for the "_on_conflict" object of the given table.
  --
  -- This object is used to generate the "ON CONFLICT" SQL clause: what should be
  -- done if an insert raises a conflict? It may not always exist: it can't be
  -- created if there aren't any unique or primary keys constraints. However, if
  -- there are no columns for which the current role has update permissions, we
  -- must still accept an empty list for `update_columns`; we do this by adding a
  -- placeholder value to the enum (see 'tableUpdateColumnsEnum').
  --
  -- The default implementation elides on_conflict support.
  conflictObject ::
    MonadBuildSchema b r m n =>
    SourceName ->
    TableInfo b ->
    Maybe (SelPermInfo b) ->
    UpdPermInfo b ->
    m (Maybe (Parser 'Input n (XOnConflict b, IR.ConflictClauseP1 b (UnpreparedValue b))))
  conflictObject _ _ _ _ = pure Nothing

  -- | The "path" argument for json column fields
  jsonPathArg ::
    MonadParse n =>
    ColumnType b ->
    InputFieldsParser n (Maybe (IR.ColumnOp b))

  orderByOperators ::
    NonEmpty (Definition EnumValueInfo, (BasicOrderType b, NullsOrderType b))
  comparisonExps ::
    MonadBuildSchema b r m n =>
    ColumnType b ->
    m (Parser 'Input n [ComparisonExp b])
  updateOperators ::
    (MonadSchema n m, MonadTableInfo r m) =>
    TableInfo b ->
    UpdPermInfo b ->
    m (Maybe (InputFieldsParser n [(Column b, IR.UpdOpExpG (UnpreparedValue b))]))
  mkCountType :: Maybe Bool -> Maybe [Column b] -> CountType b
  aggregateOrderByCountType :: ScalarType b

  -- | Computed field parser
  computedField ::
    MonadBuildSchema b r m n =>
    SourceName ->
    ComputedFieldInfo b ->
    TableName b ->
    SelPermInfo b ->
    m (Maybe (FieldParser n (AnnotatedField b)))

  -- | The 'node' root field of a Relay request.
  node ::
    MonadBuildSchema b r m n =>
    m (Parser 'Output n (HashMap (TableName b) (SourceName, SourceConfig b, SelPermInfo b, PrimaryKeyColumns b, AnnotatedFields b)))

  -- SQL literals
  columnDefaultValue :: Column b -> SQLExpression b

  -- Extra insert data
  getExtraInsertData :: TableInfo b -> ExtraInsertData b

type ComparisonExp b = OpExpG b (UnpreparedValue b)

data BackendExtension b = BackendExtension
  { backendRelay :: Maybe (XRelay b),
    backendNodesAgg :: Maybe (XNodesAgg b)
  }
