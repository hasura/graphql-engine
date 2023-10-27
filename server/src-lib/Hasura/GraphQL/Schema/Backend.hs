-- | This module defines the type class 'BackendSchema' and auxiliary types.
--
-- 'BackendSchema' represents the part of the interface that a backend driver
-- presents to the GraphQL Engine core that is responsible for generating
-- the backend's Schema Parsers.
--
-- The Schema Parsers recognise (and reflect) the schema that a backend exposes.
--
-- The 'BackendSchema' methods are used by
-- 'Hasura.GraphQL.Schema.buildGQLContext', which is the core's entrypoint to
-- schema generation.
--
-- Many of the 'BackendSchema' methods will have default implementations that a
-- backend driver may use. These may be found (chiefly) in the modules:
--
-- * The module "Hasura.GraphQL.Schema.Build", commonly qualified @GSB@
-- * "Hasura.GraphQL.Schema.Select", commonly qualified @GSS@
-- * "Hasura.GraphQL.Schema.BoolExp"
--
-- For more information see:
--
-- * <https://github.com/hasura/graphql-engine/blob/master/server/documentation/schema.md Technical overview of Schema Generation >
-- * The type 'Hasura.GraphQL.Parser.Parser', and associated source code notes
--   in the same folder (not exposed with Haddock unfortunately)
module Hasura.GraphQL.Schema.Backend
  ( -- * Main Types
    BackendSchema (..),
    BackendTableSelectSchema (..),
    BackendLogicalModelSelectSchema (..),
    BackendNativeQuerySelectSchema (..),
    BackendUpdateOperatorsSchema (..),
    MonadBuildSchema,

    -- * Auxiliary Types
    ComparisonExp,

    -- * Note: @BackendSchema@ modelling principles
    -- $modelling
  )
where

import Data.Kind (Type)
import Data.Text.Casing (GQLNameIdentifier)
import Hasura.Function.Cache
import Hasura.GraphQL.ApolloFederation (ApolloFederationParserFunction)
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser hiding (Type)
import Hasura.LogicalModel.Cache (LogicalModelInfo)
import Hasura.NativeQuery.Cache (NativeQueryInfo)
import Hasura.Prelude
import Hasura.RQL.IR
import Hasura.RQL.IR.Insert qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column hiding (EnumValueInfo)
import Hasura.RQL.Types.Column qualified as Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization (MkRootFieldName)
import Hasura.StoredProcedure.Cache (StoredProcedureInfo)
import Language.GraphQL.Draft.Syntax qualified as G

-- | Bag of constraints available to the methods of @BackendSchema@.
--
-- Note that @BackendSchema b@ is itself part of this, so a methods may also
-- call other methods. This might seem trivial, but it can be easy to miss when
-- the functions used to implement a class instance are defined in multiple
-- modules.
type MonadBuildSchema b r m n =
  ( BackendSchema b,
    MonadBuildSourceSchema b r m n
  )

-- | This type class is responsible for generating the schema of a backend.
-- Its methods are called by the common core that orchestrates the various
-- backend drivers.
--
-- Its purpose in life is to make it convenient to express the GraphQL schema we
-- want to expose for the backends that we support. This means balancing the
-- desire to have consistency with the desire to differentiate the schema of a
-- backend.
--
-- This means that it is expected to evolve over time as we add new backends,
-- and that you have the license to change it: Whatever form it currently takes
-- only reflects status quo current implementation.
--
-- The module "Hasura.GraphQL.Schema.Build" (commonly qualified as @GSB@)
-- provides standard building blocks for implementing many methods of this
-- class. And as such, these two modules are very much expected to evolve in
-- tandem.
--
-- See <#modelling Note BackendSchema modelling principles>.
class
  (Backend b) =>
  BackendSchema (b :: BackendType)
  where
  -- top level parsers
  buildTableQueryAndSubscriptionFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    TableName b ->
    TableInfo b ->
    GQLNameIdentifier ->
    SchemaT
      r
      m
      ( [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))],
        [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))],
        Maybe (G.Name, Parser 'Output n (ApolloFederationParserFunction n))
      )
  buildTableStreamingSubscriptionFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    TableName b ->
    TableInfo b ->
    GQLNameIdentifier ->
    SchemaT r m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
  buildTableRelayQueryFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    TableName b ->
    TableInfo b ->
    GQLNameIdentifier ->
    NESeq (ColumnInfo b) ->
    SchemaT r m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]
  buildTableInsertMutationFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    Scenario ->
    TableName b ->
    TableInfo b ->
    GQLNameIdentifier ->
    SchemaT r m [FieldParser n (AnnotatedInsert b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]

  -- | This method is responsible for building the GraphQL Schema for mutations
  -- backed by @UPDATE@ statements on some table, as described in
  -- @https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html@.
  --
  -- The suggested way to implement this is using building blocks in GSB, c.f.
  -- its namesake @GSB.@'Hasura.GraphQL.Schema.Build.buildTableUpdateMutationFields'.
  buildTableUpdateMutationFields ::
    (MonadBuildSchema b r m n) =>
    Scenario ->
    -- | table info
    TableInfo b ->
    -- | field display name
    GQLNameIdentifier ->
    SchemaT r m [FieldParser n (AnnotatedUpdateG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]

  buildTableDeleteMutationFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    Scenario ->
    TableName b ->
    TableInfo b ->
    GQLNameIdentifier ->
    SchemaT r m [FieldParser n (AnnDelG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]

  buildFunctionQueryFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    FunctionName b ->
    FunctionInfo b ->
    TableName b ->
    SchemaT r m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]

  buildFunctionRelayQueryFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    FunctionName b ->
    FunctionInfo b ->
    TableName b ->
    NESeq (ColumnInfo b) ->
    SchemaT r m [FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]

  buildFunctionMutationFields ::
    (MonadBuildSchema b r m n) =>
    MkRootFieldName ->
    FunctionName b ->
    FunctionInfo b ->
    TableName b ->
    SchemaT r m [FieldParser n (MutationDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))]

  buildNativeQueryRootFields ::
    (MonadBuildSchema b r m n) =>
    NativeQueryInfo b ->
    SchemaT
      r
      m
      (Maybe (FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
  buildNativeQueryRootFields _ = pure Nothing

  buildStoredProcedureRootFields ::
    (MonadBuildSchema b r m n) =>
    StoredProcedureInfo b ->
    SchemaT
      r
      m
      (Maybe (FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
  buildStoredProcedureRootFields _ = pure Nothing

  -- | Make a parser for relationships. Default implementaton elides
  -- relationships altogether.
  mkRelationshipParser ::
    (MonadBuildSchema b r m n) =>
    RelInfo b ->
    SchemaT r m (Maybe (InputFieldsParser n (Maybe (IR.AnnotatedInsertField b (UnpreparedValue b)))))
  mkRelationshipParser _ = pure Nothing

  -- backend extensions
  relayExtension :: Maybe (XRelay b)
  nodesAggExtension :: Maybe (XNodesAgg b)
  streamSubscriptionExtension :: Maybe (XStreamingSubscription b)
  groupByExtension :: Maybe (XGroupBy b)
  groupByExtension = Nothing

  -- individual components
  columnParser ::
    (MonadBuildSchema b r m n) =>
    ColumnType b ->
    G.Nullability -> -- TODO maybe use Hasura.GraphQL.Parser.Schema.Nullability instead?
    SchemaT r m (Parser 'Both n (ValueWithOrigin (ColumnValue b)))
  enumParser ::
    (MonadBuildSchema b r m n) =>
    TableName b ->
    NonEmpty (EnumValue, Column.EnumValueInfo) ->
    Maybe G.Name ->
    G.Nullability ->
    SchemaT r m (Parser 'Both n (ScalarValue b))
  possiblyNullable ::
    (MonadParse m) =>
    ScalarType b ->
    G.Nullability ->
    Parser 'Both m (ScalarValue b) ->
    Parser 'Both m (ScalarValue b)

  -- | Parser for arguments on scalar fields in a selection set
  scalarSelectionArgumentsParser ::
    (MonadParse n) =>
    ColumnType b ->
    InputFieldsParser n (Maybe (ScalarSelectionArguments b))

  orderByOperators ::
    SourceInfo b ->
    NamingCase ->
    (G.Name, NonEmpty (Definition EnumValueInfo, (BasicOrderType b, NullsOrderType b)))

  comparisonExps ::
    (MonadBuildSchema b r m n) =>
    ColumnType b ->
    SchemaT r m (Parser 'Input n [ComparisonExp b])

  -- | The input fields parser, for "count" aggregate field, yielding a function
  -- which generates @'CountType b' from optional "distinct" field value
  countTypeInput ::
    (MonadParse n) =>
    Maybe (Parser 'Both n (Column b, AnnRedactionExpUnpreparedValue b)) ->
    InputFieldsParser n (CountDistinct -> CountType b (UnpreparedValue b))

  aggregateOrderByCountType :: ScalarType b

  -- | Computed field parser
  computedField ::
    (MonadBuildSchema b r m n) =>
    ComputedFieldInfo b ->
    TableName b ->
    TableInfo b ->
    SchemaT r m (Maybe (FieldParser n (AnnotatedField b)))

-- | The public interface for the schema of table queries exposed by a backend.
--
-- Remote Schemas and the Relay schema are the chief backend-agnostic clients of
-- this typeclass.
--
-- Some of schema building components in the "Hasura.GraphQL.Schema" namespace
-- also make use of these methods, ensuring backends expose a consistent schema
-- regardless of the mode it's referenced.
--
-- Default implementations exist for all of these in
-- 'Hasura.GraphQL.Schema.Select'.
class (Backend b) => BackendTableSelectSchema (b :: BackendType) where
  tableArguments ::
    (MonadBuildSourceSchema b r m n) =>
    TableInfo b ->
    SchemaT r m (InputFieldsParser n (IR.SelectArgsG b (UnpreparedValue b)))

  tableSelectionSet ::
    (MonadBuildSourceSchema b r m n) =>
    TableInfo b ->
    SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))

  selectTable ::
    (MonadBuildSourceSchema b r m n) =>
    -- | table info
    TableInfo b ->
    -- | field display name
    G.Name ->
    -- | field description, if any
    Maybe G.Description ->
    SchemaT r m (Maybe (FieldParser n (SelectExp b)))

  selectTableAggregate ::
    (MonadBuildSourceSchema b r m n) =>
    -- | table info
    TableInfo b ->
    -- | field display name
    G.Name ->
    -- | field description, if any
    Maybe G.Description ->
    SchemaT r m (Maybe (FieldParser n (AggSelectExp b)))

type ComparisonExp b = OpExpG b (UnpreparedValue b)

class (Backend b) => BackendLogicalModelSelectSchema (b :: BackendType) where
  logicalModelArguments ::
    (MonadBuildSourceSchema b r m n) =>
    LogicalModelInfo b ->
    SchemaT r m (InputFieldsParser n (IR.SelectArgsG b (UnpreparedValue b)))

  logicalModelSelectionSet ::
    (MonadBuildSourceSchema b r m n) =>
    LogicalModelInfo b ->
    SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))

class (BackendLogicalModelSelectSchema b) => BackendNativeQuerySelectSchema (b :: BackendType) where
  selectNativeQuery ::
    (MonadBuildSourceSchema b r m n) =>
    NativeQueryInfo b ->
    G.Name ->
    Nullable ->
    Maybe G.Description ->
    SchemaT r m (Maybe (FieldParser n (AnnSimpleSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
  selectNativeQuery _ _ _ _ = pure Nothing

  selectNativeQueryObject ::
    (MonadBuildSchema b r m n) =>
    NativeQueryInfo b ->
    G.Name ->
    Maybe G.Description ->
    SchemaT
      r
      m
      (Maybe (FieldParser n (AnnObjectSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
  selectNativeQueryObject _ _ _ = pure Nothing

class (Backend b) => BackendUpdateOperatorsSchema (b :: BackendType) where
  -- | Intermediate Representation of the set of update operators that act
  -- upon table fields during an update mutation. (For example, _set and _inc)
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type UpdateOperators b :: Type -> Type

  parseUpdateOperators ::
    forall m n r.
    (MonadBuildSchema b r m n) =>
    TableInfo b ->
    UpdPermInfo b ->
    SchemaT r m (InputFieldsParser n (HashMap (Column b) (UpdateOperators b (UnpreparedValue b))))

-- $modelling
-- #modelling#
--
-- In its current form, we model every component, from the top level (query,
-- insert mutation, etc.) of the schema down to its leaf values, as a type class
-- method of @BackendSchema@.
--
-- Consider, for example, the following query for a given table "author":
--
-- > query {
-- >   author(where: {id: {_eq: 2}}) {
-- >     name
-- >   }
-- > }
--
-- The chain of functions leading to a parser for this RootField will be along
-- the lines of:
--
-- > > BackendSchema.buildTableQueryAndSubscriptionFields     (Suggested default its GSB namesake)
-- >   > GSS.selectTable
-- >     > BackendSchema.tableArguments        (Suggested default implementation being
-- >                                            GSS.defaultTableArgs)
-- >       > GSS.tableWhereArg
-- >         > GSBE.boolExp
-- >           > BackendSchema.comparisonExps
-- >             > BackendSchema.columnParser
-- >
-- >     > tableSelectionSet     (...)
-- >       > fieldSelection
--
-- (where the abbreviation @GSB@ refers to "Hasura.GraphQL.Schema.Build" and @GSS@
-- refers to "Hasura.GraphQL.Schema.Select", and @GSBE@ refers to
-- "Hasura.GraphQL.Schema.BoolExp".)
--
-- Several of those steps are part of the class, meaning that a backend can
-- customize part of this tree without having to reimplement all of it. For
-- instance, a backend that supports a different set ot table arguments can
-- choose to reimplement 'tableArguments', but can still use
-- 'Hasura.GraphQL.Schema.Select.tableWhereArg' in its custom implementation.
--
-- Applying the above modelling guidelines has pros and cons:
--
-- * Pro: You can specify both shared and diverging behavior.
-- * Pro: You can specify a lot of behavior implicitly, i.e. it's easy to write.
-- * Con: You can specify a lot of behavior implicitly, i.e. it's hard to
--   understand without tracing through implementations.
-- * Con: You get a proliferation of type class methods and it's difficult to
--   understand how they fit together.
--
-- == Going forward, we want to follow some different modelling guidelines:
--
-- We should break up / refactor the building blocks (in
-- "Hasura.GraphQL.Schema.Build" etc.) which are used to implement the top-level
-- type class methods (e.g. @BackendSchema@.'buildTableQueryAndSubscriptionFields', c.f.
-- @GSB.@'Hasura.GraphQL.Schema.Build.buildTableQueryAndSubscriptionFields', etc.) and have them
-- invoke the backend-specific behaviors they rely on via /function arguments/
-- instead of other type class methods.
--
-- When we do this, the function call sites (which will often be in @instance
-- BackendSchema ...@) become the centralised places where we decide which behavior
-- variation to follow.
--
-- When faced with answering the question of "what does this method do, and how does
-- it do it?", at least you will have listed the other components it depends on
-- front and center without having to trace through its implementation.
--
-- That is of course, if we refactor our building blocks mindfully into
-- conceptually meaningful units. Otherwise we'll just end up with an
-- incomprehensible mass of poorly shaped pieces. And we will still have a hard
-- time explaining what they do.
--
-- In other words, It is still the case that if you don't clean your room
-- you'll be living in a mess.
