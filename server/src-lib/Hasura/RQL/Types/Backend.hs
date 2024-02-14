{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Backend
  ( Backend (..),
    SessionVarType,
    XDisable,
    XEnable,
    ComputedFieldReturnType (..),
    _ReturnsTable,
    SupportedNamingCase (..),
    HasSourceConfiguration (..),
    Representable,
  )
where

import Autodocodec (HasCodec (..))
import Autodocodec.DerivingVia ()
import Autodocodec.OpenAPI ()
import Control.Lens.TH (makePrisms)
import Data.Aeson.Extended
import Data.Environment qualified as Env
import Data.Kind (Type)
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Extended
import Data.Typeable (Typeable)
import Hasura.Backends.Postgres.Connection.Settings (ConnectionTemplate (..))
import Hasura.Base.Error
import Hasura.Base.ToErrorValue
import Hasura.EncJSON (EncJSON)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate (RemoteRelSessionVariableORLiteralValue, RemoteRelSupportedOp)
import Hasura.RQL.IR.ModelInformation.Types
import Hasura.RQL.Types.BackendTag
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.HealthCheckImplementation (HealthCheckImplementation)
import Hasura.RQL.Types.ResizePool (ServerReplicas, SourceResizePoolSummary)
import Hasura.RQL.Types.Session (SessionVariables)
import Hasura.RQL.Types.SourceConfiguration
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G
import Witch (From)

type SessionVarType b = CollectableType (ScalarType b)

data ComputedFieldReturnType (b :: BackendType)
  = ReturnsScalar (ScalarType b)
  | ReturnsTable (TableName b)
  | ReturnsOthers

-- Used for extension types.
type XEnable = ()

type XDisable = Void

-- | Used for keeping track of the extent of support of naming convention
--  across different backends.
--
--  @AllConventions@ implies a full support whereas @OnlyHasuraCase@ implies
--  a partial support of only @HasuraCase@
data SupportedNamingCase = OnlyHasuraCase | AllConventions

-- | Mapping from abstract types to concrete backend representation
--
-- The RQL IR, used as the output of GraphQL parsers and of the RQL parsers, is
-- backend-agnostic: it uses an abstract representation of the structure of a
-- query, and delegates to the backends the task of choosing an appropriate
-- concrete representation.
--
-- Additionally, grouping all those types under one typeclass rather than having
-- dedicated type families allows to explicitly list all typeclass requirements,
-- which simplifies the instance declarations of all IR types.
--
-- There are no injectivity requirements on those type families: it's
-- okay for two different backends to use the same types. That means,
-- however, that functions cannot identify to what backend b a given
-- @TableName b@ refers to; most generic functions will need either a
-- type application or a 'Proxy' parameter to disambiguate between
-- different backends at the call site.
class
  ( HasSourceConfiguration b,
    Representable (BasicOrderType b),
    Representable (Column b),
    Representable (ColumnPath b),
    Representable (ComputedFieldDefinition b),
    Representable (ComputedFieldImplicitArguments b),
    Representable (ComputedFieldReturn b),
    Representable (ConstraintName b),
    Representable (ExtraTableMetadata b),
    Representable (FunctionArgument b),
    Representable (FunctionName b),
    Representable (FunctionReturnType b),
    Representable (HealthCheckTest b),
    Representable (NullsOrderType b),
    Representable (SQLExpression b),
    Representable (ScalarSelectionArguments b),
    Representable (ScalarType b),
    Representable (XComputedField b),
    Representable (XGroupBy b),
    Representable (TableName b),
    Eq (RawFunctionInfo b),
    Show (RawFunctionInfo b),
    Representable (ResolvedConnectionTemplate b),
    Ord (TableName b),
    Ord (FunctionName b),
    Ord (ScalarType b),
    Ord (Column b),
    Ord (ColumnPath b),
    Ord (ComputedFieldReturn b),
    Ord (ComputedFieldImplicitArguments b),
    Ord (ConstraintName b),
    Ord (FunctionArgument b),
    Ord (XComputedField b),
    Data (TableName b),
    From (Column b) (ColumnPath b),
    FromJSON (BackendConfig b),
    FromJSON (Column b),
    FromJSON (ColumnPath b),
    FromJSON (ColumnPath b),
    FromJSON (ComputedFieldDefinition b),
    FromJSON (ConnectionTemplateRequestContext b),
    FromJSON (ConstraintName b),
    FromJSON (ExtraTableMetadata b),
    FromJSON (FunctionName b),
    FromJSON (FunctionReturnType b),
    FromJSON (HealthCheckTest b),
    FromJSON (RawFunctionInfo b),
    FromJSON (ScalarType b),
    FromJSON (TableName b),
    FromJSONKey (Column b),
    FromJSONKey (ColumnPath b),
    FromJSONKey (ConstraintName b),
    HasCodec (BackendConfig b),
    HasCodec (BackendSourceKind b),
    HasCodec (Column b),
    HasCodec (ColumnPath b),
    HasCodec (ComputedFieldDefinition b),
    HasCodec (FunctionName b),
    HasCodec (FunctionReturnType b),
    HasCodec (ScalarType b),
    HasCodec (TableName b),
    Hashable (Column b),
    Hashable (ColumnPath b),
    ToJSON (BackendConfig b),
    ToJSON (Column b),
    ToJSON (ColumnPath b),
    ToJSON (ConstraintName b),
    ToJSON (ExecutionStatistics b),
    ToJSON (FunctionArgument b),
    ToJSON (FunctionName b),
    ToJSON (FunctionReturnType b),
    ToJSON (RawFunctionInfo b),
    ToJSON (ScalarType b),
    ToJSON (TableName b),
    ToJSON (ExtraTableMetadata b),
    ToJSON (SQLExpression b),
    ToJSON (ComputedFieldDefinition b),
    ToJSON (ComputedFieldImplicitArguments b),
    ToJSON (ComputedFieldReturn b),
    ToJSON (HealthCheckTest b),
    ToJSON (ResolvedConnectionTemplate b),
    ToJSONKey (Column b),
    ToJSONKey (ColumnPath b),
    ToJSONKey (ConstraintName b),
    ToJSONKey (ScalarType b),
    ToTxt (Column b),
    ToTxt (FunctionName b),
    ToTxt (ScalarType b),
    ToTxt (TableName b),
    ToTxt (ConstraintName b),
    ToErrorValue (Column b),
    ToErrorValue (TableName b),
    Typeable (Column b),
    Typeable (ColumnPath b),
    Typeable b,
    HasTag b,
    Traversable (CountType b),
    -- constraints of function argument
    Traversable (FunctionArgumentExp b),
    -- Type constraints.
    Eq (BackendConfig b),
    Show (BackendConfig b),
    Eq (BackendInfo b),
    Show (BackendInfo b),
    Monoid (BackendInfo b),
    Eq (ScalarValue b),
    Show (ScalarValue b),
    -- Extension constraints.
    Eq (XNodesAgg b),
    Show (XNodesAgg b),
    Eq (XRelay b),
    Show (XRelay b),
    Eq (XStreamingSubscription b),
    Show (XStreamingSubscription b),
    Eq (XNestedObjects b),
    Ord (XNestedObjects b),
    Show (XNestedObjects b),
    NFData (XNestedObjects b),
    Hashable (XNestedObjects b),
    ToJSON (XNestedObjects b),
    FromJSON (XNestedObjects b),
    ToTxt (XNestedObjects b),
    -- Intermediate Representations
    Traversable (BooleanOperators b),
    Traversable (UpdateVariant b),
    Traversable (BackendInsert b),
    Traversable (AggregationPredicates b)
  ) =>
  Backend (b :: BackendType)
  where
  -- types

  -- | Backend configuration stored in metadata
  type BackendConfig b :: Type

  -- | Runtime backend info derived from (possibly enriched) BackendConfig and stored in SchemaCache
  type BackendInfo b :: Type

  -- Fully qualified name of a table
  type TableName b :: Type

  -- Fully qualified name of a function
  type FunctionName b :: Type

  type FunctionReturnType b :: Type
  type FunctionReturnType b = XDisable

  -- Information about a function obtained by introspecting the underlying
  -- database
  type RawFunctionInfo b :: Type

  -- Fully qualified name of a constraint
  type ConstraintName b :: Type

  type BasicOrderType b :: Type
  type NullsOrderType b :: Type

  -- | The type that captures how count aggregations are modelled
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type CountType b :: Type -> Type

  -- Name of a 'column'
  type Column b :: Type

  -- Path to a column
  type ColumnPath b :: Type

  type ScalarValue b :: Type
  type ScalarType b :: Type

  type SQLExpression b :: Type
  type ComputedFieldDefinition b :: Type

  -- | Arguments of a scalar field's selection
  -- {
  --   query {
  --     some_table {
  --       # a scalar field
  --       column(ScalarSelectionArguments)
  --     }
  --   }
  -- }
  type ScalarSelectionArguments b :: Type

  type ExtraTableMetadata b :: Type

  -- | FunctionArgument
  type FunctionArgument b :: Type

  -- | Function input argument expression
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type FunctionArgumentExp b :: Type -> Type

  -- | Computed field function argument values which are being implicitly inferred from table and/or session information
  type ComputedFieldImplicitArguments b :: Type

  -- | Computed field return information
  type ComputedFieldReturn b :: Type

  -- | A config type for health check tests
  type HealthCheckTest b :: Type

  -- | A backend type can opt into supporting health checks by providing an
  -- implementation that includes a default health check test, and a health
  -- check test codec.
  healthCheckImplementation :: Maybe (HealthCheckImplementation (HealthCheckTest b))
  healthCheckImplementation = Nothing

  -- | An Implementation for version checking when adding a source.
  versionCheckImplementation :: Env.Environment -> SourceName -> SourceConnConfiguration b -> IO (Either QErr ())
  versionCheckImplementation _ _ _ = pure (Right ())

  -- | A backend type can opt into providing an implementation for
  -- fingerprinted pings to the source,
  -- useful for attribution that the user is using Hasura
  runPingSource :: Env.Environment -> (String -> IO ()) -> SourceName -> SourceConnConfiguration b -> IO ()
  runPingSource _ _ _ _ = pure ()

  -- Backend-specific IR types

  -- | Intermediate Representation of extensions to the shared set of boolean
  -- operators on table fields.
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type BooleanOperators b :: Type -> Type

  -- | Intermediate Representation of aggregation predicates.
  -- The default implementation makes aggregation predicates uninstantiable.
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type AggregationPredicates b :: Type -> Type

  type AggregationPredicates b = Const Void

  -- | The different variants of update supported by a backend for their
  -- intermediate representation. For example, a backend could use a sum type
  -- encapsulating either a single batch update or multiple batch updates.
  --
  -- The default implementation makes update expressions uninstantiable.
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type UpdateVariant b :: Type -> Type

  type UpdateVariant b = Const Void

  -- | Intermediate Representation of Insert Mutations.
  -- The default implementation makes insert expressions uninstantiable.
  --
  -- It is parameterised over the type of fields, which changes during the IR
  -- translation phases.
  type BackendInsert b :: Type -> Type

  type BackendInsert b = Const Void

  -- extension types
  type XComputedField b :: Type
  type XRelay b :: Type
  type XNodesAgg b :: Type

  -- | Flag the availability of event triggers.
  type XEventTriggers b :: Type

  -- | Extension to flag the availability of object and array relationships in inserts (aka nested inserts).
  type XNestedInserts b :: Type

  type XStreamingSubscription b :: Type

  type XNestedObjects b :: Type
  type XNestedObjects b = XDisable

  type XGroupBy b :: Type
  type XGroupBy b = XDisable

  -- The result of dynamic connection template resolution
  type ResolvedConnectionTemplate b :: Type
  type ResolvedConnectionTemplate b = () -- Uninmplemented value

  -- The request context for dynamic connection template resolution. This is
  -- defined for the `<backend>_test_connection_template` metadata API
  type ConnectionTemplateRequestContext b :: Type
  type ConnectionTemplateRequestContext b = () -- Uninmplemented value

  resolveConnectionTemplate :: SourceConfig b -> ConnectionTemplateRequestContext b -> Maybe ConnectionTemplate -> Either QErr EncJSON
  resolveConnectionTemplate _ _ _ = Left (err400 (NotSupported) "connection template is not implemented")

  -- | Information about the query execution that may be useful for debugging
  -- or reporting.
  type ExecutionStatistics b :: Type

  type ExecutionStatistics b = ()

  -- functions on types
  isComparableType :: ScalarType b -> Bool
  isNumType :: ScalarType b -> Bool

  -- | Custom aggregate operators supported by the backend.
  -- Backends that support custom aggregate operators should
  -- return a HashMap from operator name to a scalar type mapping.
  -- In the scalar type mapping the key represents the input type for the operator
  -- and the value represents the result type.
  -- Backends that do not support custom aggregate operators can use the default implementation
  -- which returns an empty map.
  getCustomAggregateOperators :: SourceConfig b -> HashMap G.Name (HashMap (ScalarType b) (ScalarType b))
  getCustomAggregateOperators = const mempty

  textToScalarValue :: Maybe Text -> ScalarValue b

  parseScalarValue :: ScalarTypeParsingContext b -> ScalarType b -> Value -> Either QErr (ScalarValue b)

  scalarValueToJSON :: ScalarValue b -> Value
  functionToTable :: FunctionName b -> TableName b
  tableToFunction :: TableName b -> FunctionName b
  computedFieldFunction :: ComputedFieldDefinition b -> FunctionName b
  computedFieldReturnType :: ComputedFieldReturn b -> ComputedFieldReturnType b

  -- | Backends that don't support aggregate computed fields will never
  -- encounter an 'RQL.IR.Select.SelectionField'. However, backends are
  -- expected to provide a total transformation from 'SelectionField' to the
  -- backend's query language.
  --
  -- Rather than implement error handling for every backend that doesn't
  -- support aggregate computed fields, and then remove that error handling for
  -- each backend when we /add/ support - honestly, adding error handling would
  -- probably take longer than adding aggregate computed field support - we
  -- instead have a flag.
  --
  -- If a backend declares this flag as 'False', computed fields will not be
  -- added to the GraphQL schema. This means that backends can safely handle
  -- 'SFComputedField' with a runtime exception /as long as/ this flag is
  -- 'False'.
  --
  -- Once all backends support all aggregate computed field operations, this
  -- flag can be deleted.
  supportsAggregateComputedFields :: Bool
  supportsAggregateComputedFields = False

  -- | Build function arguments expression from computed field implicit arguments
  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments b -> [FunctionArgumentExp b v]

  -- functions on names
  tableGraphQLName :: TableName b -> Either QErr G.Name
  functionGraphQLName :: FunctionName b -> Either QErr G.Name
  getTableIdentifier :: TableName b -> Either QErr GQLNameIdentifier

  -- TODO: metadata related functions
  snakeCaseTableName :: TableName b -> Text

  -- Global naming convention
  namingConventionSupport :: SupportedNamingCase

  -- Resize source pools based on the count of server replicas and execute IO hook post resize
  resizeSourcePools :: SourceConfig b -> ServerReplicas -> IO SourceResizePoolSummary

  -- | Default behaviour of SQL triggers on logically replicated database.
  -- Setting this to @Nothing@ will disable event trigger configuration in the
  -- metadata.
  defaultTriggerOnReplication :: Maybe (XEventTriggers b, TriggerOnReplication)

  -- | Get values from a column in a table with some filters. This function is used in evaluating remote relationship
  --   predicate in permissions
  --
  -- TODO (paritosh): This function should return a JSON array of column values. We shouldn't have to parse the column
  -- values as Text. The database's JSON serialize/deserialize can take care of correct casting of values (GS-642).
  getColVals ::
    (MonadIO m, MonadError QErr m) =>
    SessionVariables ->
    SourceName ->
    SourceConfig b ->
    TableName b ->
    (ScalarType b, Column b) ->
    (Column b, [RemoteRelSupportedOp RemoteRelSessionVariableORLiteralValue]) ->
    m [Text]

  -- | Get the top-level column from a ColumnPath
  -- For backends that don't support nested objects (i.e. where ColumnPath b = Column b) this will be `id`.
  getColumnPathColumn :: ColumnPath b -> Column b

  -- | Convert a singleton ColumnPath to a Column
  -- Should return Nothing for paths to nested fields
  tryColumnPathToColumn :: ColumnPath b -> Maybe (Column b)

  backendSupportsNestedObjects :: Either QErr (XNestedObjects b)
  default backendSupportsNestedObjects :: (XNestedObjects b ~ XDisable) => Either QErr (XNestedObjects b)
  backendSupportsNestedObjects = throw400 InvalidConfiguration "Nested objects not supported"

  sourceSupportsSchemalessTables :: SourceConfig b -> Bool
  sourceSupportsSchemalessTables = const False

  getAggregationPredicatesModels :: (MonadState [ModelNameInfo] m) => SourceName -> ModelSourceType -> AggregationPredicates b a -> m ()
  getAggregationPredicatesModels _ _ _ = pure ()

-- Prisms
$(makePrisms ''ComputedFieldReturnType)
