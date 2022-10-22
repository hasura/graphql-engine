{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Backend
  ( Backend (..),
    Representable,
    SessionVarType,
    XDisable,
    XEnable,
    ComputedFieldReturnType (..),
    _ReturnsScalar,
    SupportedNamingCase (..),
  )
where

import Control.Lens.TH (makePrisms)
import Data.Aeson.Extended
import Data.Kind (Type)
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Extended
import Data.Typeable (Typeable)
import Hasura.Base.Error
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.SQL.Backend
import Hasura.SQL.Tag
import Hasura.SQL.Types
import Language.GraphQL.Draft.Syntax qualified as G

type Representable a = (Show a, Eq a, Hashable a, Cacheable a, NFData a)

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
  ( Representable (TableName b),
    Representable (FunctionName b),
    Representable (FunctionArgument b),
    Representable (ConstraintName b),
    Representable (BasicOrderType b),
    Representable (NullsOrderType b),
    Representable (Column b),
    Representable (ScalarType b),
    Representable (SQLExpression b),
    Representable (ScalarSelectionArguments b),
    Representable (SourceConnConfiguration b),
    Representable (ExtraTableMetadata b),
    Representable (XComputedField b),
    Representable (ComputedFieldDefinition b),
    Representable (ComputedFieldImplicitArguments b),
    Representable (ComputedFieldReturn b),
    Ord (TableName b),
    Ord (FunctionName b),
    Ord (ScalarType b),
    Data (TableName b),
    Traversable (BooleanOperators b),
    FromJSON (BackendConfig b),
    FromJSON (Column b),
    FromJSON (ConstraintName b),
    FromJSON (FunctionName b),
    FromJSON (ScalarType b),
    FromJSON (TableName b),
    FromJSON (SourceConnConfiguration b),
    FromJSON (ExtraTableMetadata b),
    FromJSON (ComputedFieldDefinition b),
    FromJSON (BackendSourceKind b),
    FromJSONKey (Column b),
    ToJSON (BackendConfig b),
    ToJSON (Column b),
    ToJSON (ConstraintName b),
    ToJSON (FunctionArgument b),
    ToJSON (FunctionName b),
    ToJSON (ScalarType b),
    ToJSON (SourceConfig b),
    ToJSON (TableName b),
    ToJSON (SourceConnConfiguration b),
    ToJSON (ExtraTableMetadata b),
    ToJSON (SQLExpression b),
    ToJSON (ComputedFieldDefinition b),
    ToJSON (ComputedFieldImplicitArguments b),
    ToJSON (ComputedFieldReturn b),
    ToJSONKey (Column b),
    ToJSONKey (FunctionName b),
    ToJSONKey (ScalarType b),
    ToJSONKey (TableName b),
    ToTxt (Column b),
    ToTxt (FunctionName b),
    ToTxt (ScalarType b),
    ToTxt (TableName b),
    ToTxt (ConstraintName b),
    Cacheable (SourceConfig b),
    Cacheable (BackendConfig b),
    Typeable (TableName b),
    Typeable (ConstraintName b),
    Typeable b,
    HasTag b,
    -- constraints of function argument
    Functor (FunctionArgumentExp b),
    Foldable (FunctionArgumentExp b),
    Traversable (FunctionArgumentExp b),
    -- Type constraints.
    Eq (BackendConfig b),
    Show (BackendConfig b),
    Monoid (BackendConfig b),
    Eq (CountType b),
    Show (CountType b),
    Eq (ScalarValue b),
    Show (ScalarValue b),
    -- Extension constraints.
    Eq (XNodesAgg b),
    Show (XNodesAgg b),
    Eq (XRelay b),
    Show (XRelay b),
    Eq (XStreamingSubscription b),
    Show (XStreamingSubscription b),
    -- Intermediate Representations
    Functor (BackendUpdate b),
    Foldable (BackendUpdate b),
    Traversable (BackendUpdate b),
    Functor (BackendInsert b),
    Foldable (BackendInsert b),
    Traversable (BackendInsert b)
  ) =>
  Backend (b :: BackendType)
  where
  -- types
  type BackendConfig b :: Type

  -- | User facing connection configuration for a database.
  type SourceConnConfiguration b :: Type

  -- | Internal connection configuration for a database - connection string,
  -- connection pool etc
  type SourceConfig b :: Type

  -- Fully qualified name of a table
  type TableName b :: Type

  -- Fully qualified name of a function
  type FunctionName b :: Type

  -- Information about a function obtained by introspecting the underlying
  -- database
  type RawFunctionInfo b :: Type

  -- Fully qualified name of a constraint
  type ConstraintName b :: Type

  type BasicOrderType b :: Type
  type NullsOrderType b :: Type
  type CountType b :: Type

  -- Name of a 'column'
  type Column b :: Type

  type ScalarValue b :: Type
  type ScalarType b :: Type

  type BooleanOperators b :: Type -> Type
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
  type FunctionArgumentExp b :: Type -> Type

  -- | Computed field function argument values which are being implicitly inferred from table and/or session information
  type ComputedFieldImplicitArguments b :: Type

  -- | Computed field return information
  type ComputedFieldReturn b :: Type

  -- Backend-specific IR types

  -- | Intermediate Representation of Update Mutations.
  -- The default implementation makes update expressions uninstantiable.
  type BackendUpdate b :: Type -> Type

  type BackendUpdate b = Const Void

  -- | Intermediate Representation of Insert Mutations.
  -- The default implementation makes insert expressions uninstantiable.
  type BackendInsert b :: Type -> Type

  type BackendInsert b = Const Void

  -- extension types
  type XComputedField b :: Type
  type XRelay b :: Type
  type XNodesAgg b :: Type

  -- | Extension to flag the availability of object and array relationships in inserts (aka nested inserts).
  type XNestedInserts b :: Type

  type XStreamingSubscription b :: Type

  -- functions on types
  isComparableType :: ScalarType b -> Bool
  isNumType :: ScalarType b -> Bool
  textToScalarValue :: Maybe Text -> ScalarValue b
  parseScalarValue :: ScalarType b -> Value -> Either QErr (ScalarValue b)
  scalarValueToJSON :: ScalarValue b -> Value
  functionToTable :: FunctionName b -> TableName b
  tableToFunction :: TableName b -> FunctionName b
  computedFieldFunction :: ComputedFieldDefinition b -> FunctionName b
  computedFieldReturnType :: ComputedFieldReturn b -> ComputedFieldReturnType b

  -- | Build function arguments expression from computed field implicit arguments
  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments b -> [FunctionArgumentExp b v]

  -- functions on names
  tableGraphQLName :: TableName b -> Either QErr G.Name
  functionGraphQLName :: FunctionName b -> Either QErr G.Name
  getTableIdentifier :: TableName b -> Either QErr GQLNameIdentifier

  -- | This function is used in the validation of a remote relationship where
  -- we check whether the columns that are mapped to arguments of a remote
  -- field are compatible
  scalarTypeGraphQLName :: ScalarType b -> Either QErr G.Name

  -- TODO: metadata related functions
  snakeCaseTableName :: TableName b -> Text

  -- Global naming convention
  namingConventionSupport :: SupportedNamingCase

-- Prisms
$(makePrisms ''ComputedFieldReturnType)
