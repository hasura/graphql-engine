module Hasura.RQL.Types.Backend
  ( Backend (..),
    Representable,
    SessionVarType,
    XDisable,
    XEnable,
  )
where

import Data.Aeson.Extended
import Data.Kind (Type)
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

-- Used for extension types.
type XEnable = ()

type XDisable = Void

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
    Representable (FunctionArgType b),
    Representable (ConstraintName b),
    Representable (BasicOrderType b),
    Representable (NullsOrderType b),
    Representable (Column b),
    Representable (ScalarType b),
    Representable (SQLExpression b),
    Representable (SQLOperator b),
    Representable (SourceConnConfiguration b),
    Representable (ExtraTableMetadata b),
    Representable (XComputedField b),
    Ord (TableName b),
    Ord (FunctionName b),
    Ord (ScalarType b),
    Data (TableName b),
    Traversable (BooleanOperators b),
    FromJSON (Column b),
    FromJSON (ConstraintName b),
    FromJSON (FunctionName b),
    FromJSON (ScalarType b),
    FromJSON (TableName b),
    FromJSON (SourceConnConfiguration b),
    FromJSON (ExtraTableMetadata b),
    FromJSONKey (Column b),
    ToJSON (Column b),
    ToJSON (ConstraintName b),
    ToJSON (FunctionArgType b),
    ToJSON (FunctionName b),
    ToJSON (ScalarType b),
    ToJSON (SourceConfig b),
    ToJSON (TableName b),
    ToJSON (SourceConnConfiguration b),
    ToJSON (ExtraTableMetadata b),
    ToJSON (SQLExpression b),
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
    Typeable (TableName b),
    Typeable (ConstraintName b),
    Typeable b,
    HasTag b,
    -- Type constraints.
    Eq (CountType b),
    Show (CountType b),
    Eq (ScalarValue b),
    Show (ScalarValue b),
    -- Extension constraints.
    Eq (XNodesAgg b),
    Show (XNodesAgg b),
    Eq (XRelay b),
    Show (XRelay b),
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
  type SourceConfig b :: Type
  type SourceConnConfiguration b :: Type
  type TableName b :: Type
  type RawFunctionInfo b :: Type
  type FunctionName b :: Type
  type FunctionArgType b :: Type
  type ConstraintName b :: Type
  type BasicOrderType b :: Type
  type NullsOrderType b :: Type
  type CountType b :: Type
  type Column b :: Type
  type ScalarValue b :: Type
  type ScalarType b :: Type
  type BooleanOperators b :: Type -> Type
  type SQLExpression b :: Type
  type SQLOperator b :: Type

  type ExtraTableMetadata b :: Type

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

  -- functions on types
  functionArgScalarType :: FunctionArgType b -> ScalarType b
  isComparableType :: ScalarType b -> Bool
  isNumType :: ScalarType b -> Bool
  textToScalarValue :: Maybe Text -> ScalarValue b
  parseScalarValue :: ScalarType b -> Value -> Either QErr (ScalarValue b)
  scalarValueToJSON :: ScalarValue b -> Value
  functionToTable :: FunctionName b -> TableName b
  tableToFunction :: TableName b -> FunctionName b

  -- functions on names
  tableGraphQLName :: TableName b -> Either QErr G.Name
  functionGraphQLName :: FunctionName b -> Either QErr G.Name

  -- | This function is used in the validation of a remote relationship where
  -- we check whether the columns that are mapped to arguments of a remote
  -- field are compatible
  scalarTypeGraphQLName :: ScalarType b -> Either QErr G.Name

  -- TODO: metadata related functions
  snakeCaseTableName :: TableName b -> Text
