module Hasura.RQL.Types.Backend where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Aeson.Extended
import           Data.Kind                     (Type)
import           Data.Text.Extended
import           Data.Typeable                 (Typeable)

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.DDL.Headers        ()
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.SQL.Tag
import           Hasura.SQL.Types


type Representable a = (Show a, Eq a, Hashable a, Cacheable a, NFData a, Typeable a)

type SessionVarType b = CollectableType (ScalarType b)

-- Used for extension types.
type XEnable  = ()
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
class
  ( Representable (Identifier b)
  , Representable (TableName b)
  , Representable (FunctionName b)
  , Representable (FunctionArgType b)
  , Representable (ConstraintName b)
  , Representable (BasicOrderType b)
  , Representable (NullsOrderType b)
  , Representable (Column b)
  , Representable (ScalarType b)
  , Representable (SQLExpression b)
  , Representable (SQLOperator b)
  , Representable (SessionVarType b)
  , Representable (SourceConnConfiguration b)
  , Representable (XRelay b)
  , Representable (XNodesAgg b)
  , Representable (XRemoteField b)
  , Representable (XComputedField b)
  , Representable (XDistinct b)
  , Generic (Column b)
  , Ord (TableName b)
  , Ord (FunctionName b)
  , Ord (ScalarType b)
  , Ord (XRelay b)
  , Ord (Column b)
  , Data (TableName b)
  , Data (ScalarType b)
  , Traversable (BooleanOperators b)
  , Data (SQLExpression b)
  , ToSQL (SQLExpression b)
  , FromJSON (BasicOrderType b)
  , FromJSON (Column b)
  , FromJSON (ConstraintName b)
  , FromJSON (FunctionName b)
  , FromJSON (NullsOrderType b)
  , FromJSON (ScalarType b)
  , FromJSON (TableName b)
  , FromJSON (SourceConnConfiguration b)
  , FromJSONKey (Column b)
  , ToJSON (BasicOrderType b)
  , ToJSON (Column b)
  , ToJSON (ConstraintName b)
  , ToJSON (FunctionArgType b)
  , ToJSON (FunctionName b)
  , ToJSON (NullsOrderType b)
  , ToJSON (ScalarType b)
  , ToJSON (SourceConfig b)
  , ToJSON (SourceConfig b)
  , ToJSON (TableName b)
  , ToJSON (SourceConnConfiguration b)
  , ToJSONKey (Column b)
  , ToJSONKey (FunctionName b)
  , ToJSONKey (ScalarType b)
  , ToJSONKey (TableName b)
  , ToTxt (Column b)
  , ToTxt (FunctionName b)
  , ToTxt (ScalarType b)
  , ToTxt (TableName b)
  , ToTxt (ConstraintName b)
  , Arbitrary (Column b)
  , Arbitrary (TableName b)
  , Arbitrary (FunctionName b)
  , Arbitrary (SourceConnConfiguration b)
  , Cacheable (SourceConfig b)
  , Typeable b
  , HasTag b
  ) => Backend (b :: BackendType) where
  -- types
  type SourceConfig            b = sc | sc -> b
  type SourceConnConfiguration b = scc | scc -> b
  type Identifier              b :: Type
  type Alias                   b :: Type
  type TableName               b = tn | tn -> b
  type FunctionName            b = fn | fn -> b
  type FunctionArgType         b :: Type
  type ConstraintName          b :: Type
  type BasicOrderType          b :: Type
  type NullsOrderType          b :: Type
  type CountType               b :: Type
  type Column                  b = c | c -> b
  type ScalarValue             b = sv | sv -> b
  type ScalarType              b = s | s -> b
  type BooleanOperators        b :: Type -> Type
  type SQLExpression           b :: Type
  type SQLOperator             b :: Type

  -- extension types
  type XComputedField          b :: Type
  type XRemoteField            b :: Type
  type XRelay                  b :: Type
  type XNodesAgg               b :: Type
  type XDistinct               b :: Type

  -- functions on types
  functionArgScalarType :: FunctionArgType b -> ScalarType b
  isComparableType      :: ScalarType b -> Bool
  isNumType             :: ScalarType b -> Bool
  textToScalarValue     :: Maybe Text -> ScalarValue b
  parseScalarValue      :: ScalarType b -> Value -> Either QErr (ScalarValue b)
  scalarValueToJSON     :: ScalarValue b -> Value
  functionToTable       :: FunctionName b -> TableName b
  tableToFunction       :: TableName b -> FunctionName b

  -- functions on names
  tableGraphQLName    :: TableName b    -> Either QErr G.Name
  functionGraphQLName :: FunctionName b -> Either QErr G.Name
  scalarTypeGraphQLName :: ScalarType b -> Either QErr G.Name

  -- TODO: metadata related functions
  snakeCaseTableName :: TableName b -> Text
