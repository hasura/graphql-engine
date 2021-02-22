{-# LANGUAGE AllowAmbiguousTypes #-}
module Hasura.RQL.Types.Backend where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax       as G

import           Data.Aeson
import           Data.Kind                           (Type)
import           Data.Text.Extended
import           Data.Typeable

import qualified Hasura.Backends.Postgres.Connection as PG
import qualified Hasura.Backends.Postgres.SQL.DML    as PG
import qualified Hasura.Backends.Postgres.SQL.Types  as PG
import qualified Hasura.Backends.Postgres.SQL.Value  as PG

import           Hasura.Incremental                  (Cacheable)
import           Hasura.RQL.DDL.Headers              ()
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

type Representable a = (Show a, Eq a, Hashable a, Cacheable a, NFData a, Typeable a)

type SessionVarType b = CollectableType (ScalarType b)

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
  , Representable (XAILIKE b)
  , Representable (XANILIKE b)
  , Representable (XRelay b)
  , Representable (XNodesAgg b)
  , Representable (XRemoteField b)
  , Representable (XComputedField b)
  , Representable (XEventTrigger b)
  , Representable (XDistinct b)
  , Generic (Column b)
  , Ord (TableName b)
  , Ord (FunctionName b)
  , Ord (ScalarType b)
  , Ord (XRelay b)
  , Data (TableName b)
  , Data (ScalarType b)
  , Data (SQLExpression b)
  , Typeable (SourceConfig b)
  , Typeable b
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
  type SQLExpression           b :: Type
  type SQLOperator             b :: Type
  type XAILIKE                 b :: Type
  type XANILIKE                b :: Type
  type XComputedField          b :: Type
  type XRemoteField            b :: Type
  type XEventTrigger           b :: Type
  type XRelay                  b :: Type
  type XNodesAgg               b :: Type
  type XDistinct               b :: Type

  -- functions on types
  backendTag :: BackendTag b
  functionArgScalarType :: FunctionArgType b -> ScalarType b
  isComparableType      :: ScalarType b -> Bool
  isNumType             :: ScalarType b -> Bool
  textToScalarType      :: Text -> ScalarType b
  textToScalarValue     :: Maybe Text -> ScalarValue b
  parseScalarValue      :: ScalarType b -> Value -> Either QErr (ScalarValue b)
  scalarValueToJSON     :: ScalarValue b -> Value
  functionToTable       :: FunctionName b -> TableName b

  -- functions on names
  tableGraphQLName    :: TableName b    -> Either QErr G.Name
  functionGraphQLName :: FunctionName b -> Either QErr G.Name
  scalarTypeGraphQLName :: ScalarType b -> Either QErr G.Name

  -- TODO: metadata related functions
  snakeCaseTableName :: TableName b -> Text

instance Backend 'Postgres where
  type SourceConfig            'Postgres = PG.PGSourceConfig
  type SourceConnConfiguration 'Postgres = PG.PostgresConnConfiguration
  type Identifier              'Postgres = PG.Identifier
  type Alias                   'Postgres = PG.Alias
  type TableName               'Postgres = PG.QualifiedTable
  type FunctionName            'Postgres = PG.QualifiedFunction
  type FunctionArgType         'Postgres = PG.QualifiedPGType
  type ConstraintName          'Postgres = PG.ConstraintName
  type BasicOrderType          'Postgres = PG.OrderType
  type NullsOrderType          'Postgres = PG.NullsOrder
  type CountType               'Postgres = PG.CountType
  type Column                  'Postgres = PG.PGCol
  type ScalarValue             'Postgres = PG.PGScalarValue
  type ScalarType              'Postgres = PG.PGScalarType
  type SQLExpression           'Postgres = PG.SQLExp
  type SQLOperator             'Postgres = PG.SQLOp
  type XAILIKE                 'Postgres = ()
  type XANILIKE                'Postgres = ()
  type XComputedField          'Postgres = ()
  type XRemoteField            'Postgres = ()
  type XEventTrigger           'Postgres = ()
  type XRelay                  'Postgres = ()
  type XNodesAgg               'Postgres = ()
  type XDistinct               'Postgres = ()

  backendTag                    = PostgresTag
  functionArgScalarType         = PG._qptName
  isComparableType              = PG.isComparableType
  isNumType                     = PG.isNumType
  textToScalarType              = PG.textToPGScalarType
  textToScalarValue             = maybe (PG.PGNull PG.PGText) PG.PGValText
  parseScalarValue ty val       = runAesonParser (PG.parsePGValue ty) val
  scalarValueToJSON             = PG.pgScalarValueToJson
  functionToTable               = fmap (PG.TableName . PG.getFunctionTxt)

  tableGraphQLName              = PG.qualifiedObjectToName
  functionGraphQLName           = PG.qualifiedObjectToName
  scalarTypeGraphQLName         = runExcept . mkScalarTypeName

  snakeCaseTableName            = PG.snakeCaseQualifiedObject
