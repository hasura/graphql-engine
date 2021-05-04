{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Types where

import           Hasura.Prelude

import           Data.Typeable

import qualified Hasura.Backends.Postgres.Connection    as PG
import qualified Hasura.Backends.Postgres.SQL.DML       as PG
import qualified Hasura.Backends.Postgres.SQL.Types     as PG
import qualified Hasura.Backends.Postgres.SQL.Value     as PG
import qualified Hasura.Backends.Postgres.Types.BoolExp as PG

import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.SQL.Tag


instance
  ( HasTag   ('Postgres pgKind)
  , Typeable ('Postgres pgKind)
  ) => Backend ('Postgres pgKind) where
  type SourceConfig            ('Postgres pgKind) = PG.PGSourceConfig
  type SourceConnConfiguration ('Postgres pgKind) = PG.PostgresConnConfiguration
  type Identifier              ('Postgres pgKind) = PG.Identifier
  type Alias                   ('Postgres pgKind) = PG.Alias
  type TableName               ('Postgres pgKind) = PG.QualifiedTable
  type FunctionName            ('Postgres pgKind) = PG.QualifiedFunction
  type FunctionArgType         ('Postgres pgKind) = PG.QualifiedPGType
  type ConstraintName          ('Postgres pgKind) = PG.ConstraintName
  type BasicOrderType          ('Postgres pgKind) = PG.OrderType
  type NullsOrderType          ('Postgres pgKind) = PG.NullsOrder
  type CountType               ('Postgres pgKind) = PG.CountType
  type Column                  ('Postgres pgKind) = PG.PGCol
  type ScalarValue             ('Postgres pgKind) = PG.PGScalarValue
  type ScalarType              ('Postgres pgKind) = PG.PGScalarType
  type BooleanOperators        ('Postgres pgKind) = PG.BooleanOperators
  type SQLExpression           ('Postgres pgKind) = PG.SQLExp
  type SQLOperator             ('Postgres pgKind) = PG.SQLOp

  type XComputedField          ('Postgres pgKind) = XEnable
  type XRemoteField            ('Postgres pgKind) = XEnable
  type XRelay                  ('Postgres pgKind) = XEnable
  type XNodesAgg               ('Postgres pgKind) = XEnable
  type XDistinct               ('Postgres pgKind) = XEnable

  functionArgScalarType   = PG.mkFunctionArgScalarType
  isComparableType        = PG.isComparableType
  isNumType               = PG.isNumType
  textToScalarValue       = maybe (PG.PGNull PG.PGText) PG.PGValText
  parseScalarValue ty val = runAesonParser (PG.parsePGValue ty) val
  scalarValueToJSON       = PG.pgScalarValueToJson
  functionToTable         = fmap (PG.TableName . PG.getFunctionTxt)
  tableToFunction         = fmap (PG.FunctionName . PG.getTableTxt)

  tableGraphQLName        = PG.qualifiedObjectToName
  functionGraphQLName     = PG.qualifiedObjectToName
  scalarTypeGraphQLName   = runExcept . mkScalarTypeName

  snakeCaseTableName      = PG.snakeCaseQualifiedObject
