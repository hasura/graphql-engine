{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Types where

import           Hasura.Prelude

import qualified Hasura.Backends.Postgres.Connection    as PG
import qualified Hasura.Backends.Postgres.SQL.DML       as PG
import qualified Hasura.Backends.Postgres.SQL.Types     as PG
import qualified Hasura.Backends.Postgres.SQL.Value     as PG
import qualified Hasura.Backends.Postgres.Types.BoolExp as PG

import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend


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
  type BooleanOperators        'Postgres = PG.BooleanOperators
  type SQLExpression           'Postgres = PG.SQLExp
  type SQLOperator             'Postgres = PG.SQLOp

  type XComputedField          'Postgres = XEnable
  type XRemoteField            'Postgres = XEnable
  type XRelay                  'Postgres = XEnable
  type XNodesAgg               'Postgres = XEnable
  type XDistinct               'Postgres = XEnable

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
