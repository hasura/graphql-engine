{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Types where

import           Hasura.Prelude

import qualified Hasura.Backends.Postgres.Connection as PG
import qualified Hasura.Backends.Postgres.SQL.DML    as PG
import qualified Hasura.Backends.Postgres.SQL.Types  as PG
import qualified Hasura.Backends.Postgres.SQL.Value  as PG

import           Hasura.RQL.DDL.Headers              ()
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

  backendTag              = PostgresTag
  functionArgScalarType   = PG._qptName
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
