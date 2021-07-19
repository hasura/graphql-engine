{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Instances.Types
  (
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                             as J

import           Data.Kind                                              (Type)
import           Data.Typeable

import qualified Hasura.Backends.Postgres.Connection                    as PG
import qualified Hasura.Backends.Postgres.SQL.DML                       as PG
import qualified Hasura.Backends.Postgres.SQL.Types                     as PG
import qualified Hasura.Backends.Postgres.SQL.Value                     as PG
import qualified Hasura.Backends.Postgres.Types.BoolExp                 as PG
import qualified Hasura.Backends.Postgres.Types.CitusExtraTableMetadata as Citus

import           Hasura.Base.Error
import           Hasura.RQL.Types.Backend
import           Hasura.SQL.Backend
import           Hasura.SQL.Tag


--------------------------------------------------------------------------------
-- PostgresBackend

-- | This class is an implementation detail of 'Backend'.
-- Some types of 'Backend' differ across different Postgres "kinds". This
-- class alllows each "kind" to specify its own specific implementation. All
-- common code is directly part of the `Backend` instance.
class
  ( Representable (PgExtraTableMetadata pgKind)
  , J.ToJSON      (PgExtraTableMetadata pgKind)
  , J.FromJSON    (PgExtraTableMetadata pgKind)
  , Arbitrary     (PgExtraTableMetadata pgKind)
  ) => PostgresBackend (pgKind :: PostgresKind) where
  type PgExtraTableMetadata pgKind :: Type

instance PostgresBackend 'Vanilla where
  type PgExtraTableMetadata 'Vanilla = ()

instance PostgresBackend 'Citus where
  type PgExtraTableMetadata 'Citus = Citus.ExtraTableMetadata


----------------------------------------------------------------
-- Backend instance

instance
  ( HasTag   ('Postgres pgKind)
  , Typeable ('Postgres pgKind)
  , PostgresBackend pgKind
  ) => Backend ('Postgres pgKind) where
  type SourceConfig            ('Postgres pgKind) = PG.PGSourceConfig
  type SourceConnConfiguration ('Postgres pgKind) = PG.PostgresConnConfiguration
  type Identifier              ('Postgres pgKind) = PG.Identifier
  type TableName               ('Postgres pgKind) = PG.QualifiedTable
  type FunctionName            ('Postgres pgKind) = PG.QualifiedFunction
  type FunctionArgType         ('Postgres pgKind) = PG.QualifiedPGType
  type RawFunctionInfo         ('Postgres pgKind) = PG.PGRawFunctionInfo
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

  type ExtraTableMetadata      ('Postgres pgKind) = PgExtraTableMetadata pgKind

  type XComputedField          ('Postgres pgKind) = XEnable
  type XRelay                  ('Postgres pgKind) = XEnable
  type XNodesAgg               ('Postgres pgKind) = XEnable

  functionArgScalarType   = PG.mkFunctionArgScalarType
  isComparableType        = PG.isComparableType
  isNumType               = PG.isNumType
  textToScalarValue       = PG.textToScalarValue
  parseScalarValue ty val = runAesonParser (PG.parsePGValue ty) val
  scalarValueToJSON       = PG.pgScalarValueToJson
  functionToTable         = fmap (PG.TableName . PG.getFunctionTxt)
  tableToFunction         = fmap (PG.FunctionName . PG.getTableTxt)

  tableGraphQLName        = PG.qualifiedObjectToName
  functionGraphQLName     = PG.qualifiedObjectToName
  scalarTypeGraphQLName   = runExcept . PG.mkScalarTypeName

  snakeCaseTableName      = PG.snakeCaseQualifiedObject
