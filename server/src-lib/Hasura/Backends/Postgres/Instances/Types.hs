{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.Backends.Postgres.Instances.Types
  (
  )
where

import Data.Aeson qualified as J
import Data.Kind (Type)
import Data.Typeable
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.SQL.DML qualified as PG
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.SQL.Value qualified as PG
import Hasura.Backends.Postgres.Types.BoolExp qualified as PG
import Hasura.Backends.Postgres.Types.CitusExtraTableMetadata qualified as Citus
import Hasura.Backends.Postgres.Types.Insert qualified as PG (BackendInsert)
import Hasura.Backends.Postgres.Types.Update qualified as PG
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.SQL.Backend
import Hasura.SQL.Tag

--------------------------------------------------------------------------------
-- PostgresBackend

-- | This class is an implementation detail of 'Backend'.
-- Some types of 'Backend' differ across different Postgres "kinds". This
-- class alllows each "kind" to specify its own specific implementation. All
-- common code is directly part of the `Backend` instance.
--
-- Note: Users shouldn't ever put this as a constraint. Use `Backend ('Postgres
-- pgKind)` instead.
class
  ( Representable (PgExtraTableMetadata pgKind),
    J.ToJSON (PgExtraTableMetadata pgKind),
    J.FromJSON (PgExtraTableMetadata pgKind)
  ) =>
  PostgresBackend (pgKind :: PostgresKind)
  where
  type PgExtraTableMetadata pgKind :: Type

instance PostgresBackend 'Vanilla where
  type PgExtraTableMetadata 'Vanilla = ()

instance PostgresBackend 'Citus where
  type PgExtraTableMetadata 'Citus = Citus.ExtraTableMetadata

----------------------------------------------------------------
-- Backend instance

instance
  ( HasTag ('Postgres pgKind),
    Typeable ('Postgres pgKind),
    PostgresBackend pgKind
  ) =>
  Backend ('Postgres pgKind)
  where
  type SourceConfig ('Postgres pgKind) = PG.PGSourceConfig
  type SourceConnConfiguration ('Postgres pgKind) = PG.PostgresConnConfiguration
  type TableName ('Postgres pgKind) = PG.QualifiedTable
  type FunctionName ('Postgres pgKind) = PG.QualifiedFunction
  type FunctionArgType ('Postgres pgKind) = PG.QualifiedPGType
  type RawFunctionInfo ('Postgres pgKind) = PG.PGRawFunctionInfo
  type ConstraintName ('Postgres pgKind) = PG.ConstraintName
  type BasicOrderType ('Postgres pgKind) = PG.OrderType
  type NullsOrderType ('Postgres pgKind) = PG.NullsOrder
  type CountType ('Postgres pgKind) = PG.CountType
  type Column ('Postgres pgKind) = PG.PGCol
  type ScalarValue ('Postgres pgKind) = PG.PGScalarValue
  type ScalarType ('Postgres pgKind) = PG.PGScalarType
  type BooleanOperators ('Postgres pgKind) = PG.BooleanOperators
  type SQLExpression ('Postgres pgKind) = PG.SQLExp
  type SQLOperator ('Postgres pgKind) = PG.SQLOp

  type BackendUpdate ('Postgres pgKind) = PG.BackendUpdate

  type ExtraTableMetadata ('Postgres pgKind) = PgExtraTableMetadata pgKind
  type BackendInsert ('Postgres pgKind) = PG.BackendInsert pgKind

  type XComputedField ('Postgres pgKind) = XEnable
  type XRelay ('Postgres pgKind) = XEnable
  type XNodesAgg ('Postgres pgKind) = XEnable
  type XNestedInserts ('Postgres pgKind) = XEnable

  functionArgScalarType = PG.mkFunctionArgScalarType
  isComparableType = PG.isComparableType
  isNumType = PG.isNumType
  textToScalarValue = PG.textToScalarValue
  parseScalarValue ty val = runAesonParser (PG.parsePGValue ty) val
  scalarValueToJSON = PG.pgScalarValueToJson
  functionToTable = fmap (PG.TableName . PG.getFunctionTxt)
  tableToFunction = fmap (PG.FunctionName . PG.getTableTxt)

  tableGraphQLName = PG.qualifiedObjectToName
  functionGraphQLName = PG.qualifiedObjectToName
  scalarTypeGraphQLName = runExcept . PG.mkScalarTypeName

  snakeCaseTableName = PG.snakeCaseQualifiedObject
