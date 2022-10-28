{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Types
--
-- Defines a 'Hasura.RQL.Types.Backend.Backend' type class instance for Postgres.
module Hasura.Backends.Postgres.Instances.Types
  (
  )
where

import Autodocodec (HasCodec (codec))
import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.Kind (Type)
import Data.Typeable
import Hasura.Backends.Postgres.Connection qualified as Postgres
import Hasura.Backends.Postgres.SQL.DML qualified as Postgres
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Backends.Postgres.SQL.Value qualified as Postgres
import Hasura.Backends.Postgres.Types.BoolExp qualified as Postgres
import Hasura.Backends.Postgres.Types.CitusExtraTableMetadata qualified as Citus
import Hasura.Backends.Postgres.Types.ComputedField qualified as Postgres
import Hasura.Backends.Postgres.Types.Function qualified as Postgres
import Hasura.Backends.Postgres.Types.Insert qualified as Postgres (BackendInsert)
import Hasura.Backends.Postgres.Types.Update qualified as Postgres
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp.AggregationPredicates qualified as Agg
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.HealthCheck
import Hasura.RQL.Types.HealthCheckImplementation (HealthCheckImplementation (..))
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

instance PostgresBackend 'Cockroach where
  type PgExtraTableMetadata 'Cockroach = ()

----------------------------------------------------------------
-- Backend instance

instance
  ( HasTag ('Postgres pgKind),
    Typeable ('Postgres pgKind),
    PostgresBackend pgKind,
    FromJSON (BackendSourceKind ('Postgres pgKind)),
    HasCodec (BackendSourceKind ('Postgres pgKind))
  ) =>
  Backend ('Postgres pgKind)
  where
  type BackendConfig ('Postgres pgKind) = ()
  type BackendInfo ('Postgres pgKind) = ()
  type SourceConfig ('Postgres pgKind) = Postgres.PGSourceConfig
  type SourceConnConfiguration ('Postgres pgKind) = Postgres.PostgresConnConfiguration
  type TableName ('Postgres pgKind) = Postgres.QualifiedTable
  type FunctionName ('Postgres pgKind) = Postgres.QualifiedFunction
  type FunctionArgument ('Postgres pgKind) = Postgres.FunctionArg
  type RawFunctionInfo ('Postgres pgKind) = Postgres.PGRawFunctionInfo
  type ConstraintName ('Postgres pgKind) = Postgres.ConstraintName
  type BasicOrderType ('Postgres pgKind) = Postgres.OrderType
  type NullsOrderType ('Postgres pgKind) = Postgres.NullsOrder
  type CountType ('Postgres pgKind) = Postgres.CountType
  type Column ('Postgres pgKind) = Postgres.PGCol
  type ScalarValue ('Postgres pgKind) = Postgres.PGScalarValue
  type ScalarType ('Postgres pgKind) = Postgres.PGScalarType
  type BooleanOperators ('Postgres pgKind) = Postgres.BooleanOperators
  type SQLExpression ('Postgres pgKind) = Postgres.SQLExp
  type ComputedFieldDefinition ('Postgres pgKind) = Postgres.ComputedFieldDefinition
  type ScalarSelectionArguments ('Postgres pgKind) = Postgres.ColumnOp

  type FunctionArgumentExp ('Postgres pgKind) = Postgres.ArgumentExp
  type ComputedFieldImplicitArguments ('Postgres pgKind) = Postgres.ComputedFieldImplicitArguments
  type ComputedFieldReturn ('Postgres pgKind) = Postgres.ComputedFieldReturn

  type BackendUpdate ('Postgres pgKind) = Postgres.BackendUpdate pgKind

  type AggregationPredicates ('Postgres pgKind) = Agg.AggregationPredicatesImplementation ('Postgres pgKind)

  type ExtraTableMetadata ('Postgres pgKind) = PgExtraTableMetadata pgKind
  type BackendInsert ('Postgres pgKind) = Postgres.BackendInsert pgKind

  type XComputedField ('Postgres pgKind) = XEnable
  type XRelay ('Postgres pgKind) = XEnable
  type XNodesAgg ('Postgres pgKind) = XEnable
  type XNestedInserts ('Postgres pgKind) = XEnable
  type XStreamingSubscription ('Postgres pgKind) = XEnable

  type HealthCheckTest ('Postgres pgKind) = HealthCheckTestSql
  healthCheckImplementation =
    Just $
      HealthCheckImplementation
        { _hciDefaultTest = defaultHealthCheckTestSql,
          _hciTestCodec = codec
        }

  isComparableType = Postgres.isComparableType
  isNumType = Postgres.isNumType
  textToScalarValue = Postgres.textToScalarValue
  parseScalarValue ty val = runAesonParser (Postgres.parsePGValue ty) val
  scalarValueToJSON = Postgres.pgScalarValueToJson
  functionToTable = fmap (Postgres.TableName . Postgres.getFunctionTxt)
  tableToFunction = fmap (Postgres.FunctionName . Postgres.getTableTxt)
  computedFieldFunction = Postgres._cfdFunction
  computedFieldReturnType = \case
    Postgres.CFRScalar scalarType -> ReturnsScalar scalarType
    Postgres.CFRSetofTable table -> ReturnsTable table
  fromComputedFieldImplicitArguments = Postgres.fromComputedFieldImplicitArguments

  tableGraphQLName = Postgres.qualifiedObjectToName
  functionGraphQLName = Postgres.qualifiedObjectToName

  snakeCaseTableName = Postgres.snakeCaseQualifiedObject
  getTableIdentifier = Postgres.getIdentifierQualifiedObject
  namingConventionSupport = Postgres.namingConventionSupport

  resizeSourcePools sourceConfig = Postgres._pecResizePools (Postgres._pscExecCtx sourceConfig)
