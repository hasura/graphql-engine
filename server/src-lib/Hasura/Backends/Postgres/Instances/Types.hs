{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Types
--
-- Defines a 'Hasura.RQL.Types.Backend.Backend' type class instance for Postgres.
module Hasura.Backends.Postgres.Instances.Types
  (
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.Kind (Type)
import Data.Typeable
import Hasura.Backends.Postgres.Connection qualified as PG
import Hasura.Backends.Postgres.SQL.DML qualified as PG
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Backends.Postgres.SQL.Value qualified as PG
import Hasura.Backends.Postgres.Types.BoolExp qualified as PG
import Hasura.Backends.Postgres.Types.CitusExtraTableMetadata qualified as Citus
import Hasura.Backends.Postgres.Types.ComputedField qualified as PG
import Hasura.Backends.Postgres.Types.Function qualified as PG
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
    PostgresBackend pgKind,
    FromJSON (BackendSourceKind ('Postgres pgKind))
  ) =>
  Backend ('Postgres pgKind)
  where
  type BackendConfig ('Postgres pgKind) = ()
  type SourceConfig ('Postgres pgKind) = PG.PGSourceConfig
  type SourceConnConfiguration ('Postgres pgKind) = PG.PostgresConnConfiguration
  type TableName ('Postgres pgKind) = PG.QualifiedTable
  type FunctionName ('Postgres pgKind) = PG.QualifiedFunction
  type FunctionArgument ('Postgres pgKind) = PG.FunctionArg
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
  type ComputedFieldDefinition ('Postgres pgKind) = PG.ComputedFieldDefinition
  type ScalarSelectionArguments ('Postgres pgKind) = PG.ColumnOp

  type FunctionArgumentExp ('Postgres pgKind) = PG.ArgumentExp
  type ComputedFieldImplicitArguments ('Postgres pgKind) = PG.ComputedFieldImplicitArguments
  type ComputedFieldReturn ('Postgres pgKind) = PG.ComputedFieldReturn

  type BackendUpdate ('Postgres pgKind) = PG.BackendUpdate pgKind

  type ExtraTableMetadata ('Postgres pgKind) = PgExtraTableMetadata pgKind
  type BackendInsert ('Postgres pgKind) = PG.BackendInsert pgKind

  type XComputedField ('Postgres pgKind) = XEnable
  type XRelay ('Postgres pgKind) = XEnable
  type XNodesAgg ('Postgres pgKind) = XEnable
  type XNestedInserts ('Postgres pgKind) = XEnable
  type XStreamingSubscription ('Postgres pgKind) = XEnable

  isComparableType = PG.isComparableType
  isNumType = PG.isNumType
  textToScalarValue = PG.textToScalarValue
  parseScalarValue ty val = runAesonParser (PG.parsePGValue ty) val
  scalarValueToJSON = PG.pgScalarValueToJson
  functionToTable = fmap (PG.TableName . PG.getFunctionTxt)
  tableToFunction = fmap (PG.FunctionName . PG.getTableTxt)
  computedFieldFunction = PG._cfdFunction
  computedFieldReturnType = \case
    PG.CFRScalar scalarType -> ReturnsScalar scalarType
    PG.CFRSetofTable table -> ReturnsTable table
  fromComputedFieldImplicitArguments = PG.fromComputedFieldImplicitArguments

  tableGraphQLName = PG.qualifiedObjectToName
  functionGraphQLName = PG.qualifiedObjectToName

  snakeCaseTableName = PG.snakeCaseQualifiedObject
  getTableIdentifier = PG.getIdentifierQualifiedObject
  namingConventionSupport = PG.namingConventionSupport
