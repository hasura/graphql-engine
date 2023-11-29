{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | MSSQL Instances Types
--
-- Defines a 'Hasura.RQL.Types.Backend.Backend' type class instance for MSSQL.
module Hasura.Backends.MSSQL.Instances.Types () where

import Autodocodec (codec)
import Data.Aeson
import Data.Text.Casing (GQLNameIdentifier)
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Backends.MSSQL.Connection qualified as MSSQL
import Hasura.Backends.MSSQL.ToQuery ()
import Hasura.Backends.MSSQL.Types.Insert qualified as MSSQL (BackendInsert)
import Hasura.Backends.MSSQL.Types.Internal qualified as MSSQL
import Hasura.Backends.MSSQL.Types.Update qualified as MSSQL (UpdateOperator)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.IR.Update.Batch (UpdateBatch)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common (TriggerOnReplication (..))
import Hasura.RQL.Types.HealthCheck
import Hasura.RQL.Types.HealthCheckImplementation (HealthCheckImplementation (..))
import Hasura.RQL.Types.ResizePool (ServerReplicas, SourceResizePoolSummary (..))
import Language.GraphQL.Draft.Syntax qualified as G

instance Backend 'MSSQL where
  type BackendConfig 'MSSQL = ()
  type BackendInfo 'MSSQL = ()
  type TableName 'MSSQL = MSSQL.TableName
  type RawFunctionInfo 'MSSQL = Void

  -- It's something of a wart that we have to
  -- specify this here, as we don't support functions for MSSQL.
  type FunctionName 'MSSQL = MSSQL.FunctionName
  type FunctionArgument 'MSSQL = Void
  type ConstraintName 'MSSQL = MSSQL.ConstraintName
  type BasicOrderType 'MSSQL = MSSQL.Order
  type NullsOrderType 'MSSQL = MSSQL.NullsOrder
  type CountType 'MSSQL = MSSQL.CountType
  type Column 'MSSQL = MSSQL.ColumnName
  type ColumnPath 'MSSQL = MSSQL.ColumnName
  type ScalarValue 'MSSQL = MSSQL.Value
  type ScalarType 'MSSQL = MSSQL.ScalarType
  type BooleanOperators 'MSSQL = MSSQL.BooleanOperators
  type SQLExpression 'MSSQL = MSSQL.Expression
  type ScalarSelectionArguments 'MSSQL = Void

  type ComputedFieldDefinition 'MSSQL = Void
  type FunctionArgumentExp 'MSSQL = Const Void
  type ComputedFieldImplicitArguments 'MSSQL = Void
  type ComputedFieldReturn 'MSSQL = Void

  type ExtraTableMetadata 'MSSQL = [MSSQL.ColumnName] -- List of identity columns
  type BackendInsert 'MSSQL = MSSQL.BackendInsert
  type UpdateVariant 'MSSQL = UpdateBatch 'MSSQL MSSQL.UpdateOperator

  type XComputedField 'MSSQL = XDisable
  type XRelay 'MSSQL = XDisable
  type XNodesAgg 'MSSQL = XEnable
  type XEventTriggers 'MSSQL = XEnable
  type XNestedInserts 'MSSQL = XDisable
  type XStreamingSubscription 'MSSQL = XDisable

  type HealthCheckTest 'MSSQL = HealthCheckTestSql
  healthCheckImplementation =
    Just
      $ HealthCheckImplementation
        { _hciDefaultTest = defaultHealthCheckTestSql,
          _hciTestCodec = codec
        }

  isComparableType :: ScalarType 'MSSQL -> Bool
  isComparableType = MSSQL.isComparableType

  isNumType :: ScalarType 'MSSQL -> Bool
  isNumType = MSSQL.isNumType

  textToScalarValue :: Maybe Text -> ScalarValue 'MSSQL
  textToScalarValue = maybe ODBC.NullValue ODBC.TextValue

  parseScalarValue :: ScalarTypeParsingContext 'MSSQL -> ScalarType 'MSSQL -> Value -> Either QErr (ScalarValue 'MSSQL)
  parseScalarValue = const MSSQL.parseScalarValue

  -- TODO: Is this Postgres specific? Should it be removed from the class?
  scalarValueToJSON :: ScalarValue 'MSSQL -> Value
  scalarValueToJSON = error "Unexpected MSSQL error: calling scalarValueToJSON. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  functionToTable :: FunctionName 'MSSQL -> TableName 'MSSQL
  functionToTable = error "Unexpected MSSQL error: calling functionToTable. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  tableToFunction :: TableName 'MSSQL -> FunctionName 'MSSQL
  tableToFunction tn = MSSQL.FunctionName (MSSQL.tableName tn) (MSSQL.tableSchema tn)

  tableGraphQLName :: TableName 'MSSQL -> Either QErr G.Name
  tableGraphQLName = MSSQL.getGQLTableName

  functionGraphQLName :: FunctionName 'MSSQL -> Either QErr G.Name
  functionGraphQLName = error "Unexpected MSSQL error: calling functionGraphQLName. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  snakeCaseTableName :: TableName 'MSSQL -> Text
  snakeCaseTableName tn = MSSQL.snakeCaseName (MSSQL.tableName tn) (MSSQL.tableSchema tn)

  getTableIdentifier :: TableName 'MSSQL -> Either QErr GQLNameIdentifier
  getTableIdentifier = MSSQL.getTableIdentifier

  namingConventionSupport :: SupportedNamingCase
  namingConventionSupport = OnlyHasuraCase

  computedFieldFunction :: ComputedFieldDefinition 'MSSQL -> FunctionName 'MSSQL
  computedFieldFunction = absurd

  computedFieldReturnType :: ComputedFieldReturn 'MSSQL -> ComputedFieldReturnType 'MSSQL
  computedFieldReturnType = absurd

  fromComputedFieldImplicitArguments :: v -> ComputedFieldImplicitArguments 'MSSQL -> [FunctionArgumentExp 'MSSQL v]
  fromComputedFieldImplicitArguments _ = absurd

  resizeSourcePools :: SourceConfig 'MSSQL -> ServerReplicas -> IO SourceResizePoolSummary
  resizeSourcePools sourceConfig = MSSQL.mssqlResizePools (MSSQL._mscExecCtx sourceConfig)

  defaultTriggerOnReplication = Just ((), TOREnableTrigger)

  getColVals _ _ _ _ _ _ = throw500 "getColVals: not implemented for the MSSQL backend"

  getColumnPathColumn = id

  tryColumnPathToColumn = Just

instance HasSourceConfiguration 'MSSQL where
  type SourceConfig 'MSSQL = MSSQL.MSSQLSourceConfig
  type SourceConnConfiguration 'MSSQL = MSSQL.MSSQLConnConfiguration
  sourceConfigNumReadReplicas = MSSQL._mscReadReplicas
  sourceConfigConnectonTemplateEnabled = const False -- not supported
  sourceSupportsColumnRedaction = const True
  sourceConfigBackendSourceKind _sourceConfig = MSSQLKind
