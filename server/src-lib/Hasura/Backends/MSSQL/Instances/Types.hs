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
import Hasura.Backends.MSSQL.Types.Update qualified as MSSQL (BackendUpdate)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common (TriggerOnReplication (..))
import Hasura.RQL.Types.HealthCheck
import Hasura.RQL.Types.HealthCheckImplementation (HealthCheckImplementation (..))
import Hasura.RQL.Types.ResizePool (ServerReplicas)
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Syntax qualified as G

instance Backend 'MSSQL where
  type BackendConfig 'MSSQL = ()
  type BackendInfo 'MSSQL = ()
  type SourceConfig 'MSSQL = MSSQL.MSSQLSourceConfig
  type SourceConnConfiguration 'MSSQL = MSSQL.MSSQLConnConfiguration
  type TableName 'MSSQL = MSSQL.TableName
  type RawFunctionInfo 'MSSQL = Void

  -- It's something of a wart that we have to
  -- specify this here, as we don't support functions for MSSQL.
  type FunctionName 'MSSQL = MSSQL.FunctionName
  type FunctionArgument 'MSSQL = Void
  type ConstraintName 'MSSQL = MSSQL.ConstraintName
  type BasicOrderType 'MSSQL = MSSQL.Order
  type NullsOrderType 'MSSQL = MSSQL.NullsOrder
  type CountType 'MSSQL = MSSQL.Countable MSSQL.ColumnName
  type Column 'MSSQL = MSSQL.ColumnName
  type ScalarValue 'MSSQL = MSSQL.Value
  type ScalarType 'MSSQL = MSSQL.ScalarType
  type BooleanOperators 'MSSQL = MSSQL.BooleanOperators
  type SQLExpression 'MSSQL = MSSQL.Expression
  type ScalarSelectionArguments 'MSSQL = Void

  type BackendUpdate 'MSSQL = MSSQL.BackendUpdate

  type ComputedFieldDefinition 'MSSQL = Void
  type FunctionArgumentExp 'MSSQL = Const Void
  type ComputedFieldImplicitArguments 'MSSQL = Void
  type ComputedFieldReturn 'MSSQL = Void

  type ExtraTableMetadata 'MSSQL = [MSSQL.ColumnName] -- List of identity columns
  type BackendInsert 'MSSQL = MSSQL.BackendInsert

  type XComputedField 'MSSQL = XDisable
  type XRelay 'MSSQL = XDisable
  type XNodesAgg 'MSSQL = XEnable
  type XNestedInserts 'MSSQL = XDisable
  type XStreamingSubscription 'MSSQL = XDisable

  type HealthCheckTest 'MSSQL = HealthCheckTestSql
  healthCheckImplementation =
    Just $
      HealthCheckImplementation
        { _hciDefaultTest = defaultHealthCheckTestSql,
          _hciTestCodec = codec
        }

  isComparableType :: ScalarType 'MSSQL -> Bool
  isComparableType = MSSQL.isComparableType

  isNumType :: ScalarType 'MSSQL -> Bool
  isNumType = MSSQL.isNumType

  textToScalarValue :: Maybe Text -> ScalarValue 'MSSQL
  textToScalarValue = maybe ODBC.NullValue ODBC.TextValue

  parseScalarValue :: ScalarType 'MSSQL -> Value -> Either QErr (ScalarValue 'MSSQL)
  parseScalarValue = MSSQL.parseScalarValue

  -- TODO: Is this Postgres specific? Should it be removed from the class?
  scalarValueToJSON :: ScalarValue 'MSSQL -> Value
  scalarValueToJSON = error "Unexpected MSSQL error: calling scalarValueToJSON. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  functionToTable :: FunctionName 'MSSQL -> TableName 'MSSQL
  functionToTable = error "Unexpected MSSQL error: calling functionToTable. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  tableToFunction :: TableName 'MSSQL -> FunctionName 'MSSQL
  tableToFunction = MSSQL.FunctionName . MSSQL.tableName

  tableGraphQLName :: TableName 'MSSQL -> Either QErr G.Name
  tableGraphQLName = MSSQL.getGQLTableName

  functionGraphQLName :: FunctionName 'MSSQL -> Either QErr G.Name
  functionGraphQLName = error "Unexpected MSSQL error: calling functionGraphQLName. Please report this error at https://github.com/hasura/graphql-engine/issues/6590"

  snakeCaseTableName :: TableName 'MSSQL -> Text
  snakeCaseTableName = MSSQL.snakeCaseTableName

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

  resizeSourcePools :: SourceConfig 'MSSQL -> ServerReplicas -> IO ()
  resizeSourcePools sourceConfig =
    MSSQL.mssqlResizePools (MSSQL._mscExecCtx sourceConfig)

  defaultTriggerOnReplication = TOREnableTrigger
