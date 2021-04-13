module Hasura.Backends.BigQuery.DDL
  ( buildComputedFieldInfo
  , buildRemoteFieldInfo
  , fetchAndValidateEnumValues
  , createTableEventTrigger
  , buildEventTriggerInfo
  , buildFunctionInfo
  , updateColumnInEventTrigger
  , parseBoolExpOperations
  , parseCollectableType
  , module M
  )
where

import           Hasura.Backends.BigQuery.DDL.BoolExp
import           Hasura.Prelude

import           Data.Aeson

import qualified Data.Environment                         as Env

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.SchemaCache
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Server.Types
import           Hasura.Backends.BigQuery.Instances.Types ()
import           Hasura.Server.Utils
import           Hasura.Session

import qualified Hasura.Backends.BigQuery.Types           as BigQuery
import           Hasura.Backends.BigQuery.DDL.Source      as M

buildComputedFieldInfo
  :: (MonadError QErr m)
  => HashSet (TableName 'BigQuery)
  -> TableName 'BigQuery
  -> ComputedFieldName
  -> ComputedFieldDefinition 'BigQuery
  -> RawFunctionInfo
  -> Maybe Text
  -> m (ComputedFieldInfo 'BigQuery)
buildComputedFieldInfo _ _ _ _ _ _ =
  throw400 NotSupported "Computed fields aren't supported for BigQuery sources"

buildRemoteFieldInfo
  :: (MonadError QErr m)
  => RemoteRelationship 'BigQuery
  -> [ColumnInfo 'BigQuery]
  -> RemoteSchemaMap
  -> m (RemoteFieldInfo 'BigQuery, [SchemaDependency])
buildRemoteFieldInfo _ _ _ =
  throw400 NotSupported "Remote joins aren't supported for BigQuery sources"

fetchAndValidateEnumValues
  :: (Monad m)
  => SourceConfig 'BigQuery
  -> TableName 'BigQuery
  -> Maybe (PrimaryKey 'BigQuery (RawColumnInfo 'BigQuery))
  -> [RawColumnInfo 'BigQuery]
  -> m (Either QErr EnumValues)
fetchAndValidateEnumValues _ _ _ _ = runExceptT $
  throw400 NotSupported "Enum tables are not supported for BigQuery sources"

createTableEventTrigger
  :: (Monad m)
  => ServerConfigCtx
  -> SourceConfig 'BigQuery
  -> TableName 'BigQuery
  -> [ColumnInfo 'BigQuery]
  -> TriggerName
  -> TriggerOpsDef
  -> m (Either QErr ())
createTableEventTrigger _ _ _ _ _ _ = runExceptT $
  throw400 NotSupported "Cannot create table event triggers in BigQuery sources"

buildEventTriggerInfo
  :: MonadError QErr m
  => Env.Environment
  -> SourceName
  -> TableName 'BigQuery
  -> EventTriggerConf
  -> m (EventTriggerInfo, [SchemaDependency])
buildEventTriggerInfo _ _ _ _ =
  throw400 NotSupported "Table event triggers are not supported for BigQuery sources"

buildFunctionInfo
  :: (MonadError QErr m)
  => SourceName
  -> FunctionName 'BigQuery
  -> SystemDefined
  -> FunctionConfig
  -> [FunctionPermissionMetadata]
  -> RawFunctionInfo
  -> m (FunctionInfo 'BigQuery, SchemaDependency)
buildFunctionInfo _ _ _ _ _ _ =
  throw400 NotSupported "SQL Functions are not supported for BigQuery source"

updateColumnInEventTrigger
  :: TableName 'BigQuery
  -> Column 'BigQuery
  -> Column 'BigQuery
  -> TableName 'BigQuery
  -> EventTriggerConf
  -> EventTriggerConf
updateColumnInEventTrigger _ _ _ _ = id

parseCollectableType
  :: (MonadError QErr m)
  => CollectableType (ColumnType 'BigQuery)
  -> Value
  -> m (PartialSQLExp 'BigQuery)
parseCollectableType collectableType = \case
  String t
    | isSessionVariable t -> pure $ mkTypedSessionVar collectableType $ mkSessionVariable t
    | isReqUserId t       -> pure $ mkTypedSessionVar collectableType userIdHeader
  val -> case collectableType of
    CollectableTypeScalar scalarType ->
      PSESQLExp . BigQuery.ValueExpression <$> parseScalarValueColumnType scalarType val
    CollectableTypeArray _           ->
      throw400 NotSupported "Array types are not supported in BigQuery backend"

mkTypedSessionVar
  :: CollectableType (ColumnType 'BigQuery)
  -> SessionVariable -> PartialSQLExp 'BigQuery
mkTypedSessionVar columnType =
  PSESessVar (msColumnTypeToScalarType <$> columnType)

msColumnTypeToScalarType :: ColumnType 'BigQuery -> ScalarType 'BigQuery
msColumnTypeToScalarType = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _   -> BigQuery.StringScalarType
