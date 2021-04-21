module Hasura.RQL.Types.Metadata.Backend where

import           Hasura.Prelude

import qualified Data.Environment                    as Env

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Aeson

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
import           Hasura.RQL.Types.Source
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Server.Types


class (Backend b) => BackendMetadata (b :: BackendType) where

  buildComputedFieldInfo
    :: (MonadError QErr m)
    => HashSet (TableName b)
    -> TableName b
    -> ComputedFieldName
    -> ComputedFieldDefinition b
    -> RawFunctionInfo -- TODO: Parameterize this too
    -> Maybe Text
    -> m (ComputedFieldInfo b)

  buildRemoteFieldInfo
    :: (MonadError QErr m)
    => RemoteRelationship b
    -> [ColumnInfo b]
    -> RemoteSchemaMap
    -> m (RemoteFieldInfo b, [SchemaDependency])

  fetchAndValidateEnumValues
    :: (MonadIO m, MonadBaseControl IO m)
    => SourceConfig b
    -> TableName b
    -> Maybe (PrimaryKey b (RawColumnInfo b))
    -> [RawColumnInfo b]
    -> m (Either QErr EnumValues)

  -- | Function that resolves the connection related source configuration, and
  -- creates a connection pool (and other related parameters) in the process
  resolveSourceConfig
    :: (MonadIO m, MonadBaseControl IO m, MonadResolveSource m)
    => SourceName
    -> SourceConnConfiguration b
    -> m (Either QErr (SourceConfig b))

  -- | Function that introspects a database for tables, columns, functions etc.
  resolveDatabaseMetadata
    :: (MonadIO m, MonadBaseControl IO m, MonadResolveSource m)
    => SourceConfig b
    -> m (Either QErr (ResolvedSource b))

  createTableEventTrigger
    :: (MonadBaseControl IO m, MonadIO m)
    => ServerConfigCtx
    -> SourceConfig b
    -> TableName b
    -> [ColumnInfo b]
    -> TriggerName
    -> TriggerOpsDef
    -> m (Either QErr ())

  buildEventTriggerInfo
    :: MonadError QErr m
    => Env.Environment
    -> SourceName
    -> TableName b
    -> EventTriggerConf
    -> m (EventTriggerInfo, [SchemaDependency])

  parseBoolExpOperations
    :: (MonadError QErr m, TableCoreInfoRM b m)
    => ValueParser b m v
    -> TableName b
    -> FieldInfoMap (FieldInfo b)
    -> ColumnInfo b
    -> Value
    -> m [OpExpG b v]

  buildFunctionInfo
    :: (MonadError QErr m)
    => SourceName
    -> FunctionName b
    -> SystemDefined
    -> FunctionConfig
    -> [FunctionPermissionMetadata]
    -> RawFunctionInfo
    -> m (FunctionInfo b, SchemaDependency)

  updateColumnInEventTrigger
    :: TableName b
    -> Column b
    -> Column b
    -> TableName b
    -> EventTriggerConf
    -> EventTriggerConf

  parseCollectableType
    :: (MonadError QErr m)
    => CollectableType (ColumnType b)
    -> Value
    -> m (PartialSQLExp b)

  postDropSourceHook
    :: (MonadError QErr m, MonadIO m, MonadBaseControl IO m)
    => SourceConfig b
    -> m ()
