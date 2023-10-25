module Hasura.RQL.Types.Metadata.Backend
  ( BackendMetadata (..),
  )
where

import Control.Arrow.Extended
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Environment qualified as Env
import Data.Has (Has)
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Incremental qualified as Inc
import Hasura.Logging (Hasura, Logger)
import Hasura.LogicalModel.Cache (LogicalModelInfo)
import Hasura.NativeQuery.Metadata (ArgumentName, InterpolatedQuery, NativeQueryMetadata)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.BoolExp
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.NamingCase (NamingCase)
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Source.Table (SourceTableInfo)
import Hasura.SQL.Types
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.Server.Migrate.Version
import Hasura.Services.Network
import Hasura.StoredProcedure.Metadata (StoredProcedureConfig, StoredProcedureMetadata)
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP

class
  ( Backend b,
    Eq (AggregationPredicates b (PartialSQLExp b)),
    Eq (BooleanOperators b (PartialSQLExp b)),
    Eq (FunctionArgumentExp b (PartialSQLExp b)),
    Ord (BackendInvalidationKeys b),
    Hashable (AggregationPredicates b (PartialSQLExp b)),
    Hashable (BooleanOperators b (PartialSQLExp b)),
    Hashable (FunctionArgumentExp b (PartialSQLExp b)),
    Monoid (BackendInvalidationKeys b)
  ) =>
  BackendMetadata (b :: BackendType)
  where
  buildComputedFieldInfo ::
    (MonadError QErr m) =>
    HashSet (TableName b) ->
    TableName b ->
    HashSet (Column b) ->
    ComputedFieldName ->
    ComputedFieldDefinition b ->
    RawFunctionInfo b ->
    Comment ->
    m (ComputedFieldInfo b)

  fetchAndValidateEnumValues ::
    (MonadIO m, MonadBaseControl IO m) =>
    SourceConfig b ->
    TableName b ->
    Maybe (PrimaryKey b (RawColumnInfo b)) ->
    [RawColumnInfo b] ->
    m (Either QErr EnumValues)

  type BackendInvalidationKeys b
  type BackendInvalidationKeys b = ()

  resolveBackendInfo ::
    ( ArrowChoice arr,
      Inc.ArrowCache m arr,
      Inc.ArrowDistribute arr,
      ArrowWriter (Seq CollectItem) arr,
      MonadIO m,
      MonadBaseControl IO m,
      ProvidesNetwork m
    ) =>
    Logger Hasura ->
    (Inc.Dependency (Maybe (BackendInvalidationKeys b)), BackendConfig b) `arr` BackendInfo b
  default resolveBackendInfo ::
    ( Arrow arr,
      BackendInfo b ~ ()
    ) =>
    Logger Hasura ->
    (Inc.Dependency (Maybe (BackendInvalidationKeys b)), BackendConfig b) `arr` BackendInfo b
  resolveBackendInfo = const $ arr $ const ()

  -- | Function that resolves the connection related source configuration, and
  -- creates a connection pool (and other related parameters) in the process
  resolveSourceConfig ::
    (MonadIO m, MonadBaseControl IO m, MonadResolveSource m) =>
    SourceName ->
    SourceConnConfiguration b ->
    BackendSourceKind b ->
    BackendInfo b ->
    Env.Environment ->
    HTTP.Manager ->
    m (Either QErr (SourceConfig b))

  -- | Function that introspects a database for tables, columns, functions etc.
  resolveDatabaseMetadata ::
    ( MonadIO m,
      MonadBaseControl IO m,
      MonadResolveSource m,
      FF.HasFeatureFlagChecker m
    ) =>
    Logger Hasura ->
    SourceMetadata b ->
    SourceConfig b ->
    m (Either QErr (DBObjectsIntrospection b))

  parseBoolExpOperations ::
    (MonadError QErr m) =>
    ValueParser b m v ->
    FieldInfoMap (FieldInfo b) -> -- The root table's FieldInfoMap
    FieldInfoMap (FieldInfo b) -> -- The FieldInfoMap of the table currently "in focus"
    ColumnReference b ->
    Value ->
    m [OpExpG b v]

  buildObjectRelationshipInfo ::
    (MonadError QErr m) =>
    SourceConfig b ->
    SourceName ->
    HashMap (TableName b) (HashSet (ForeignKey b)) ->
    TableName b ->
    ObjRelDef b ->
    m (RelInfo b, Seq SchemaDependency)

  buildArrayRelationshipInfo ::
    (MonadError QErr m) =>
    SourceConfig b ->
    SourceName ->
    HashMap (TableName b) (HashSet (ForeignKey b)) ->
    TableName b ->
    ArrRelDef b ->
    m (RelInfo b, Seq SchemaDependency)

  buildFunctionInfo ::
    (MonadError QErr m) =>
    SourceName ->
    FunctionName b ->
    SystemDefined ->
    FunctionConfig b ->
    FunctionPermissionsMap ->
    RawFunctionInfo b ->
    -- | the function comment
    Maybe Text ->
    NamingCase ->
    m (FunctionInfo b, SchemaDependency)

  updateColumnInEventTrigger ::
    TableName b ->
    Column b ->
    Column b ->
    TableName b ->
    EventTriggerConf b ->
    EventTriggerConf b

  parseCollectableType ::
    (MonadError QErr m, MonadReader r m, Has (ScalarTypeParsingContext b) r) =>
    CollectableType (ColumnType b) ->
    Value ->
    m (PartialSQLExp b)

  postDropSourceHook ::
    (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
    SourceConfig b ->
    TableEventTriggers b ->
    m ()

  -- TODO: rename?
  validateRelationship ::
    (MonadError QErr m) =>
    TableCache b ->
    TableName b ->
    Either (ObjRelDef b) (ArrRelDef b) ->
    m ()
  default validateRelationship ::
    (MonadError QErr m) =>
    TableCache b ->
    TableName b ->
    Either (ObjRelDef b) (ArrRelDef b) ->
    m ()
  validateRelationship _ _ _ = pure ()

  -- | Function that that builds a boolean expression field out of a computed field
  buildComputedFieldBooleanExp ::
    ( MonadError QErr m,
      TableCoreInfoRM b m
    ) =>
    BoolExpResolver b m v ->
    BoolExpRHSParser b m v ->
    FieldInfoMap (FieldInfo b) -> -- The root table's FieldInfoMap
    FieldInfoMap (FieldInfo b) -> -- The FieldInfoMap of the table currently "in focus"
    ComputedFieldInfo b ->
    Value ->
    m (AnnComputedFieldBoolExp b v)

  -- | Run all operations required to create, update, or migrate the internal
  -- catalog used by the backend for internal bookkeeping, if any. The return
  -- type indicates whether the performed operations subsequently require
  -- re-creating event trigers.
  prepareCatalog ::
    (MonadIO m, MonadBaseControl IO m) =>
    SourceConfig b ->
    ExceptT QErr m (RecreateEventTriggers, SourceCatalogMigrationState)

  -- | List all the tables on a given data source, including those not tracked
  -- by Hasura. Primarily useful for user interfaces to allow untracked tables
  -- to be tracked.
  listAllTables ::
    (CacheRM m, MonadBaseControl IO m, MetadataM m, MonadError QErr m, MonadIO m, MonadReader r m, Has (Logger Hasura) r, ProvidesNetwork m) =>
    SourceName ->
    m [TableName b]

  -- | List all the functions on a given data source, including those not tracked
  -- by Hasura. Primarily useful for user interfaces to allow untracked functions
  -- to be tracked.
  listAllTrackables ::
    (CacheRM m, MonadBaseControl IO m, MetadataM m, MonadError QErr m, MonadIO m, MonadReader r m, Has (Logger Hasura) r, ProvidesNetwork m) =>
    SourceName ->
    m (TrackableInfo b)

  -- | Get information about a given table on a given source, whether tracked
  -- or not. Primarily useful for user interfaces.
  getTableInfo ::
    (CacheRM m, MetadataM m, MonadError QErr m, MonadBaseControl IO m, MonadIO m) =>
    SourceName ->
    TableName b ->
    m (Maybe (SourceTableInfo b))

  validateNativeQuery ::
    (MonadIO m, MonadError QErr m) =>
    Env.Environment ->
    SourceName ->
    SourceConnConfiguration b ->
    SourceConfig b ->
    LogicalModelInfo b ->
    NativeQueryMetadata b ->
    m (InterpolatedQuery ArgumentName)
  validateNativeQuery _ _ _ _ _ _ =
    throw500 "validateNativeQuery: not implemented for this backend."

  validateStoredProcedure ::
    (MonadIO m, MonadError QErr m) =>
    Env.Environment ->
    SourceConnConfiguration b ->
    LogicalModelInfo b ->
    StoredProcedureMetadata b ->
    m ()
  validateStoredProcedure _ _ _ _ =
    throw500 "validateStoredProcedure: not implemented for this backend."

  getStoredProcedureGraphqlName ::
    (MonadError QErr m) =>
    FunctionName b ->
    StoredProcedureConfig ->
    m G.Name
  getStoredProcedureGraphqlName _ _ =
    throw500 "getStoredProcedureGraphqlName: not implemented for this backend."

  -- | Allows the backend to control whether or not a particular source supports being
  -- the target of remote relationships or not
  supportsBeingRemoteRelationshipTarget :: SourceConfig b -> Bool
