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
import Hasura.CustomReturnType.Metadata (CustomReturnTypeMetadata)
import Hasura.Function.Cache
import Hasura.GraphQL.Schema.NamingCase
import Hasura.Incremental qualified as Inc
import Hasura.Logging (Hasura, Logger)
import Hasura.NativeQuery.Metadata (NativeQueryMetadata)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BoolExp
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Hasura.SQL.Types
import Hasura.Server.Migrate.Version
import Hasura.Services.Network
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
      ArrowWriter (Seq (Either InconsistentMetadata MetadataDependency)) arr,
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
    (MonadIO m, MonadBaseControl IO m, MonadResolveSource m) =>
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

  buildFunctionInfo ::
    (MonadError QErr m) =>
    SourceName ->
    FunctionName b ->
    SystemDefined ->
    FunctionConfig ->
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
    (MonadError QErr m) =>
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
    MonadError QErr m =>
    TableCache b ->
    TableName b ->
    Either (ObjRelDef b) (ArrRelDef b) ->
    m ()
  default validateRelationship ::
    MonadError QErr m =>
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

  validateNativeQuery ::
    (MonadIO m, MonadError QErr m) =>
    Env.Environment ->
    SourceConnConfiguration b ->
    CustomReturnTypeMetadata b ->
    NativeQueryMetadata b ->
    m ()
  validateNativeQuery _ _ _ _ =
    throw500 "validateNativeQuery: not implemented for this backend."

  -- | How to convert a column to a field.
  -- For backends that don't support nested objects or arrays the default implementation
  -- (i.e. wrapping the ColumnInfo in FIColumn) is what you want.
  columnInfoToFieldInfo ::
    HashMap G.Name (TableObjectType b) ->
    ColumnInfo b ->
    FieldInfo b
  columnInfoToFieldInfo _ = FIColumn

  -- | Allows the backend to control whether or not a particular source supports being
  -- the target of remote relationships or not
  supportsBeingRemoteRelationshipTarget :: SourceConfig b -> Bool
