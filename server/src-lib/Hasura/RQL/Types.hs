module Hasura.RQL.Types
  ( MonadTx(..)

  , SQLGenCtx(..)
  , HasSQLGenCtx(..)

  , RemoteSchemaPermsCtx(..)
  , HasRemoteSchemaPermsCtx(..)

  , HasSystemDefined(..)
  , HasSystemDefinedT
  , runHasSystemDefinedT

  , askSourceInfo
  , askSourceConfig
  , askSourceTables
  , askTableCache
  , askTabInfo
  , askTabInfoSource
  , askTableCoreInfo
  , askTableCoreInfoSource
  , askFieldInfoMap
  , askFieldInfoMapSource
  , askPGType
  , assertPGCol
  , askRelType
  , askFieldInfo
  , askPGColInfo
  , askComputedFieldInfo
  , askRemoteRel
  , findTable

  , module R
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M
import qualified Data.Text                           as T
import qualified Database.PG.Query                   as Q

import           Control.Monad.Unique
import           Data.Aeson
import           Data.Text.Extended
import           Network.HTTP.Client.Extended        (HasHttpManagerM (..))

import qualified Hasura.Backends.Postgres.SQL.Types  as PG

import           Hasura.Backends.Postgres.Connection as R
import           Hasura.RQL.IR.BoolExp               as R
import           Hasura.RQL.Types.Action             as R
import           Hasura.RQL.Types.Column             as R
import           Hasura.RQL.Types.Common             as R
import           Hasura.RQL.Types.ComputedField      as R
import           Hasura.RQL.Types.CustomTypes        as R
import           Hasura.RQL.Types.Endpoint           as R
import           Hasura.RQL.Types.Error              as R
import           Hasura.RQL.Types.EventTrigger       as R
import           Hasura.RQL.Types.Function           as R
import           Hasura.RQL.Types.Metadata           as R
import           Hasura.RQL.Types.Permission         as R
import           Hasura.RQL.Types.QueryCollection    as R
import           Hasura.RQL.Types.Relationship       as R
import           Hasura.RQL.Types.RemoteRelationship as R
import           Hasura.RQL.Types.RemoteSchema       as R
import           Hasura.RQL.Types.ScheduledTrigger   as R
import           Hasura.RQL.Types.SchemaCache        as R
import           Hasura.RQL.Types.SchemaCache.Build  as R
import           Hasura.RQL.Types.SchemaCacheTypes   as R
import           Hasura.RQL.Types.Source             as R
import           Hasura.RQL.Types.Table              as R
import           Hasura.SQL.Backend                  as R
import           Hasura.Session
import           Hasura.Tracing


askSourceInfo
  :: (CacheRM m, MonadError QErr m)
  => SourceName -> m (SourceInfo 'Postgres)
askSourceInfo sourceName = do
  sources <- scPostgres <$> askSchemaCache
  onNothing (unsafeSourceInfo =<< M.lookup sourceName sources) $
    -- FIXME: this error can also happen for a lookup with the wrong type
    throw400 NotExists $ "source with name " <> sourceName <<> " does not exist"

askSourceConfig
  :: (CacheRM m, MonadError QErr m)
  => SourceName -> m (SourceConfig 'Postgres)
askSourceConfig = fmap _siConfiguration . askSourceInfo

askSourceTables :: CacheRM m => SourceName -> m (TableCache 'Postgres)
askSourceTables sourceName = do
  sources <- scPostgres <$> askSchemaCache
  pure $ fromMaybe mempty $ unsafeSourceTables =<< M.lookup sourceName sources


askTabInfo
  :: (QErrM m, CacheRM m)
  => SourceName -> PG.QualifiedTable -> m (TableInfo 'Postgres)
askTabInfo sourceName tableName = do
  rawSchemaCache <- askSchemaCache
  unsafeTableInfo sourceName tableName (scPostgres rawSchemaCache)
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "table " <> tableName <<> " does not exist in source: " <> sourceNameToText sourceName

askTabInfoSource
  :: (QErrM m, TableInfoRM 'Postgres m)
  => PG.QualifiedTable -> m (TableInfo 'Postgres)
askTabInfoSource tableName = do
  lookupTableInfo tableName >>= (`onNothing` throwTableDoesNotExist tableName)

data RemoteSchemaPermsCtx
  = RemoteSchemaPermsEnabled
  | RemoteSchemaPermsDisabled
  deriving (Show, Eq)

instance FromJSON RemoteSchemaPermsCtx where
  parseJSON = withText "RemoteSchemaPermsCtx" $ \t ->
    case T.toLower t of
      "true"  -> pure RemoteSchemaPermsEnabled
      "false" -> pure RemoteSchemaPermsDisabled
      _       -> fail "enable_remote_schema_permissions should be a boolean value"

instance ToJSON RemoteSchemaPermsCtx where
  toJSON = \case
    RemoteSchemaPermsEnabled  -> "true"
    RemoteSchemaPermsDisabled -> "false"

class (Monad m) => HasRemoteSchemaPermsCtx m where
  askRemoteSchemaPermsCtx :: m RemoteSchemaPermsCtx

instance (HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (ReaderT r m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx
instance (HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (StateT s m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx
instance (Monoid w, HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (WriterT w m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx
instance (HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (TableCoreCacheRT b m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx
instance (HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (TraceT m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx
instance (HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (MetadataT m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx
instance (HasRemoteSchemaPermsCtx m)
         => HasRemoteSchemaPermsCtx (LazyTxT QErr m) where
  askRemoteSchemaPermsCtx = lift askRemoteSchemaPermsCtx

class (Monad m) => HasSQLGenCtx m where
  askSQLGenCtx :: m SQLGenCtx

instance (HasSQLGenCtx m) => HasSQLGenCtx (ReaderT r m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (StateT s m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (Monoid w, HasSQLGenCtx m) => HasSQLGenCtx (WriterT w m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (TableCoreCacheRT b m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (TraceT m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (MetadataT m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (Q.TxET QErr m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (LazyTxT QErr m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (TableCacheRT b m) where
  askSQLGenCtx = lift askSQLGenCtx

class (Monad m) => HasSystemDefined m where
  askSystemDefined :: m SystemDefined

instance (HasSystemDefined m) => HasSystemDefined (ReaderT r m) where
  askSystemDefined = lift askSystemDefined
instance (HasSystemDefined m) => HasSystemDefined (StateT s m) where
  askSystemDefined = lift askSystemDefined
instance (Monoid w, HasSystemDefined m) => HasSystemDefined (WriterT w m) where
  askSystemDefined = lift askSystemDefined
instance (HasSystemDefined m) => HasSystemDefined (TraceT m) where
  askSystemDefined = lift askSystemDefined

newtype HasSystemDefinedT m a
  = HasSystemDefinedT { unHasSystemDefinedT :: ReaderT SystemDefined m a }
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadUnique, MonadError e, MonadTx
           , HasHttpManagerM, HasSQLGenCtx, SourceM, TableCoreInfoRM b, CacheRM, UserInfoM, HasRemoteSchemaPermsCtx)

runHasSystemDefinedT :: SystemDefined -> HasSystemDefinedT m a -> m a
runHasSystemDefinedT systemDefined = flip runReaderT systemDefined . unHasSystemDefinedT

instance (Monad m) => HasSystemDefined (HasSystemDefinedT m) where
  askSystemDefined = HasSystemDefinedT ask

throwTableDoesNotExist :: (QErrM m) => PG.QualifiedTable -> m a
throwTableDoesNotExist tableName = throw400 NotExists ("table " <> tableName <<> " does not exist")

findTable :: (QErrM m) => PG.QualifiedTable -> HashMap PG.QualifiedTable a -> m a
findTable tableName infoMap =
  M.lookup tableName infoMap `onNothing` throwTableDoesNotExist tableName

askTableCache
  :: (QErrM m, CacheRM m) => SourceName -> m (TableCache 'Postgres)
askTableCache sourceName = do
  schemaCache <- askSchemaCache
  sourceInfo  <- M.lookup sourceName (scPostgres schemaCache)
    `onNothing` throw400 NotExists ("source " <> sourceName <<> " does not exist")
  unsafeSourceTables sourceInfo
    `onNothing` throw400 NotExists ("source " <> sourceName <<> " is not a PG cache")

askTableCoreInfo
  :: (QErrM m, CacheRM m) => SourceName -> TableName 'Postgres -> m (TableCoreInfo 'Postgres)
askTableCoreInfo sourceName tableName =
  _tiCoreInfo <$> askTabInfo sourceName tableName

-- | Asking for a table core info without explicit @'SourceName' argument.
-- The source name is implicitly inferred from @'SourceM' via @'TableCoreInfoRM'.
-- This is useful in RQL DML queries which are executed in a particular source database.
askTableCoreInfoSource
  :: (QErrM m, TableCoreInfoRM 'Postgres m) => PG.QualifiedTable -> m (TableCoreInfo 'Postgres)
askTableCoreInfoSource tableName =
  lookupTableCoreInfo tableName >>= (`onNothing` throwTableDoesNotExist tableName)

askFieldInfoMap
  :: (QErrM m, CacheRM m)
  => SourceName -> TableName 'Postgres -> m (FieldInfoMap (FieldInfo 'Postgres))
askFieldInfoMap sourceName tableName =
  _tciFieldInfoMap . _tiCoreInfo <$> askTabInfo sourceName tableName

-- | Asking for a table's fields info without explicit @'SourceName' argument.
-- The source name is implicitly inferred from @'SourceM' via @'TableCoreInfoRM'.
-- This is useful in RQL DML queries which are executed in a particular source database.
askFieldInfoMapSource
  :: (QErrM m, TableCoreInfoRM 'Postgres m)
  => PG.QualifiedTable -> m (FieldInfoMap (FieldInfo 'Postgres))
askFieldInfoMapSource tableName =
  _tciFieldInfoMap <$> askTableCoreInfoSource tableName

askPGType
  :: (MonadError QErr m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> PG.PGCol
  -> Text
  -> m (ColumnType 'Postgres)
askPGType m c msg =
  pgiType <$> askPGColInfo m c msg

askPGColInfo
  :: (MonadError QErr m)
  => FieldInfoMap (FieldInfo backend)
  -> PG.PGCol
  -> Text
  -> m (ColumnInfo backend)
askPGColInfo m c msg = do
  fieldInfo <- modifyErr ("column " <>) $
             askFieldInfo m (fromCol @'Postgres c)
  case fieldInfo of
    (FIColumn pgColInfo)     -> pure pgColInfo
    (FIRelationship   _)     -> throwErr "relationship"
    (FIComputedField _)      -> throwErr "computed field"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a postgres column; but, "
      , c <<> " is a " <> fieldType <> "; "
      , msg
      ]

askComputedFieldInfo
  :: (MonadError QErr m)
  => FieldInfoMap (FieldInfo backend)
  -> ComputedFieldName
  -> m (ComputedFieldInfo backend)
askComputedFieldInfo fields computedField = do
  fieldInfo <- modifyErr ("computed field " <>) $
               askFieldInfo fields $ fromComputedField computedField
  case fieldInfo of
    (FIColumn           _)       -> throwErr "column"
    (FIRelationship     _)       -> throwErr "relationship"
    (FIRemoteRelationship     _) -> throwErr "remote relationship"
    (FIComputedField cci)        -> pure cci
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a computed field; but, "
      , computedField <<> " is a " <> fieldType <> "; "
      ]

assertPGCol :: (MonadError QErr m)
            => FieldInfoMap (FieldInfo backend)
            -> Text
            -> PG.PGCol
            -> m ()
assertPGCol m msg c = do
  _ <- askPGColInfo m c msg
  return ()

askRelType :: (MonadError QErr m)
           => FieldInfoMap (FieldInfo backend)
           -> RelName
           -> Text
           -> m (RelInfo backend)
askRelType m r msg = do
  colInfo <- modifyErr ("relationship " <>) $
             askFieldInfo m (fromRel r)
  case colInfo of
    (FIRelationship relInfo) -> return relInfo
    _                        ->
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a relationship; but, "
      , r <<> " is a postgres column; "
      , msg
      ]

askFieldInfo :: (MonadError QErr m)
             => FieldInfoMap fieldInfo
             -> FieldName
             -> m fieldInfo
askFieldInfo m f =
  onNothing (M.lookup f m) $ throw400 NotExists (f <<> " does not exist")

askRemoteRel :: (MonadError QErr m)
           => FieldInfoMap (FieldInfo backend)
           -> RemoteRelationshipName
           -> m (RemoteFieldInfo backend)
askRemoteRel fieldInfoMap relName = do
  fieldInfo <- askFieldInfo fieldInfoMap (fromRemoteRelationship relName)
  case fieldInfo of
    (FIRemoteRelationship remoteFieldInfo) -> return remoteFieldInfo
    _                                      ->
      throw400 UnexpectedPayload "expecting a remote relationship"
