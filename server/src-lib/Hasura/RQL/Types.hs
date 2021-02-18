module Hasura.RQL.Types
  ( MonadTx(..)

  , SQLGenCtx(..)
  , RemoteSchemaPermsCtx(..)

  , ServerConfigCtx(..)
  , HasServerConfigCtx(..)

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
  , assertPGCol
  , askRelType
  , askComputedFieldInfo
  , askRemoteRel
  , findTable

  , module R
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M
import qualified Database.PG.Query                   as Q

import           Control.Monad.Unique
import           Data.Text.Extended
import           Network.HTTP.Client.Extended        (HasHttpManagerM (..))

import           Hasura.Backends.Postgres.Connection as R
import           Hasura.RQL.IR.BoolExp               as R
import           Hasura.RQL.Types.Action             as R
import           Hasura.RQL.Types.ApiLimit           as R
import           Hasura.RQL.Types.Backend            as R
import           Hasura.RQL.Types.Column             as R
import           Hasura.RQL.Types.Common             as R
import           Hasura.RQL.Types.ComputedField      as R
import           Hasura.RQL.Types.CustomTypes        as R
import           Hasura.RQL.Types.Endpoint           as R
import           Hasura.RQL.Types.Error              as R
import           Hasura.RQL.Types.EventTrigger       as R
import           Hasura.RQL.Types.Function           as R
import           Hasura.RQL.Types.Metadata           as R
import           Hasura.RQL.Types.Metadata.Backend   as R
import           Hasura.RQL.Types.Metadata.Object    as R
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
import           Hasura.Server.Types
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

askSourceTables :: (Backend b) => CacheRM m => SourceName -> m (TableCache b)
askSourceTables sourceName = do
  sources <- scPostgres <$> askSchemaCache
  pure $ fromMaybe mempty $ unsafeSourceTables =<< M.lookup sourceName sources


askTabInfo
  :: (QErrM m, CacheRM m, Backend b)
  => SourceName -> TableName b -> m (TableInfo b)
askTabInfo sourceName tableName = do
  rawSchemaCache <- askSchemaCache
  unsafeTableInfo sourceName tableName (scPostgres rawSchemaCache)
    `onNothing` throw400 NotExists errMsg
  where
    errMsg = "table " <> tableName <<> " does not exist in source: " <> sourceNameToText sourceName

askTabInfoSource
  :: (QErrM m, TableInfoRM b m, Backend b)
  => TableName b -> m (TableInfo b)
askTabInfoSource tableName = do
  lookupTableInfo tableName >>= (`onNothing` throwTableDoesNotExist tableName)

class (Monad m) => HasServerConfigCtx m where
  askServerConfigCtx :: m ServerConfigCtx

instance (HasServerConfigCtx m)
         => HasServerConfigCtx (ReaderT r m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m)
         => HasServerConfigCtx (StateT s m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (Monoid w, HasServerConfigCtx m)
         => HasServerConfigCtx (WriterT w m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m)
         => HasServerConfigCtx (TableCoreCacheRT b m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m)
         => HasServerConfigCtx (TraceT m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m)
         => HasServerConfigCtx (MetadataT m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m)
         => HasServerConfigCtx (LazyTxT QErr m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m) => HasServerConfigCtx (Q.TxET QErr m) where
  askServerConfigCtx = lift askServerConfigCtx
instance (HasServerConfigCtx m) => HasServerConfigCtx (TableCacheRT b m) where
  askServerConfigCtx = lift askServerConfigCtx

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
           , HasHttpManagerM, SourceM, TableCoreInfoRM b, CacheRM, UserInfoM, HasServerConfigCtx)

runHasSystemDefinedT :: SystemDefined -> HasSystemDefinedT m a -> m a
runHasSystemDefinedT systemDefined = flip runReaderT systemDefined . unHasSystemDefinedT

instance (Monad m) => HasSystemDefined (HasSystemDefinedT m) where
  askSystemDefined = HasSystemDefinedT ask

throwTableDoesNotExist :: (QErrM m, Backend b) => TableName b -> m a
throwTableDoesNotExist tableName = throw400 NotExists ("table " <> tableName <<> " does not exist")

findTable :: (QErrM m, Backend b) => TableName b -> HashMap (TableName b) a -> m a
findTable tableName infoMap =
  M.lookup tableName infoMap `onNothing` throwTableDoesNotExist tableName

askTableCache
  :: (QErrM m, CacheRM m, Backend b) => SourceName -> m (TableCache b)
askTableCache sourceName = do
  schemaCache <- askSchemaCache
  sourceInfo  <- M.lookup sourceName (scPostgres schemaCache)
    `onNothing` throw400 NotExists ("source " <> sourceName <<> " does not exist")
  unsafeSourceTables sourceInfo
    `onNothing` throw400 NotExists ("source " <> sourceName <<> " is not a PG cache")

askTableCoreInfo
  :: (QErrM m, CacheRM m, Backend b)
  => SourceName -> TableName b -> m (TableCoreInfo b)
askTableCoreInfo sourceName tableName =
  _tiCoreInfo <$> askTabInfo sourceName tableName

-- | Asking for a table core info without explicit @'SourceName' argument.
-- The source name is implicitly inferred from @'SourceM' via @'TableCoreInfoRM'.
-- This is useful in RQL DML queries which are executed in a particular source database.
askTableCoreInfoSource
  :: (QErrM m, Backend b, TableCoreInfoRM b m) => TableName b -> m (TableCoreInfo b)
askTableCoreInfoSource tableName =
  lookupTableCoreInfo tableName >>= (`onNothing` throwTableDoesNotExist tableName)

askFieldInfoMap
  :: (QErrM m, CacheRM m, Backend b)
  => SourceName -> TableName b -> m (FieldInfoMap (FieldInfo b))
askFieldInfoMap sourceName tableName =
  _tciFieldInfoMap . _tiCoreInfo <$> askTabInfo sourceName tableName

-- | Asking for a table's fields info without explicit @'SourceName' argument.
-- The source name is implicitly inferred from @'SourceM' via @'TableCoreInfoRM'.
-- This is useful in RQL DML queries which are executed in a particular source database.
askFieldInfoMapSource
  :: (QErrM m, Backend b, TableCoreInfoRM b m)
  => TableName b -> m (FieldInfoMap (FieldInfo b))
askFieldInfoMapSource tableName =
  _tciFieldInfoMap <$> askTableCoreInfoSource tableName

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

assertPGCol :: (MonadError QErr m, Backend backend)
            => FieldInfoMap (FieldInfo backend)
            -> Text
            -> Column backend
            -> m ()
assertPGCol m msg c = do
  _ <- askColInfo m c msg
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
