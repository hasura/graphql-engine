{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Metadata.Class where

import qualified Data.Aeson                         as J
import qualified Data.Aeson.Casing                  as J
import qualified Data.Aeson.TH                      as J
import qualified Data.Time                          as UTC
import qualified Database.PG.Query                  as Q

import           Hasura.Db
import           Hasura.Prelude
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.SchemaCache.Build
import           Hasura.Server.Types
import           Hasura.Server.Utils                (fmapL)

import qualified Hasura.Tracing                     as Tracing

data SchemaSyncEventProcessResult
  = SchemaSyncEventProcessResult
  { _sseprShouldReload       :: !Bool
  , _sseprCacheInvalidations :: !CacheInvalidations
  }

data SchemaSyncEventPayload
  = SchemaSyncEventPayload
  { _ssepInstanceId    :: !InstanceId
  , _ssepOccurredAt    :: !UTC.UTCTime
  , _ssepInvalidations :: !CacheInvalidations
  }
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''SchemaSyncEventPayload)

-- A broader interface to manage all metadata related operations
class Monad m => MonadMetadataManage m where

  fetchMetadataTx :: m (Q.TxE QErr Metadata)
  default fetchMetadataTx :: m (Q.TxE QErr Metadata)
  fetchMetadataTx = pure getMetadata

  updateMetadataTx :: m (Metadata -> Q.TxE QErr ())
  default updateMetadataTx :: m (Metadata -> Q.TxE QErr ())
  updateMetadataTx = pure setMetadata

  notifySchemaSyncTx :: m (InstanceId -> CacheInvalidations -> Q.TxE QErr ())
  default notifySchemaSyncTx :: m (InstanceId -> CacheInvalidations -> Q.TxE QErr ())
  notifySchemaSyncTx = pure notifySchemaSync

  processSchemaSyncEventPayload :: InstanceId -> J.Value -> m (Either Text SchemaSyncEventProcessResult)
  default processSchemaSyncEventPayload :: InstanceId -> J.Value -> m (Either Text SchemaSyncEventProcessResult)
  processSchemaSyncEventPayload instanceId payloadValue = pure $ do
    eventPayload <- fmapL qeError $ runExcept $ decodeValue payloadValue
    let _sseprShouldReload = instanceId /= _ssepInstanceId eventPayload
        _sseprCacheInvalidations = _ssepInvalidations eventPayload
    pure SchemaSyncEventProcessResult{..}

getMetadata :: Q.TxE QErr Metadata
getMetadata = do
  rows <- Q.withQE defaultTxErrorHandler
    [Q.sql|
       SELECT metadata from hdb_catalog.hdb_metadata where id = 1
    |] () True
  case rows of
    []                             -> pure emptyMetadata
    [(Identity (Q.AltJ metadata))] -> pure metadata
    _                              -> throw500 "multiple rows in hdb_metadata table"

setMetadata :: Metadata -> Q.TxE QErr ()
setMetadata metadata =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_metadata
       (id, metadata) VALUES (1, $1::json)
     ON CONFLICT (id) DO UPDATE SET metadata = $1::json
    |] (Identity $ Q.AltJ metadata) True

notifySchemaSync :: InstanceId -> CacheInvalidations -> Q.TxE QErr ()
notifySchemaSync instanceId invalidations = do
  Q.Discard () <- Q.withQE defaultTxErrorHandler [Q.sql|
      SELECT pg_notify('hasura_schema_update', json_build_object(
        'instance_id', $1,
        'occurred_at', NOW(),
        'invalidations', $2
        )::text
      )
    |] (instanceId, Q.AltJ invalidations) True
  pure ()

instance MonadMetadataManage m => MonadMetadataManage (ReaderT r m) where
  fetchMetadataTx = lift fetchMetadataTx
  updateMetadataTx = lift updateMetadataTx
  notifySchemaSyncTx = lift notifySchemaSyncTx

instance MonadMetadataManage m => MonadMetadataManage (ExceptT e m) where
  fetchMetadataTx = lift fetchMetadataTx
  updateMetadataTx = lift updateMetadataTx
  notifySchemaSyncTx = lift notifySchemaSyncTx

instance MonadMetadataManage m => MonadMetadataManage (Tracing.TraceT m) where
  fetchMetadataTx = lift fetchMetadataTx
  updateMetadataTx = lift updateMetadataTx
  notifySchemaSyncTx = lift notifySchemaSyncTx

class (Monad m) => MonadMetadata m where
  -- | Fetch @'Metadata' from storage. If no metadata stored, return @'emptyMetadata'.
  fetchMetadata :: m Metadata

  -- | Update the given @'Metadata' in the storage
  updateMetadata :: Metadata -> m ()

instance MonadMetadata m => MonadMetadata (ReaderT r m) where
  fetchMetadata = lift fetchMetadata
  updateMetadata = lift . updateMetadata

instance MonadMetadata m => MonadMetadata (StateT s m) where
  fetchMetadata = lift fetchMetadata
  updateMetadata = lift . updateMetadata

instance MonadMetadata m => MonadMetadata (Tracing.TraceT m) where
  fetchMetadata = lift fetchMetadata
  updateMetadata = lift . updateMetadata
