{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Metadata.Class where

import qualified Database.PG.Query         as Q

import           Hasura.Db
import           Hasura.Prelude
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Metadata

import qualified Hasura.Tracing            as Tracing

-- A broader interface to manage all metadata related operations
class Monad m => MonadMetadataManage m where

  updateMetadataTx :: m (Metadata -> Q.TxE QErr ())
  default updateMetadataTx :: m (Metadata -> Q.TxE QErr ())
  updateMetadataTx = pure setMetadata

setMetadata :: Metadata -> Q.TxE QErr ()
setMetadata metadata =
  Q.unitQE defaultTxErrorHandler
    [Q.sql|
     INSERT INTO hdb_catalog.hdb_metadata
       (id, metadata) VALUES (1, $1::json)
     ON CONFLICT (id) DO UPDATE SET metadata = $1::json
    |] (Identity $ Q.AltJ metadata) True

instance MonadMetadataManage m => MonadMetadataManage (ReaderT r m) where
  updateMetadataTx = lift updateMetadataTx

instance MonadMetadataManage m => MonadMetadataManage (ExceptT e m) where
  updateMetadataTx = lift updateMetadataTx

instance MonadMetadataManage m => MonadMetadataManage (Tracing.TraceT m) where
  updateMetadataTx = lift updateMetadataTx

class (Monad m) => MonadMetadata m where
  -- | Fetch @'Metadata' from storage. If no metadata stored, return @'emptyMetadata'.
  fetchMetadata :: m Metadata

  -- | Update the given @'Metadata' in the storage
  updateMetadata :: Metadata -> m ()

  -- | Run any database transaction in metadata db storage
  -- runTxInMetadataStorage :: Q.TxE QErr a -> m a

instance MonadMetadata m => MonadMetadata (ReaderT r m) where
  fetchMetadata = lift fetchMetadata
  updateMetadata = lift . updateMetadata
  -- runTxInMetadataStorage = lift . runTxInMetadataStorage

instance MonadMetadata m => MonadMetadata (StateT s m) where
  fetchMetadata = lift fetchMetadata
  updateMetadata = lift . updateMetadata
  -- runTxInMetadataStorage = lift . runTxInMetadataStorage

instance MonadMetadata m => MonadMetadata (Tracing.TraceT m) where
  fetchMetadata = lift fetchMetadata
  updateMetadata = lift . updateMetadata
  -- runTxInMetadataStorage = lift . runTxInMetadataStorage
