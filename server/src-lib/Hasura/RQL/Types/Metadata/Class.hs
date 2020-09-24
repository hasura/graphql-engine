{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Metadata.Class where

import           Hasura.Prelude
import           Hasura.RQL.Types.Metadata

import qualified Hasura.Tracing            as Tracing

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
