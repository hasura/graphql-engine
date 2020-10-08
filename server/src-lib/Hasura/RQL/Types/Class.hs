{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Class where

import           Hasura.Prelude
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.ScheduledTrigger

import           Data.Aeson

import qualified Hasura.Tracing                    as Tracing

class (Monad m) => MonadMetadata m where
  -- | Fetch the @'Metadata'
  fetchMetadata :: m Metadata

  -- | Update the given @'Metadata'
  updateMetadata :: Metadata -> m ()

instance MonadMetadata m => MonadMetadata (ReaderT r m) where
  fetchMetadata        = lift fetchMetadata
  updateMetadata       = lift . updateMetadata

instance MonadMetadata m => MonadMetadata (StateT s m) where
  fetchMetadata        = lift fetchMetadata
  updateMetadata       = lift . updateMetadata

instance MonadMetadata m => MonadMetadata (Tracing.TraceT m) where
  fetchMetadata        = lift fetchMetadata
  updateMetadata       = lift . updateMetadata

class (Monad m) => MonadScheduledEvents m where
  -- | Record the scheduled event
  createScheduledEvent :: CreateScheduledEvent -> m ()

  -- | Clear cron events
  dropFutureCronEvents :: TriggerName -> m ()

  -- | Record cron events
  addCronEventSeeds :: [CronEventSeed] -> m ()

  -- | Fetch cron/oneoff scheduled event invocations
  fetchInvocations :: EventId -> ScheduledEventType -> m [ScheduledEventInvocation]

instance MonadScheduledEvents m => MonadScheduledEvents (ReaderT r m) where
  createScheduledEvent = lift . createScheduledEvent
  dropFutureCronEvents = lift . dropFutureCronEvents
  addCronEventSeeds    = lift . addCronEventSeeds
  fetchInvocations a b = lift $ fetchInvocations a b

instance MonadScheduledEvents m => MonadScheduledEvents (StateT s m) where
  createScheduledEvent = lift . createScheduledEvent
  dropFutureCronEvents = lift . dropFutureCronEvents
  addCronEventSeeds    = lift . addCronEventSeeds
  fetchInvocations a b = lift $ fetchInvocations a b

instance MonadScheduledEvents m => MonadScheduledEvents (Tracing.TraceT m) where
  createScheduledEvent = lift . createScheduledEvent
  dropFutureCronEvents = lift . dropFutureCronEvents
  addCronEventSeeds    = lift . addCronEventSeeds
  fetchInvocations a b = lift $ fetchInvocations a b

class (Monad m) => MonadCatalogState m where
  -- | Retrieve the state from metadata storage catalog
  fetchCatalogState :: m CatalogState
  -- | Update the state from metadata storage catalog
  updateCatalogState :: CatalogStateType -> Value -> m ()

instance MonadCatalogState m => MonadCatalogState (ReaderT r m) where
  fetchCatalogState      = lift fetchCatalogState
  updateCatalogState a b = lift $ updateCatalogState a b

instance MonadCatalogState m => MonadCatalogState (StateT s m) where
  fetchCatalogState      = lift fetchCatalogState
  updateCatalogState a b = lift $ updateCatalogState a b

instance MonadCatalogState m => MonadCatalogState (Tracing.TraceT m) where
  fetchCatalogState      = lift fetchCatalogState
  updateCatalogState a b = lift $ updateCatalogState a b
