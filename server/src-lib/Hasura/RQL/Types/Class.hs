{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.Types.Class where

import           Hasura.Prelude
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Metadata
import           Hasura.RQL.Types.ScheduledTrigger

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

instance MonadScheduledEvents m => MonadScheduledEvents (ReaderT r m) where
  createScheduledEvent = lift . createScheduledEvent
  dropFutureCronEvents = lift . dropFutureCronEvents
  addCronEventSeeds    = lift . addCronEventSeeds

instance MonadScheduledEvents m => MonadScheduledEvents (StateT s m) where
  createScheduledEvent = lift . createScheduledEvent
  dropFutureCronEvents = lift . dropFutureCronEvents
  addCronEventSeeds    = lift . addCronEventSeeds

instance MonadScheduledEvents m => MonadScheduledEvents (Tracing.TraceT m) where
  createScheduledEvent = lift . createScheduledEvent
  dropFutureCronEvents = lift . dropFutureCronEvents
  addCronEventSeeds    = lift . addCronEventSeeds
