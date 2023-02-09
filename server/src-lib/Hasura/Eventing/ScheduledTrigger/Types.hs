{-# LANGUAGE TemplateHaskell #-}

module Hasura.Eventing.ScheduledTrigger.Types
  ( CronTriggerStats (CronTriggerStats, _ctsMaxScheduledTime, _ctsName),
    FetchedCronTriggerStats (..),
    FetchedCronTriggerStatsLogger,
    RetryContext (RetryContext, _rctxConf),
    ScheduledEventOp (..),
    ScheduledEventWebhookPayload (ScheduledEventWebhookPayload, sewpName, sewpScheduledTime, sewpRequestTransform, sewpResponseTransform),
    ScheduledTriggerInternalErr (ScheduledTriggerInternalErr),
    CronEventsCount (..),
    OneOffScheduledEventsCount (..),
    FetchedScheduledEventsStats (..),
    FetchedScheduledEventsStatsLogger,
  )
where

import Control.FoldDebounce qualified as FDebounce
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Time.Clock
import Hasura.Base.Error
import Hasura.Eventing.HTTP
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform (MetadataResponseTransform, RequestTransform)
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.ScheduledTrigger

newtype ScheduledTriggerInternalErr
  = ScheduledTriggerInternalErr QErr
  deriving (Eq)

instance L.ToEngineLog ScheduledTriggerInternalErr L.Hasura where
  toEngineLog (ScheduledTriggerInternalErr qerr) =
    (L.LevelError, L.scheduledTriggerLogType, J.toJSON qerr)

data CronTriggerStats = CronTriggerStats
  { _ctsName :: !TriggerName,
    _ctsUpcomingEventsCount :: !Int,
    _ctsMaxScheduledTime :: !UTCTime
  }
  deriving (Eq)

$(J.deriveToJSON hasuraJSON ''CronTriggerStats)

data FetchedCronTriggerStats = FetchedCronTriggerStats
  { _fctsCronTriggers :: [CronTriggerStats],
    _fctsNumFetches :: Int
  }
  deriving (Eq)

$(J.deriveToJSON hasuraJSON ''FetchedCronTriggerStats)

instance L.ToEngineLog FetchedCronTriggerStats L.Hasura where
  toEngineLog stats =
    (L.LevelInfo, L.cronEventGeneratorProcessType, J.toJSON stats)

instance Semigroup FetchedCronTriggerStats where
  (FetchedCronTriggerStats lTriggers lFetches) <> (FetchedCronTriggerStats rTriggers rFetches) =
    FetchedCronTriggerStats (lTriggers <> rTriggers) (lFetches + rFetches)

instance Monoid FetchedCronTriggerStats where
  mempty = FetchedCronTriggerStats mempty 0

type FetchedCronTriggerStatsLogger = FDebounce.Trigger FetchedCronTriggerStats FetchedCronTriggerStats

data RetryContext = RetryContext
  { _rctxTries :: !Int,
    _rctxConf :: !STRetryConf
  }
  deriving (Eq)

data ScheduledEventWebhookPayload = ScheduledEventWebhookPayload
  { sewpId :: !EventId,
    sewpName :: !(Maybe TriggerName),
    sewpScheduledTime :: !UTCTime,
    sewpPayload :: !J.Value,
    sewpComment :: !(Maybe Text),
    -- | sewpCreatedAt is the time at which the event was created,
    -- In case of one-off scheduled events, it's the time at which
    -- the user created the event and in case of cron triggers, the
    -- graphql-engine generator, generates the cron events, the
    -- `created_at` is just an implementation detail, so we
    -- don't send it
    sewpCreatedAt :: !(Maybe UTCTime),
    sewpRequestTransform :: !(Maybe RequestTransform),
    sewpResponseTransform :: !(Maybe MetadataResponseTransform)
  }
  deriving (Show, Eq)

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''ScheduledEventWebhookPayload)

data ScheduledEventOp
  = SEOpRetry !UTCTime
  | SEOpStatus !ScheduledEventStatus
  deriving (Show, Eq)

newtype CronEventsCount = CronEventsCount {unCronEventsCount :: Int}
  deriving (Eq, Show, J.ToJSON, J.FromJSON, Num)

newtype OneOffScheduledEventsCount = OneOffScheduledEventsCount {unOneOffScheduledEventsCount :: Int}
  deriving (Eq, Show, J.ToJSON, J.FromJSON, Num)

-- | Statistics of scheduled events fetched within a timeframe
data FetchedScheduledEventsStats = FetchedScheduledEventsStats
  { _fsesNumCronEventsFetched :: CronEventsCount,
    _fsesNumOneOffScheduledEventsFetched :: OneOffScheduledEventsCount,
    _fsesNumFetches :: Int
  }
  deriving (Eq, Show)

$(J.deriveToJSON hasuraJSON ''FetchedScheduledEventsStats)

instance L.ToEngineLog FetchedScheduledEventsStats L.Hasura where
  toEngineLog stats =
    (L.LevelInfo, L.scheduledTriggerProcessLogType, J.toJSON stats)

instance Semigroup FetchedScheduledEventsStats where
  (FetchedScheduledEventsStats lCron lOneOff lFetches) <> (FetchedScheduledEventsStats rCron rOneOff rFetches) =
    FetchedScheduledEventsStats (lCron + rCron) (lOneOff + rOneOff) (lFetches + rFetches)

instance Monoid FetchedScheduledEventsStats where
  mempty = FetchedScheduledEventsStats (CronEventsCount 0) (OneOffScheduledEventsCount 0) 0

type FetchedScheduledEventsStatsLogger = FDebounce.Trigger FetchedScheduledEventsStats FetchedScheduledEventsStats
