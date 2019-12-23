{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.DDL.ScheduledTrigger
  ( ScheduledTriggerQuery(..)
  , runCreateScheduledTrigger
  , ScheduleType(..)
  , ScheduledTrigger(..)
  , formatTime'
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Time.Clock
import           Data.Time.Format
import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DDL.Schema.Cache (CacheBuildM)
import           Hasura.RQL.DDL.Utils
import           Hasura.RQL.Types.Common     (NonEmptyText)
import           Instances.TH.Lift           ()
import           Language.Haskell.TH.Syntax  as TH
import           System.Cron.Parser
import           System.Cron.Types


import qualified Data.Aeson                  as J
import qualified Data.Text                   as T
import qualified Database.PG.Query           as Q

-- aeson doesn't decode 'UTC' identifier so explicitly provide 'Z'
-- TODO: take proper timezone
formatTime' :: UTCTime -> T.Text
formatTime'= T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S Z"

data RetryConf
  = RetryConf
  { rcNumRetries  :: !Int
  , rcIntervalSec :: !Int
  , rcTimeoutSec  :: !(Maybe Int)
  , rcTolerance   :: !DiffTime
  } deriving (Show, Eq)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RetryConf)

data ScheduleType = OneOff UTCTime | Cron CronSchedule
  deriving (Show, Eq)

$(deriveJSON (defaultOptions){sumEncoding=TaggedObject "type" "value"} ''ScheduleType)

data ScheduledTrigger
  = ScheduledTrigger
  { stName     :: !T.Text
  , stWebhook  :: !T.Text
  , stSchedule :: !ScheduleType
  , stPayload  :: !(Maybe J.Value)
  }
  deriving (Show, Eq)

-- TODO :: Change stqSchedule to ScheduleType after writing TH.Lift instances

data ScheduleTypeUnstrict = UnstrictOneOff T.Text | UnstrictCron T.Text
  deriving (Show, Eq, Lift)

$(deriveJSON (defaultOptions){constructorTagModifier = drop 8,  sumEncoding=TaggedObject "type" "value"} ''ScheduleTypeUnstrict)

data ScheduledTriggerQuery
  = ScheduledTriggerQuery
  { stqName     :: !NonEmptyText
  , stqWebhook  :: !NonEmptyText
  , stqSchedule :: !ScheduleTypeUnstrict
  , stqPayload  :: !(Maybe J.Value)
  }
  deriving (Show, Eq, Lift)

instance FromJSON CronSchedule where
  parseJSON = withText "CronSchedule" $ \t ->
    either fail pure $ parseCronSchedule t

instance ToJSON CronSchedule where
  toJSON = J.String . serializeCronSchedule

instance FromJSON ScheduledTriggerQuery where
  parseJSON =
    withObject "ScheduledTriggerQuery" $ \o -> do
      stqName <- o .: "name"
      stqWebhook <- o .: "webhook"
      stqPayload <- o .:? "payload"
      stqScheduleUnstrict :: ScheduleTypeUnstrict <- o .: "schedule"
      scheduleType :: ScheduleType <-
        either fail pure $ eitherDecode' (J.encode stqScheduleUnstrict)
      stqSchedule <-
        case scheduleType of
          OneOff utcTime -> pure $ UnstrictOneOff (formatTime' utcTime)
          Cron cron      -> pure $ UnstrictCron (serializeCronSchedule cron)
      pure ScheduledTriggerQuery {..}

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''ScheduledTriggerQuery)

runCreateScheduledTrigger :: CacheBuildM m => ScheduledTriggerQuery ->  m EncJSON
runCreateScheduledTrigger ScheduledTriggerQuery{..} = do
  liftTx $  Q.unitQE defaultTxErrorHandler
         [Q.sql|
           INSERT into hdb_catalog.hdb_scheduled_trigger
                       (name, webhook, schedule, payload)
           VALUES ($1, $2, $3, $4)
         |] (stqName, stqWebhook, Q.AltJ $ toJSON stqSchedule, Q.AltJ <$> stqPayload) False
  return successMsg
