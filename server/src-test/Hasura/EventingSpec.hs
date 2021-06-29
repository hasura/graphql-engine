-- | Tests for stuff under Hasura.Eventing hierarchy
module Hasura.EventingSpec (spec) where

import           Hasura.Eventing.ScheduledTrigger
import           Hasura.Prelude

import           Data.Time.Clock
import           System.Cron.Parser
import           Test.Hspec

spec :: Spec
spec = do
  scheduleTriggersSpec

scheduleTriggersSpec :: Spec
scheduleTriggersSpec = do
  -- https://hasura.io/docs/latest/graphql/core/scheduled-triggers/create-cron-trigger.html
  --
  -- FYI this is quite helpful for experimenting with cron expressions:
  -- https://crontab.guru/
  describe "cron" $ do
    it "calculates future events sanely" $ do
      cronTest "* * * * *"
        ["2021-04-20 16:20:00 UTC",
         "2021-04-20 16:21:00 UTC",
         "2021-04-20 16:22:00 UTC"]

      cronTest "5 0 * 8 *"    -- “At 00:05 in August.”
        ["2021-08-01 00:05:00 UTC",
         "2021-08-02 00:05:00 UTC",
         "2021-08-03 00:05:00 UTC"]

      cronTest "15 14 1 * *"  -- “At 14:15 on day-of-month 1.”
        ["2021-05-01 14:15:00 UTC",
         "2021-06-01 14:15:00 UTC",
         "2021-07-01 14:15:00 UTC"]

      cronTest "0 22 * * 1-5" -- “At 22:00 on every day-of-week from Monday through Friday.”
        ["2021-04-20 22:00:00 UTC",
         "2021-04-21 22:00:00 UTC",
         "2021-04-22 22:00:00 UTC"]
  where
    -- A few unit tests for schedule projection into the future, from an
    -- arbitrary time:
    now = read "2021-04-20 16:19:19.450 UTC" :: UTCTime  -- Tuesday
    cronTest cronExpr expected = case parseCronSchedule cronExpr of
      Left e -> error $ "Fix test: " <> show e
      Right sched ->
        generateScheduleTimes now 3 sched
          `shouldBe` map read expected
