{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "avoid Control.Concurrent.threadDelay" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Store
  ( tests,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Metrics.Prometheus.Histogram (HistogramSample (..))
import qualified System.Metrics.Prometheus.Internal.Sample as Sample
import System.Metrics.Prometheus.Internal.Store
import Test.HUnit (assertEqual)
import Test.Hspec

tests :: Spec
tests =
  describe "The internal Store interface" $ do
    it "passes a smoke test" test_smokeTest
    it "throws exceptions on invalid input" test_validation
    describe
      "Permanent and removable metrics"
      test_permanentAndRemovableMetrics
    test_uncheckedDynamicGroups

-- | A test that simply runs functions from the interface to make sure they
-- don't throw errors or never return, that is, that they don't evaluate to
-- bottom.
test_smokeTest :: IO ()
test_smokeTest = do
  result <- race (threadDelay 1000000) smokeTest
  assertEqual "Smoke test took too long" result (Right ())

smokeTest :: IO ()
smokeTest = do
  store <- newStore

  let counterIdentifier = Identifier "ccounter" mempty
      gaugeIdentifier = Identifier "cgauge" mempty
      histogramIdentifier = Identifier "chistogram" mempty
  !_ <- createCounter counterIdentifier "" store
  !_ <- createGauge gaugeIdentifier "" store
  !_ <- createHistogram [] histogramIdentifier "" store

  let registration :: Text -> Registration
      registration prefix =
        mconcat
          [ registerCounter (Identifier (prefix <> "counter") mempty) "" (pure 0),
            registerGauge (Identifier (prefix <> "gauge") mempty) "" (pure 0),
            registerHistogram (Identifier (prefix <> "histogram") mempty) "" (pure emptyHistogramSample),
            flip registerGroup (pure ()) $
              Sample.fromList
                [ (prefix <> "groupMetric", HashMap.singleton (prefix <> "counter") mempty, "", const (Counter 0)),
                  (prefix <> "groupMetric", HashMap.singleton (prefix <> "gauge") mempty, "", const (Gauge 0))
                ],
            flip registerUncheckedDynamicGroup (pure ()) $
              Map.fromList
                [ (prefix <> "uncheckedGroupMetric", ("", const mempty))
                ]
          ]

  registerPermanently store $ registration "p"
  deregistrationHandle1 <- registerRemovably store $ registration "r"
  deregistrationHandle2 <- registerRemovablyCatch store $ registration "rc"

  !_ <- sampleAll store

  deregistrationHandle1
  case deregistrationHandle2 of
    Left _ -> pure ()
    Right deregister -> deregister

emptyHistogramSample :: HistogramSample
emptyHistogramSample =
  HistogramSample
    { histBuckets = Map.empty,
      histSum = 0,
      histCount = 0
    }

-- | Basic test of the store's input validation.
test_validation :: IO ()
test_validation = do
  let registerSomething :: Store -> Text -> Text -> Text -> IO ()
      registerSomething store metricName helpText labelName =
        void $
          registerRemovably store $
            let identifier =
                  Identifier metricName (HashMap.singleton labelName "labelValue")
             in registerCounter identifier helpText (pure 0)
  store <- newStore

  registerSomething store "validMetricName" "validHelpText" "validLabelName"
  -- should not throw an exception

  registerSomething store "0invalidMetricName" "validHelpText" "validLabelName"
    `shouldThrow` \case ValidationError (InvalidMetricName _) -> True; _ -> False

  registerSomething store "validMetricName" "invalidHelpText\\t" "validLabelName"
    `shouldThrow` \case ValidationError (InvalidHelpText _) -> True; _ -> False

  registerSomething store "validMetricName" "validHelpText" "\"invalidLabelName"
    `shouldThrow` \case ValidationError (InvalidLabelName _) -> True; _ -> False

test_permanentAndRemovableMetrics :: Spec
test_permanentAndRemovableMetrics = do
  let registerThing =
        registerCounter (Identifier "name" HashMap.empty) "help" (pure 0)
  it "Permanent metrics conflict with themselves" $ do
    let permanentPermanent = do
          store <- newStore
          registerPermanently store registerThing
          registerPermanently store registerThing
    permanentPermanent
      `shouldThrow` \case MetricIdentifierAlreadyUsed {} -> True; _ -> False

  it "Permanent metrics conflict with removable metrics" $ do
    let permanentRemovable = do
          store <- newStore
          registerPermanently store registerThing
          _ <- registerRemovably store registerThing
          pure ()
    permanentRemovable
      `shouldThrow` \case MetricIdentifierAlreadyUsed {} -> True; _ -> False

    do
      -- should not throw an exception
      store <- newStore
      registerPermanently store registerThing
      Left (MetricIdentifierAlreadyUsed {}) <-
        registerRemovablyCatch store registerThing
      pure ()

    let removablePermanent = do
          store <- newStore
          _ <- registerRemovably store registerThing
          registerPermanently store registerThing
    removablePermanent
      `shouldThrow` \case MetricIdentifierAlreadyUsed {} -> True; _ -> False

  it "Removable metrics do not conflict" $ do
    -- should not throw an exception
    store <- newStore
    _ <-
      registerRemovably store $
        registerCounter (Identifier "name" HashMap.empty) "help" (pure 0)
    _ <-
      registerRemovably store $
        registerCounter (Identifier "name" HashMap.empty) "help" (pure 0)
    pure ()

test_uncheckedDynamicGroups :: Spec
test_uncheckedDynamicGroups =
  describe "Unchecked dynamic metric groups" $ do
    let labelSet1 = HashMap.singleton "label1" "labelVal1"
        labelSet2 = HashMap.singleton "label2" "labelVal2"
        dynamicGroup =
          Map.fromList
            [ ( "name1",
                ( "help1",
                  \() -> Map.singleton labelSet1 (Gauge 1)
                )
              ),
              ( "name2",
                ( "help2",
                  \() -> Map.singleton labelSet2 (Gauge 2)
                )
              )
            ]
        expectedSample =
          Map.fromList
            [ ("name1", ("help1", Map.singleton labelSet1 (Gauge 1))),
              ("name2", ("help2", Map.singleton labelSet2 (Gauge 2)))
            ]

    it "Register and deregister their metrics (smoke test)" $ do
      store <- newStore

      deregistrationHandle <-
        registerRemovably store $
          registerUncheckedDynamicGroup dynamicGroup (pure ())

      sample1 <- sampleAll store
      sample1 `shouldBe` expectedSample

      deregistrationHandle

      sample2 <- sampleAll store
      sample2 `shouldBe` Map.empty
