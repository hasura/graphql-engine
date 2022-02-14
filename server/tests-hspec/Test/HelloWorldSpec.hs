-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Harness.State (State)
import Harness.Test.Feature qualified as Feature
import Test.Hspec (SpecWith, it, shouldBe)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec = Feature.run [] tests

--------------------------------------------------------------------------------
-- Tests

tests :: Feature.Options -> SpecWith State
tests _opts = it "No-op" \_ -> do
  () `shouldBe` ()
