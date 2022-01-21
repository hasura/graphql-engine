-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Harness.State (State)
import Harness.Test.Feature qualified as Feature
import Test.Hspec
import Prelude

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec =
  Feature.feature
    Feature.Feature
      { Feature.backends = [],
        Feature.tests = tests
      }

--------------------------------------------------------------------------------
-- Tests

tests :: SpecWith State
tests = it "No-op" (const (shouldBe () ()))
