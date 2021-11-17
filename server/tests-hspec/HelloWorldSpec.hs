-- | A starting point feature test.
module HelloWorldSpec (spec) where

import Harness.Feature qualified as Feature
import Test.Hspec

--------------------------------------------------------------------------------
-- Preamble

spec :: Spec
spec =
  Feature.feature
    Feature.Feature
      { Feature.backends = [],
        Feature.tests = tests
      }

--------------------------------------------------------------------------------
-- Tests

tests :: Spec
tests = it "No-op" (shouldBe () ())
