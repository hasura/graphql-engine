-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment)
import Test.Hspec (SpecWith, it, shouldBe)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec = Context.run [] tests

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests _opts = it "No-op" \_ -> do
  () `shouldBe` ()
