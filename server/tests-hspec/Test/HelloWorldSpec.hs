-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Harness.State (State)
import Harness.Test.Context qualified as Context
import Test.Hspec (SpecWith, it, shouldBe)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith State
spec = Context.run [] tests

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith State
tests _opts = it "No-op" \_ -> do
  () `shouldBe` ()
