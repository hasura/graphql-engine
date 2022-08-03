-- | A starting point feature test.
module Test.HelloWorldSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Harness.Test.Context qualified as Context
import Harness.TestEnvironment (TestEnvironment (..))
import Test.Hspec (SpecWith, describe, it, shouldBe)

--------------------------------------------------------------------------------
-- Preamble

spec :: SpecWith TestEnvironment
spec =
  Context.run
    ( NE.fromList
        [ Context.context (Context.Backend Context.Postgres)
        ]
    )
    tests

--------------------------------------------------------------------------------
-- Tests

tests :: Context.Options -> SpecWith TestEnvironment
tests _opts =
  describe "HelloWorld" do
    it "No-op" \te -> do
      logger te "woop\n"
      () `shouldBe` ()
