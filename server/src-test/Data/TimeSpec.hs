module Data.TimeSpec (spec) where
-- | Time-related properties we care about.

import Prelude
import Data.Time.Clock.Units
import Data.Time
import Data.Aeson
import Test.Hspec

spec :: Spec
spec = do
  timeUnitsSpec
  diffTimeSpec

timeUnitsSpec :: Spec
timeUnitsSpec =
  describe "time units" $ do
    it "converts correctly" $ do
      seconds 123 `shouldBe` 123
      milliseconds 123 `shouldBe` 0.123
      microseconds 123 `shouldBe` 0.000123
      nanoseconds 123 `shouldBe` 0.000000123

    it "has a correct Read instance" $ do
      seconds (read "123") `shouldBe` 123
      milliseconds (read "123") `shouldBe` 0.123
      microseconds (read "123") `shouldBe` 0.000123
      nanoseconds (read "123") `shouldBe` 0.000000123

    it "JSON serializes as proper units" $ do
      toJSON (1 :: Seconds) `shouldBe` Number 1
      decode "1.0" `shouldBe` Just (1 :: Seconds)

    it "converts with fromUnits" $ do
      fromUnits (2 :: Minutes) `shouldBe` (120 :: NominalDiffTime)
      fromUnits (60 :: Seconds) `shouldBe` (1 :: Minutes)

diffTimeSpec :: Spec
diffTimeSpec =
  describe "DiffTime" $ do
    it "JSON serializes as seconds" $ do
    -- although we would prefer to use Seconds instead...
      toJSON (1 :: DiffTime) `shouldBe` Number 1
      decode "1.0" `shouldBe` Just (1 :: DiffTime)
