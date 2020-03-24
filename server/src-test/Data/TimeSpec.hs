{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.TimeSpec (spec) where
-- | Time-related properties we care about.

import           Data.Aeson
import           Data.Time
import           Data.Time.Clock.Units
import           Prelude
import           Test.Hspec

spec :: Spec
spec = do
  describe "absolute" $ timeUnitSpec @'Absolute
  describe "calendar" $ timeUnitSpec @'Calendar
  fromUnitsSpec
  diffTimeSpec

timeUnitSpec
  :: forall t
   . ( Eq (Duration t)
     , Num (Duration t)
     , Fractional (Duration t)
     , Show (Duration t)
     , ToJSON (Duration t)
     , FromJSON (Duration t)
     , AsPicoseconds t
     ) => Spec
timeUnitSpec = do
  it "converts correctly" $ do
    (seconds @t 123) `shouldBe` 123
    (milliseconds @t 123) `shouldBe` 0.123
    (microseconds @t 123) `shouldBe` 0.000123
    (nanoseconds @t 123) `shouldBe` 0.000000123

  it "has a correct Read instance" $ do
    (seconds @t (read "123")) `shouldBe` 123
    (milliseconds @t (read "123")) `shouldBe` 0.123
    (microseconds @t (read "123")) `shouldBe` 0.000123
    (nanoseconds @t (read "123")) `shouldBe` 0.000000123

  it "JSON serializes as proper units" $ do
    toJSON (seconds @t 1) `shouldBe` Number 1
    decode "1.0" `shouldBe` Just (seconds @t 1)

fromUnitsSpec :: Spec
fromUnitsSpec =
    describe "Converting between Absolute Duration and Calendar Duration" $ do
    it "converts absolute duration to calendar duration" $ do
      fromUnits (2 :: Minutes 'Absolute) `shouldBe` (120 :: NominalDiffTime)
      fromUnits (60 :: Seconds 'Absolute) `shouldBe` (1 :: Minutes 'Calendar)
    it "converts calendar duration to absolute duration" $ do
      fromUnits (2 :: Minutes 'Calendar) `shouldBe` (120 :: DiffTime)
      fromUnits (60 :: Seconds 'Calendar) `shouldBe` (1 :: Minutes 'Absolute)

diffTimeSpec :: Spec
diffTimeSpec =
  describe "DiffTime" $ do
    it "JSON serializes as seconds" $ do
    -- although we would prefer to use Seconds instead...
      toJSON (1 :: DiffTime) `shouldBe` Number 1
      decode "1.0" `shouldBe` Just (1 :: DiffTime)
