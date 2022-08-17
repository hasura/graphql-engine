-- | basic tests on Numeric Types
module Data.NumericSpec
  ( spec,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.Time qualified as Time
import Hasura.Prelude
import Hasura.RQL.Types.Numeric qualified as Numeric
import Hasura.RQL.Types.Numeric qualified as UUT
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, shouldBe)
import Test.Hspec qualified as Hspec
import Test.Hspec.Hedgehog (MonadTest)
import Test.Hspec.Hedgehog qualified as Hedgehog

--------------------------------------------------------------------------------

spec :: Spec
spec = Hspec.describe "Numeric Spec" $ do
  nonNegativeSpec
  nonNegativeIntSpec
  positiveIntSpec
  nonNegativeDiffTimeSpec

--------------------------------------------------------------------------------

nonNegativeSpec :: Spec
nonNegativeSpec =
  Hspec.describe "NonNegative" $ do
    Hspec.it "only validates non negative integers" $ do
      UUT.mkNonNegative @Integer 23 `shouldBe` Just (Numeric.unsafeNonNegative 23)
      UUT.mkNonNegative @Integer (-23) `shouldBe` Nothing

    Hspec.it "only validates non negative floats" $ do
      UUT.mkNonNegative @Float 23 `shouldBe` Just (Numeric.unsafeNonNegative 23)
      UUT.mkNonNegative @Float (-23) `shouldBe` Nothing

    Hspec.it "FromJSON succeeds with a positive value" $ do
      let result = Aeson.decode @(UUT.NonNegative Float) "100"
      result `shouldBe` Just (Numeric.unsafeNonNegative 100)

    Hspec.it "FromJSON fails with negative value" $ do
      let result = Aeson.decode @(UUT.NonNegative Float) "-100"
      result `shouldBe` Nothing

    Hspec.it "JSON Round Tripping" $ Hedgehog.hedgehog do
      expected :: Float <- Hedgehog.forAll $ Gen.float (Range.constant 0 9999)
      jsonTripping (UUT.unsafeNonNegative expected)

nonNegativeIntSpec :: Spec
nonNegativeIntSpec =
  Hspec.describe "NonNegativeInt" $ do
    Hspec.it "only validates non negative integers" $ do
      UUT.mkNonNegativeInt 23 `shouldBe` Just (Numeric.unsafeNonNegativeInt 23)
      UUT.mkNonNegativeInt (-23) `shouldBe` Nothing

    Hspec.it "FromJSON succeeds with a positive value" $ do
      let result = Aeson.decode @UUT.NonNegativeInt "100"
      result `shouldBe` Just (Numeric.unsafeNonNegativeInt 100)

    Hspec.it "FromJSON fails with negative value" $ do
      let result = Aeson.decode @UUT.NonNegativeInt "-100"
      result `shouldBe` Nothing

    Hspec.it "JSON Round Tripping" $ Hedgehog.hedgehog do
      expected :: Int <- Hedgehog.forAll $ Gen.integral (Range.linear 0 9999)
      jsonTripping (UUT.unsafeNonNegativeInt expected)

positiveIntSpec :: Spec
positiveIntSpec =
  Hspec.describe "PositiveInt" $ do
    Hspec.it "only validates positive integers" $ do
      UUT.mkPositiveInt 23 `shouldBe` Just (Numeric.unsafePositiveInt 23)
      UUT.mkPositiveInt (-23) `shouldBe` Nothing
      UUT.mkPositiveInt (0) `shouldBe` Nothing

    Hspec.it "FromJSON succeeds with a positive value" $ do
      let result = Aeson.decode @UUT.PositiveInt "100"
      result `shouldBe` Just (Numeric.unsafePositiveInt 100)

    Hspec.it "FromJSON fails with negative value" $ do
      let result = Aeson.decode @UUT.PositiveInt "-100"
      result `shouldBe` Nothing

    Hspec.it "FromJSON fails with zero" $ do
      let result = Aeson.decode @UUT.PositiveInt "0"
      result `shouldBe` Nothing

    Hspec.it "JSON Round Tripping" $ Hedgehog.hedgehog do
      expected :: Int <- Hedgehog.forAll $ Gen.integral (Range.linear 1 9999)
      jsonTripping (UUT.unsafePositiveInt expected)

nonNegativeDiffTimeSpec :: Spec
nonNegativeDiffTimeSpec =
  Hspec.describe "NonNegativeInt" $ do
    Hspec.it "only validates non negative DiffTimes" $ do
      UUT.mkNonNegativeDiffTime 23 `shouldBe` Just 23
      UUT.mkNonNegativeDiffTime (-23) `shouldBe` Nothing

    Hspec.it "FromJSON succeeds with a positive value" $ do
      let result = Aeson.decode @UUT.NonNegativeDiffTime "100"
      result `shouldBe` Just 100

    Hspec.it "FromJSON fails with negative value" $ do
      let result = Aeson.decode @UUT.NonNegativeDiffTime "-100"
      result `shouldBe` Nothing

    Hspec.it "JSON Round Tripping" $ Hedgehog.hedgehog do
      expected :: DiffTime <- Hedgehog.forAll $ fmap Time.secondsToDiffTime $ Gen.integral (Range.linear 0 9999)
      jsonTripping (UUT.unsafeNonNegativeDiffTime expected)

jsonTripping :: (MonadTest m, Show a, Eq a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> m ()
jsonTripping a = Hedgehog.tripping a Aeson.encode Aeson.decode
