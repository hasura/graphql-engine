module Hasura.Server.Migrate.VersionSpec (spec) where

import Data.Either (isLeft)
import Hasura.Prelude
import Hasura.Server.Migrate.Version
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

spec :: Spec
spec =
  describe "a catalog version" do
    it "can be read from an integer" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant 0 maxBound)
      let input = show expected
      let version :: Either String CatalogVersion = readEither input
      version === Right (CatalogVersion expected)

    it "cannot be read from a negative integer" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant minBound (-1))
      let input = show expected
      let version :: Either String CatalogVersion = readEither input
      assert $ isLeft version

    it "can read the number 0.8, for legacy reasons" do
      let version :: Either String CatalogVersion = readEither "0.8"
       in version `shouldBe` Right CatalogVersion08

    it "cannot read the number 0.8 in another form" do
      let version :: Either String CatalogVersion = readEither "8e-1"
       in version `shouldSatisfy` isLeft

    it "cannot read any other non-integral number" $ hedgehog do
      expected <-
        forAll $
          Gen.filter (/= 0.8) $ Gen.float (Range.constantFrom 0 (-1e6) 1e6)
      let input = show expected
      let version :: Either String CatalogVersion = readEither input
      assert $ isLeft version
