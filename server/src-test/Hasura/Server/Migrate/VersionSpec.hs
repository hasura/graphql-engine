module Hasura.Server.Migrate.VersionSpec (spec) where

import Data.Either (isLeft)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType (MSSQL))
import Hasura.Server.Migrate.Version (MetadataCatalogVersion (..))
import Hasura.Server.Migrate.Version qualified as Version
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog

-- 'SourceCatalogVersion' is parameterized by a phantom backend type.
-- The backend type is irrelevant; this is an arbitrary choice.
type SourceCatalogVersion = Version.SourceCatalogVersion 'MSSQL

spec :: Spec
spec = do
  describe "a metadata catalog version" do
    it "can be read from an integer" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant 0 maxBound)
      let input = show expected
      let version :: Either String MetadataCatalogVersion = readEither input
      version === Right (MetadataCatalogVersion expected)

    it "cannot be read from a negative integer" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant minBound (-1))
      let input = show expected
      let version :: Either String MetadataCatalogVersion = readEither input
      assert $ isLeft version

    it "can read the number 0.8, for legacy reasons" do
      let version :: Either String MetadataCatalogVersion = readEither "0.8"
       in version `shouldBe` Right MetadataCatalogVersion08

    it "cannot read the number 0.8 in another form" do
      let version :: Either String MetadataCatalogVersion = readEither "8e-1"
       in version `shouldSatisfy` isLeft

    it "cannot read any other non-integral number" $ hedgehog do
      expected <-
        forAll
          $ Gen.filter (/= 0.8)
          $ Gen.float (Range.constantFrom 0 (-1e6) 1e6)
      let input = show expected
      let version :: Either String MetadataCatalogVersion = readEither input
      assert $ isLeft version

    it "shows as a number" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant 0 maxBound)
      let input = show expected
      let version :: MetadataCatalogVersion = read input
      show version === input

    it "shows as '0.8'" do
      show MetadataCatalogVersion08 `shouldBe` "0.8"

  describe "a source catalog version" do
    it "can be read from an integer" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant 0 maxBound)
      let input = show expected
      let version :: Either String SourceCatalogVersion = readEither input
      version === Right (Version.SourceCatalogVersion expected)

    it "shows as a number" $ hedgehog do
      expected :: Int <- forAll $ Gen.integral (Range.constant 0 maxBound)
      let input = show expected
      let version :: SourceCatalogVersion = read input
      show version === input
