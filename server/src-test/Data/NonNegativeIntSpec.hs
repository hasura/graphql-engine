-- | basic tests on NonNegativeIntType
module Data.NonNegativeIntSpec (spec) where

import Hasura.RQL.Types.Common (mkNonNegativeInt)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

spec :: Spec
spec = do
  nonNegIntSpec

nonNegIntSpec :: Spec
nonNegIntSpec =
  describe "non negative integer type" $ do
    it "only validates non negative integers" $ do
      mkNonNegativeInt 23 `shouldBe` Just 23
      mkNonNegativeInt (-23) `shouldBe` Nothing

-- TODO: add spec for fromJSON for NonNegativeInt type
