module Data.NonNegativeIntSpec (spec) where
-- | basic tests on NonNegativeIntType

import           Hasura.RQL.Types.Common (NonNegativeInt, mkNonNegativeInt)
import           Prelude
import           Test.Hspec              (Spec, describe, it, shouldBe)

evalMaybeNonNegativeInt :: Maybe NonNegativeInt -> Bool
evalMaybeNonNegativeInt x = case x of
    Just _  -> True
    Nothing -> False

spec :: Spec
spec = do
  nonNegIntSpec

nonNegIntSpec :: Spec
nonNegIntSpec =
    describe "non negative integer type" $ do
        it "only validates non negative integers" $ do
            (evalMaybeNonNegativeInt $ mkNonNegativeInt 23) `shouldBe` True
            (evalMaybeNonNegativeInt $ mkNonNegativeInt (-23)) `shouldBe` False

