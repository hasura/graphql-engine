module Data.NonNegativeIntSpec (spec) where
-- | basic tests on NonNegativeIntType

import           Data.Aeson              (ToJSON (toJSON), Value (Number), decode)
import           Hasura.RQL.Types.Common (NonNegativeInt (..))
import           Prelude
import           Test.Hspec              (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  nonNegIntSpec

nonNegIntSpec :: Spec
nonNegIntSpec =
    describe "non negative integer type" $ do
        it "only validates non negative integers" $ do
            NonNegativeInt 23 `shouldBe` 23
            NonNegativeInt (-23) `shouldNotBe` (-23)

        it "JSON accepts only non negative integers" $ do
            toJSON (23 :: NonNegativeInt) `shouldBe` Number 23
            toJSON (-23 :: NonNegativeInt) `shouldNotBe` Number (-23)
            decode "23" `shouldBe` Just (23 :: NonNegativeInt)
            decode "-23" `shouldNotBe` Just (-23 :: NonNegativeInt)
