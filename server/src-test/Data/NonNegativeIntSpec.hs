module Data.NonNegativeIntSpec (spec) where
-- | basic tests on NonNegativeIntType

import           Data.Aeson              (decode)              
import           Hasura.RQL.Types.Common (NonNegativeInt, mkNonNegativeInt)
import           Prelude
import           Test.Hspec              (Spec, describe, it, shouldBe, shouldNotBe)

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

        it "JSON accepts only non negative integers" $ do
            -- use fromJSON tests here
            -- fromJSON "{\"NonNegativeInt\": 23}" `shouldBe` (unsafeNonNegativeInt 23)
            -- fromJSON (-23 :: NonNegativeInt) `shouldNotBe` Number (-23) 
            decode "23" `shouldBe` Just (23 :: NonNegativeInt)
            decode "-23" `shouldNotBe` Just (-23 :: NonNegativeInt)
