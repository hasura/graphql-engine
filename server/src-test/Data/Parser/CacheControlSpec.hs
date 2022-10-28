module Data.Parser.CacheControlSpec (spec) where

import Data.Parser.CacheControl (CacheControl)
import Data.Parser.CacheControl qualified as CCP
import Data.Text qualified as T
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMaxAge" do
    it "successfully parses max-age=5" $ do
      let header = "public, must-revalidate, max-age=5, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Right 5

    it "successfully parses s-maxage=5" $ do
      let header = "public, must-revalidate, s-maxage=5, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Right 5

    it "doesn't have max-age; fails parsing max-age" $ do
      let header = "public, must-revalidate, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Left "could not find max-age/s-maxage"

    it "max-age value is wrong; fails parsing max-age" $ do
      let header = "public, max-age=\"abcd\" must-revalidate, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Left "could not parse max-age/s-maxage value"

  describe "noCacheExists" $ do
    testExistsFn CCP.noCacheExists True "public, no-cache, must-revalidate"
    testExistsFn CCP.noCacheExists False "public, must-revalidate"
  describe "noStoreExists" $ do
    testExistsFn CCP.noStoreExists True "public, no-store, must-revalidate"
    testExistsFn CCP.noStoreExists False "public, must-revalidate"
  describe "mustRevalidateExists" $ do
    testExistsFn CCP.mustRevalidateExists True "public, no-store, must-revalidate"
    testExistsFn CCP.mustRevalidateExists False "public, no-store"

testExistsFn :: (CacheControl -> Bool) -> Bool -> Text -> Spec
testExistsFn existsFn expected headerValue =
  it ("returns " ++ show expected ++ " for '" ++ T.unpack headerValue ++ "'") do
    (existsFn <$> CCP.parseCacheControl headerValue) `shouldBe` Right expected
