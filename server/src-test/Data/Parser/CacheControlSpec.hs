module Data.Parser.CacheControlSpec (spec) where

import           Hasura.Prelude

import qualified Data.Parser.CacheControl as CCP

import           Test.Hspec


spec :: Spec
spec = do
  describe "successfully parse cache-control header" $ do
    it "parses max-age=5" $ do
      let header = "public, must-revalidate, max-age=5, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Right 5

    it "parses s-maxage=5" $ do
      let header = "public, must-revalidate, s-maxage=5, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Right 5

  describe "parse cache-control header fails" $ do
    it "doesn't have max-age; fails parsing max-age" $ do
      let header = "public, must-revalidate, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Left "could not find max-age/s-maxage"

    it "max-age value is wrong; fails parsing max-age" $ do
      let header = "public, max-age=\"abcd\" must-revalidate, no-transform"
      CCP.parseMaxAge @Integer header `shouldBe` Left "could not find max-age/s-maxage"
