{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.MySQL.TypesSpec (spec) where

import Data.Aeson (parseJSON, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)
import Hasura.Backends.MySQL.Types (ConnPoolSettings (..))
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "MySQL" do
    describe "ConnPoolSettings" do
      it "should parse idle timeout and max connections" do
        let input =
              [aesonQQ|
                { "idle_timeout": 100,
                  "max_connections": 10
                }
              |]
        let decoded = parseEither parseJSON input
        let expected = ConnPoolSettings {_cscIdleTimeout = 100, _cscMaxConnections = 10}
        decoded `shouldBe` Right expected

      it "should round-trip" do
        let expected = ConnPoolSettings {_cscIdleTimeout = 100, _cscMaxConnections = 10}
        let actual = parseEither parseJSON $ toJSON expected
        actual `shouldBe` Right expected
