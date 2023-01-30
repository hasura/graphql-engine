{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Tests for EncJSON decoding
module Hasura.EncJSONSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Database.PG.Query qualified as PG
import Hasura.EncJSON (EncJSON, encJFromLbsWithoutSoh, encJToBS, encJToLBS)
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  decodeJsonbSpec

jsonString :: BS.ByteString
jsonString = "{\"label\":true,\"another_label\":1234,\"array\":[123,123,123]}"

decodeJsonbSpec :: Spec
decodeJsonbSpec = do
  describe "jsonb - strict bytestring" $ do
    it "reads a regular JSON value that does not contain a SOH header" $ do
      let input = jsonString
          encoded = either (error . T.unpack) id $ PG.fromCol @EncJSON (Just input)
          bytes = encJToBS encoded

      -- result is same as input
      BS.length bytes `shouldBe` BS.length jsonString

      -- decoding again gives the same result
      encJToBS <$> PG.fromCol @EncJSON (Just bytes) `shouldBe` Right bytes

    it "reads a JSONB value containing a SOH header" $ do
      let input = BS.cons 1 jsonString
          encoded = either (error . T.unpack) id $ PG.fromCol @EncJSON (Just input)
          bytes = encJToBS encoded

      -- result is same as input
      BS.length bytes `shouldBe` BS.length jsonString

      -- decoding again gives the same result
      encJToBS <$> PG.fromCol @EncJSON (Just bytes) `shouldBe` Right bytes

    it "returns null when passed Nothing" $ do
      encJToBS <$> PG.fromCol @EncJSON Nothing `shouldBe` Right "null"

  describe "jsonb - lazy bytestring" $ do
    it "reads a regular JSON value that does not contain a SOH header" $ do
      let input = LBS.fromStrict jsonString
          encoded = encJFromLbsWithoutSoh input
          bytes = LBS.toStrict $ encJToLBS encoded

      -- result is same as input
      BS.length bytes `shouldBe` BS.length jsonString

      -- decoding again gives the same result
      encJToBS encoded `shouldBe` bytes

    it "reads a JSONB value containing a SOH header" $ do
      let input = LBS.fromStrict $ BS.cons 1 jsonString
          encoded = encJFromLbsWithoutSoh input
          bytes = encJToBS encoded

      -- result is same as input
      BS.length bytes `shouldBe` BS.length jsonString

      -- decoding again gives the same result
      encJToBS encoded `shouldBe` bytes
