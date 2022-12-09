module Hasura.Server.CompressionSpec (spec) where

import Data.Set qualified as Set
import Hasura.Prelude
import Hasura.Server.Compression
import Test.Hspec

spec :: Spec
spec = describe "serialized data compression" $ parallel do
  describe "getAcceptedEncodings" do
    it "detects gzip and not" do
      getAcceptedEncodings [("x", "x"), ("accept-encoding", "gzip")]
        `shouldBe` Set.fromList [Just CTGZip, identityEncoding]

      getAcceptedEncodings [("accept-encoding", "brotli, gzip;q=0.9")]
        `shouldBe` Set.fromList [Just CTGZip, identityEncoding]

      getAcceptedEncodings [("accept-encoding", "brotli")]
        `shouldBe` Set.fromList [identityEncoding]

      getAcceptedEncodings [("accept-encoding", "identity;q=0.42,brotli, gzip;q=0.9")]
        `shouldBe` Set.fromList [Just CTGZip, identityEncoding]

      getAcceptedEncodings [("accept-encoding", "identity;q=0.42,brotli, gzip;q=0")]
        `shouldBe` Set.fromList [identityEncoding]

    it "handles explicit rejection of identity" do
      getAcceptedEncodings [("accept-encoding", "identity;q=0,brotli, gzip;q=0.9")]
        `shouldBe` Set.fromList [Just CTGZip]

      -- strictly per spec this would result in a 406, but we'll likely
      -- just decide to return uncompressed (identity) higher up
      getAcceptedEncodings [("accept-encoding", "identity;q=0,brotli")]
        `shouldBe` Set.fromList []
      getAcceptedEncodings [("accept-encoding", "*;q=0,brotli")]
        `shouldBe` Set.fromList []

      getAcceptedEncodings [("accept-encoding", "gzip, *;q=0")]
        `shouldBe` Set.fromList [Just CTGZip]

    -- behaviors that might change if we decide it's worth it:
    it "arbitrary/historical behavior" do
      -- see Compression.hs for discussion
      getAcceptedEncodings [("accept-encoding", "*")]
        `shouldBe` Set.fromList [identityEncoding]
      getAcceptedEncodings []
        `shouldBe` Set.fromList [identityEncoding]
      getAcceptedEncodings [("accept-encoding", "")]
        `shouldBe` Set.fromList [identityEncoding]
