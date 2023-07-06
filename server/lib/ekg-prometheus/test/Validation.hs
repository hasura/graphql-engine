{-# LANGUAGE OverloadedStrings #-}

module Validation
  ( tests,
  )
where

import System.Metrics.Prometheus.Validation
  ( isValidHelpText,
    isValidLabelValue,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

-- | Basic tests of the valdiation functions.
tests :: Spec
tests =
  describe "Validation functions" $ do
    describe "`isValidHelpText`" $ do
      it "accepts empty strings" $
        isValidHelpText "" `shouldBe` True
      it "accepts escaped backslashes and newlines" $
        isValidHelpText "\\\\,\\n" `shouldBe` True
      it "rejects unescaped backslashes" $ do
        isValidHelpText "\\" `shouldBe` False
        isValidHelpText "\\t" `shouldBe` False
      it "rejects unescaped newlines" $
        isValidHelpText "\n" `shouldBe` False
    describe "`isValidLabelValue`" $ do
      it "accepts empty strings" $
        isValidLabelValue "" `shouldBe` True
      it "accepts escaped backslashes, newlines, and double-quotes" $
        isValidLabelValue "\\\\,\\n,\\\"" `shouldBe` True
      it "rejects unescaped backslashes" $ do
        isValidLabelValue "\\" `shouldBe` False
        isValidLabelValue "\\t" `shouldBe` False
      it "rejects unescaped newlines" $
        isValidLabelValue "\n" `shouldBe` False
      it "rejects unescaped double-quotes" $
        isValidLabelValue "\"" `shouldBe` False
