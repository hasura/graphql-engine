module Data.Parser.JSONPathSpec (spec) where

import Data.Aeson (JSONPath)
import Data.Aeson.Key qualified as K
import Data.Aeson.Types (JSONPathElement (..))
import Data.Parser.JSONPath
import Data.Text qualified as T
import Hasura.Prelude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encoding a JSON path" $ do
    it "encodes a one-level path"
      $ encodeJSONPath [Key "ABCD"]
      `shouldBe` "$.ABCD"

    it "encodes a multi-level path"
      $ encodeJSONPath [Key "7seven", Index 0, Key "@!^@*#(!("]
      `shouldBe` "$[\"7seven\"][0][\"@!^@*#(!(\"]"

    it "escapes control characters and quotes"
      $ encodeJSONPath [Key "/\\ '\" \t\r\n \xfffd"]
      `shouldBe` "$[\"/\\\\ '\\\" \\t\\r\\n \xfffd\"]"

  describe "parsing a JSON path" $ do
    it "parses a single '$'"
      $ parseJSONPath "$"
      `shouldBe` Right []

    it "parses bracketed single quotes"
      $ parseJSONPath "$['foo \\' \" bar']"
      `shouldBe` Right [Key "foo ' \" bar"]

    it "parses bracketed double quotes"
      $ parseJSONPath "$[\"bar ' \\\" foo\"]"
      `shouldBe` Right [Key "bar ' \" foo"]

  describe "the round trip" $ do
    it "encodes and parses random JSON paths"
      $ withMaxSuccess 1000
      $ forAll (resize 20 generateJSONPath)
      $ \jsonPath ->
        let encPath = encodeJSONPath jsonPath
            parsedJSONPathE = parseJSONPath encPath
         in case parsedJSONPathE of
              Left err -> counterexample (T.unpack (err <> ": " <> encPath)) False
              Right parsedJSONPath -> property $ parsedJSONPath === jsonPath

generateJSONPath :: Gen JSONPath
generateJSONPath = map (either id id) <$> listOf1 genPathElementEither
  where
    genPathElementEither = do
      indexLeft <- Left <$> genIndex
      keyRight <- Right <$> genKey
      elements [indexLeft, keyRight]
    genIndex = Index <$> choose (0, 100)
    genKey = Key . K.fromText . T.pack <$> listOf1 arbitraryUnicodeChar
