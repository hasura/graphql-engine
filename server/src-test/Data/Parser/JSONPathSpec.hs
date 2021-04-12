module Data.Parser.JSONPathSpec (spec) where

import           Hasura.Prelude
import           Hasura.RQL.Types     (encodeJSONPath)

import           Data.Parser.JSONPath
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Text            as T

spec :: Spec
spec = describe "encode and parse JSONPath" $ do
  it "JSONPath encoder" $
    forM_ generateTestEncodeJSONPath $ \(jsonPath, result) ->
      encodeJSONPath jsonPath `shouldBe` result

  describe "JSONPath parser" $ do

    it "Single $" $
      parseJSONPath "$" `shouldBe` (Right [] :: Either String JSONPath)

    it "Random json paths" $
      withMaxSuccess 1000 $
        forAll (resize 20 generateJSONPath) $ \jsonPath ->
          let encPath = encodeJSONPath jsonPath
              parsedJSONPathE = parseJSONPath $ T.pack encPath
          in case parsedJSONPathE of
              Left err             -> counterexample (err <> ": " <> encPath) False
              Right parsedJSONPath -> property $ parsedJSONPath == jsonPath



generateTestEncodeJSONPath :: [(JSONPath, String)]
generateTestEncodeJSONPath =
  [ ([Key "7seven", Index 0, Key "@!^@*#(!("], "$['7seven'][0]['@!^@*#(!(']")
  , ([Key "ABCD"], "$.ABCD")
  ]

generateJSONPath :: Gen JSONPath
generateJSONPath = map (either id id) <$> listOf1 genPathElementEither
  where
    genPathElementEither = do
      indexLeft <- Left <$> genIndex
      keyRight <- Right <$> genKey
      elements [indexLeft, keyRight]
    genIndex = Index <$> choose (0, 100)
    genKey = (Key . T.pack) <$> listOf1 (elements $ alphaNumerics ++ ".,!@#$%^&*_-?:;|/\"")
