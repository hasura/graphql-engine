{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the pretty printing of 'ErrorMessage' values.
module Hasura.Base.ErrorMessageSpec (spec) where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Vector qualified as Vector
import Hasura.Base.ErrorMessage
import Hasura.Base.ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import Test.Hspec

-- Orphan instance to avoid implementing `Show ErrorMessage` in production code.
instance Show ErrorMessage where
  -- convert to 'String' through the 'ToJSON' interface.
  show = unpack . decodeUtf8 . encode . toJSON

spec :: Spec
spec =
  describe "ErrorMessage" do
    it "can be concatenated" do
      let thing :: Thing Int = Thing 7
          message = "the thing is broken: " <> toErrorValue thing
       in message `shouldBe` "the thing is broken: Thing 7"

    it "builds lists" do
      let things :: [Thing Double] = [Thing 1.0, Thing 2.0, Thing 3.14159]
          message = "the things are broken: " <> toErrorValue things
       in message `shouldBe` "the things are broken: [Thing 1.0, Thing 2.0, Thing 3.14159]"

    it "builds non-empty lists" do
      let things :: NonEmpty (Thing Char) = Thing 'a' :| [Thing 'b', Thing 'c']
          message = "the things are broken: " <> toErrorValue things
       in message `shouldBe` "the things are broken: [Thing 'a', Thing 'b', Thing 'c']"

    it "builds hashsets" do
      let things :: HashSet (Thing Int) = HashSet.fromList [Thing 7, Thing 9, Thing 11]
          message = "the things are broken: " <> toErrorValue things
       in message `shouldBe` "the things are broken: [Thing 7, Thing 9, Thing 11]"

    it "provides utility functions to quote in various different ways" do
      let w = SingleQuoted 'w'
          x = DoubleQuoted 'x'
          y = BacktickQuoted 'y'
          z = Parenthesized 'z'
          message = sconcat $ "errors in " :| [toErrorValue w, ", ", toErrorValue x, ", ", toErrorValue y, ", and ", toErrorValue z]
       in message `shouldBe` "errors in 'w', \"x\", `y`, and (z)"

    it "serializes GraphQL names" do
      let name = [G.name|hello|]
          message = "error in name " <> toErrorValue name
       in message `shouldBe` "error in name 'hello'"

    it "serializes GraphQL types" do
      let name = G.TypeNamed (G.Nullability False) [G.name|entity|]
          message = "error in type " <> toErrorValue name
       in message `shouldBe` "error in type 'entity!'"

    describe "JSON value serialization" do
      it "serializes a JSON null" do
        let value = Aeson.Null
            message = "the value " <> toErrorValue value <> " is invalid"
         in message `shouldBe` "the value null is invalid"

      it "serializes a JSON array" do
        let value = Aeson.Bool False
            message = "the value " <> toErrorValue value <> " is invalid"
         in message `shouldBe` "the value false is invalid"

      it "serializes a JSON number" do
        let value = Aeson.Number 99
            message = "the value " <> toErrorValue value <> " is invalid"
         in message `shouldBe` "the value 99 is invalid"

      it "serializes a JSON string" do
        let value = Aeson.String "this is a string"
            message = "the value " <> toErrorValue value <> " is invalid"
         in message `shouldBe` "the value \"this is a string\" is invalid"

      it "serializes a JSON array" do
        let value = Aeson.Array $ Vector.fromList [Aeson.Number 1, Aeson.Bool True, Aeson.String "three"]
            message = "the value " <> toErrorValue value <> " is invalid"
         in message `shouldBe` "the value [1,true,\"three\"] is invalid"

      it "serializes a JSON object" do
        let value = Aeson.Object $ KeyMap.fromList [("hello", Aeson.String "world"), ("name", Aeson.String "Alice"), ("age", Aeson.Number 42)]
            message = "the value " <> toErrorValue value <> " is invalid"
         in message `shouldBe` "the value {\"age\":42,\"hello\":\"world\",\"name\":\"Alice\"} is invalid"

newtype Thing a = Thing a
  deriving newtype (Eq, Hashable)

instance Show a => ToErrorValue (Thing a) where
  toErrorValue (Thing x) = toErrorMessage $ "Thing " <> tshow x

newtype SingleQuoted = SingleQuoted Char

instance ToErrorValue SingleQuoted where
  toErrorValue (SingleQuoted c) = squote $ Text.singleton c

newtype DoubleQuoted = DoubleQuoted Char

instance ToErrorValue DoubleQuoted where
  toErrorValue (DoubleQuoted c) = dquote $ Text.singleton c

newtype BacktickQuoted = BacktickQuoted Char

instance ToErrorValue BacktickQuoted where
  toErrorValue (BacktickQuoted c) = bquote $ Text.singleton c

newtype Parenthesized = Parenthesized Char

instance ToErrorValue Parenthesized where
  toErrorValue (Parenthesized c) = paren $ Text.singleton c
