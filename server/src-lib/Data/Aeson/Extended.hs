module Data.Aeson.Extended
  ( module J
  , encodeToStrictText
  , ToJSONKeyValue (..)
  , FromJSONKeyValue (..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Text    as JT
import qualified Data.Text.Lazy     as LT

import           Data.Aeson         as J
import           Data.Aeson.Types   (Parser)
import           Data.Functor.Const


encodeToStrictText :: (ToJSON a) => a -> Text
encodeToStrictText = LT.toStrict . JT.encodeToLazyText


class ToJSONKeyValue a where
  toJSONKeyValue :: a -> (Text, J.Value)

class FromJSONKeyValue a where
  parseJSONKeyValue :: (Text, J.Value) -> Parser a


instance ToJSONKeyValue Void where
  toJSONKeyValue = absurd

instance ToJSONKeyValue a => ToJSONKeyValue (Const a b) where
  toJSONKeyValue = toJSONKeyValue . getConst
