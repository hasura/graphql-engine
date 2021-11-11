module Data.Aeson.Extended
  ( module J,
    encodeToStrictText,
    ToJSONKeyValue (..),
    FromJSONKeyValue (..),
    mapWithJSONPath,
  )
where

import Data.Aeson as J
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types (JSONPathElement (..), Parser)
import Data.Functor.Const
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Hasura.Prelude

encodeToStrictText :: (ToJSON a) => a -> Text
encodeToStrictText = toStrict . toLazyText . encodeToTextBuilder

class ToJSONKeyValue a where
  toJSONKeyValue :: a -> (Text, J.Value)

class FromJSONKeyValue a where
  parseJSONKeyValue :: (Text, J.Value) -> Parser a

instance ToJSONKeyValue Void where
  toJSONKeyValue = absurd

instance ToJSONKeyValue a => ToJSONKeyValue (Const a b) where
  toJSONKeyValue = toJSONKeyValue . getConst

-- | map a 'Parser' over a list, keeping the JSONPath context
mapWithJSONPath :: (a -> Parser b) -> [a] -> Parser [b]
mapWithJSONPath parser xs =
  traverse (\(idx, item) -> parser item <?> Index idx) $ zip [0 ..] xs
