module Data.Aeson.Extended
  ( FromJSONKeyValue (..),
    ToJSONKeyValue (..),
    mapWithJSONPath,
    encodeToStrictText,
    (.=?),

    -- * Re-exports
    module Data.Aeson,
  )
where

-------------------------------------------------------------------------------

import Data.Aeson
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types (JSONPathElement (..), Parser)
import Data.Functor.Const (getConst)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Hasura.Prelude

-------------------------------------------------------------------------------

class ToJSONKeyValue a where
  toJSONKeyValue :: a -> (Text, Value)

class FromJSONKeyValue a where
  parseJSONKeyValue :: (Text, Value) -> Parser a

instance ToJSONKeyValue Void where
  toJSONKeyValue = absurd

instance ToJSONKeyValue a => ToJSONKeyValue (Const a b) where
  toJSONKeyValue = toJSONKeyValue . getConst

-------------------------------------------------------------------------------

-- | An optional key-value pair for encoding a JSON object.
--
-- @
-- object $ ["foo" .= 0] <> catMaybes [ "bar" .=? Nothing, "baz" .=? 2 ]
-- @
(.=?) :: (ToJSON v, KeyValue kv) => Text -> Maybe v -> Maybe kv
(.=?) k = fmap (k .=)
{-# INLINE (.=?) #-}

infixr 8 .=?

-- | Map a 'Parser' over a list, keeping the JSONPath context
mapWithJSONPath :: (a -> Parser b) -> [a] -> Parser [b]
mapWithJSONPath parser xs =
  traverse (\(idx, item) -> parser item <?> Index idx) $ zip [0 ..] xs

encodeToStrictText :: ToJSON a => a -> Text
encodeToStrictText = toStrict . toLazyText . encodeToTextBuilder
