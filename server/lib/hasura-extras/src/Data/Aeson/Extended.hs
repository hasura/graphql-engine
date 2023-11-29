module Data.Aeson.Extended
  ( FromJSONKeyValue (..),
    ToJSONKeyValue (..),
    FromJSONWithContext (..),
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
  toJSONKeyValue :: a -> (Key, Value)

class FromJSONKeyValue a where
  parseJSONKeyValue :: (Key, Value) -> Parser a

instance ToJSONKeyValue Void where
  toJSONKeyValue = absurd

instance (ToJSONKeyValue a) => ToJSONKeyValue (Const a b) where
  toJSONKeyValue = toJSONKeyValue . getConst

-- | Similar to 'FromJSON', except the parser can also source data with which
-- to construct 'a' from a context 'ctx'.
--
-- This can be useful if the 'a' value contains some data that is not from the
-- current piece of JSON (the 'Value'). For example, some data from higher
-- up in the overall JSON graph, or from some system context.
class FromJSONWithContext ctx a | a -> ctx where
  parseJSONWithContext :: ctx -> Value -> Parser a

-------------------------------------------------------------------------------

-- | An optional key-value pair for encoding a JSON object.
--
-- @
-- object $ ["foo" .= 0] <> catMaybes [ "bar" .=? Nothing, "baz" .=? 2 ]
-- @
(.=?) :: (ToJSON v, KeyValue kv) => Key -> Maybe v -> Maybe kv
(.=?) k = fmap (k .=)
{-# INLINE (.=?) #-}

infixr 8 .=?

-- | Map a 'Parser' over a list, keeping the JSONPath context
mapWithJSONPath :: (a -> Parser b) -> [a] -> Parser [b]
mapWithJSONPath parser xs =
  traverse (\(idx, item) -> parser item <?> Index idx) $ zip [0 ..] xs

encodeToStrictText :: (ToJSON a) => a -> Text
encodeToStrictText = toStrict . toLazyText . encodeToTextBuilder
