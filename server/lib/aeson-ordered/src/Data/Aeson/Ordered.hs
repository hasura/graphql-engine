{-# LANGUAGE CPP #-}

-- | A version of aeson that parses with key order preserved.
--
-- Copyright:
--              (c) 2022 Hasura Inc.
--              (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
module Data.Aeson.Ordered
  ( Value (..),
    Object,
    Array,
    safeUnion,
    value,
    decode,
    Data.Aeson.Ordered.toList,
    fromList,
    asObject,
    object,
    array,
    insert,
    delete,
    adjust,
    empty,
    eitherDecode,
    Data.Aeson.Ordered.lookup,
    toOrdered,
    fromOrdered,
    fromOrderedHashMap,
    fromOrderedObject,
  )
where

import Control.Applicative ()
import Control.Lens (prism)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (AsNumber (..))
import Data.Aeson.Parser (jstring)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as A8
import Data.Bifunctor (bimap, second)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Data (Data)
import Data.Functor (($>))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Hashable (Hashable (..))
import Data.List (foldl')
import Data.Scientific (Scientific)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Copied constants from aeson

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

--------------------------------------------------------------------------------
-- Our altered type

-- | A JSON \"object\" (key\/value map). This is where this type
-- differs to the aeson package.
newtype Object = Object_ {unObject_ :: InsOrdHashMap Text Value}
  deriving stock (Data, Eq, Generic, Read, Show)
  deriving newtype (Hashable)

-- | Union the keys, ordered, in two maps, erroring on duplicates.
safeUnion :: Object -> Object -> Either String Object
safeUnion (Object_ x) (Object_ y) =
  fmap
    Object_
    ( traverse
        id
        ( InsOrdHashMap.unionWithKey
            (\k _a _b -> Left ("Duplicate key: " ++ T.unpack k))
            (fmap Right x)
            (fmap Right y)
        )
    )

-- | Empty object.
empty :: Object
empty = Object_ mempty

-- | Ordered Value from ordered hashmap
fromOrderedHashMap :: InsOrdHashMap Text Value -> Value
fromOrderedHashMap = Object . Object_

-- | Insert before the element at index i. Think of it in terms of
-- 'splitAt', which is (take k, drop k). Deletes existing key, if any.
insert :: (Int, Text) -> Value -> Object -> Object
insert (idx, key) val =
  Object_
    . InsOrdHashMap.fromList
    . uncurry (<>)
    . second ((key, val) :)
    . splitAt idx
    . InsOrdHashMap.toList
    . InsOrdHashMap.delete key
    . unObject_

-- | Lookup a key.
lookup :: Text -> Object -> Maybe Value
lookup key (Object_ omap) = InsOrdHashMap.lookup key omap

-- | Delete a key.
delete :: Text -> Object -> Object
delete key (Object_ omap) = Object_ (InsOrdHashMap.delete key omap)

adjust :: (Value -> Value) -> Text -> Object -> Object
adjust f key (Object_ omap) = Object_ (InsOrdHashMap.adjust f key omap)

-- | ToList a key.
toList :: Object -> [(Text, Value)]
toList (Object_ omap) = InsOrdHashMap.toList omap

-- | FromList a key.
fromList :: [(Text, Value)] -> Object
fromList = Object_ . InsOrdHashMap.fromList

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value. Intentionally
-- shadowing the 'Value' from the aeson package.
data Value
  = Object !Object
  | Array !Array
  | String !Text
  | Number !Scientific
  | Bool !Bool
  | Null
  deriving stock (Data, Eq, Generic, Read, Show)

instance Hashable Value where
  -- Lifted from Aeson's implementation for 'Value'.
  hashWithSalt s = \case
    (Object o) -> s `hashWithSalt` (0 :: Int) `hashWithSalt` o
    (Array a) -> foldl' hashWithSalt (s `hashWithSalt` (1 :: Int)) a
    (String str) -> s `hashWithSalt` (2 :: Int) `hashWithSalt` str
    (Number n) -> s `hashWithSalt` (3 :: Int) `hashWithSalt` n
    (Bool b) -> s `hashWithSalt` (4 :: Int) `hashWithSalt` b
    Null -> s `hashWithSalt` (5 :: Int)

-- Adapter instance for 'lens-aeson' which lets us write optics over the numeric
-- values in ordered JSON collections as if they were plain 'Scientific' types.
instance AsNumber Value where
  _Number = prism upcast downcast
    where
      upcast = Number
      downcast v = case v of
        Number n -> Right n
        _ -> Left v

-- | Value pairs to Value
object :: [(Text, Value)] -> Value
object = Object . fromList

-- | Value list to Value
array :: [Value] -> Value
array = Array . V.fromList

-- | Convert Aeson Value to Ordered Value
toOrdered :: (J.ToJSON a) => a -> Value
toOrdered v = case J.toJSON v of
  J.Object obj -> Object $ fromList $ map (bimap K.toText toOrdered) $ KM.toList obj
  J.Array arr -> Array $ V.fromList $ map toOrdered $ V.toList arr
  J.String text -> String text
  J.Number number -> Number number
  J.Bool boolean -> Bool boolean
  J.Null -> Null

-- | Convert Ordered Value to Aeson Value
fromOrdered :: Value -> J.Value
fromOrdered v = case v of
  Object obj -> J.Object $ fromOrderedObject obj
  Array arr -> J.Array $ V.fromList $ map fromOrdered $ V.toList arr
  String text -> J.String text
  Number number -> J.Number number
  Bool boolean -> J.Bool boolean
  Null -> J.Null

-- | Convert an ordered aeson object to a stock aeson object. The output type is
-- 'Data.Aeson.Object', not 'Value', to preserve the information that the value
-- is an object as opposed to some other possible JSON value.
fromOrderedObject :: Object -> J.Object
fromOrderedObject obj =
  KM.fromList $
    map (bimap K.fromText fromOrdered) $
      Data.Aeson.Ordered.toList obj

asObject :: (IsString s) => Value -> Either s Object
asObject = \case
  Object o -> Right o
  _ -> Left "expecting ordered object"

--------------------------------------------------------------------------------
-- Top-level entry points

eitherDecode :: L.ByteString -> Either String Value
eitherDecode = A.parseOnly value . L.toStrict

decode :: ByteString -> Maybe Value
decode = either (const Nothing) Just . A.parseOnly value

--------------------------------------------------------------------------------
-- Modified aeson parser

-- Copied from the aeson package.
arrayValues :: Parser Array
arrayValues = do
  skipSpace
  w <- A.peekWord8'
  if w == CLOSE_SQUARE
    then A.anyWord8 >> return V.empty
    else loop [] 1
  where
    loop acc !len = do
      v <- (value A.<?> "json list value") <* skipSpace
      ch <- A.satisfy (\w -> w == COMMA || w == CLOSE_SQUARE) A.<?> "',' or ']'"
      if ch == COMMA
        then skipSpace >> loop (v : acc) (len + 1)
        else return (V.reverse (V.fromListN len (v : acc)))
{-# INLINE arrayValues #-}

-- Copied from aeson package.
objectValues :: Parser (InsOrdHashMap Text Value)
objectValues = do
  skipSpace
  w <- A.peekWord8'
  if w == CLOSE_CURLY
    then A.anyWord8 >> return InsOrdHashMap.empty
    else loop InsOrdHashMap.empty
  where
    -- Why use acc pattern here, you may ask? because 'H.fromList' use 'unsafeInsert'
    -- and it's much faster because it's doing in place update to the 'HashMap'!
    loop acc = do
      k <- (jstring A.<?> "object key") <* skipSpace <* (A8.char ':' A.<?> "':'")
      v <- (value A.<?> "object value") <* skipSpace
      ch <- A.satisfy (\w -> w == COMMA || w == CLOSE_CURLY) A.<?> "',' or '}'"
      let acc' = InsOrdHashMap.insert k v acc
      if ch == COMMA
        then skipSpace >> loop acc'
        else pure acc'
{-# INLINE objectValues #-}

-- Copied from aeson package.
value :: Parser Value
value = do
  skipSpace
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE -> String <$> jstring
    OPEN_CURLY -> A.anyWord8 *> (Object . Object_ <$> objectValues)
    OPEN_SQUARE -> A.anyWord8 *> (Array <$> arrayValues)
    C_f -> A8.string "false" $> Bool False
    C_t -> A8.string "true" $> Bool True
    C_n -> A8.string "null" $> Null
    _
      | w >= 48 && w <= 57 || w == 45 ->
          Number <$> A8.scientific
      | otherwise -> fail "not a valid json value"
{-# INLINE value #-}

-- Copied from aeson package.

-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpace :: Parser ()
skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}
