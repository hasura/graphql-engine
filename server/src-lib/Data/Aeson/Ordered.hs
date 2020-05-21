{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | A version of aeson that parses with key order preserved.
--
-- Copyright:
--              (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.

module Data.Aeson.Ordered
  ( Value(..)
  , Object
  , Array
  , safeUnion
  , value
  , decode
  , Data.Aeson.Ordered.toList
  , fromList
  , object
  , array
  , insert
  , delete
  , empty
  , eitherDecode
  , toEncJSON
  , Data.Aeson.Ordered.lookup
  , toOrdered
  , fromOrdered
  ) where

import           Control.Applicative              hiding (empty)
import qualified Data.Aeson                       as J
import           Data.Aeson.Parser                (jstring)
import           Data.Attoparsec.ByteString       (Parser)
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Bifunctor
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as L
import           Data.Data
import           Data.Functor
import qualified Data.HashMap.Strict              as Map
import           Data.HashMap.Strict.InsOrd       (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd       as OMap
import           Data.Scientific
import qualified Data.Text                        as T
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V
import           GHC.Generics
import           Hasura.EncJSON
import           Hasura.Prelude                   hiding (empty, first, second)

--------------------------------------------------------------------------------
-- Encoding via Hasura's EncJSON

toEncJSON :: Value -> EncJSON
toEncJSON =
  \case
    Object (Object_ omap) ->
      encJFromAssocList (map (second toEncJSON) (OMap.toList omap))
    Array vec -> encJFromList (map toEncJSON (V.toList vec))
    String s -> encJFromJValue s
    Number sci -> encJFromJValue sci
    Bool b -> encJFromJValue b
    Null -> encJFromJValue J.Null

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
-- differs to the 'aeson' package.
newtype Object = Object_ { unObject_ :: InsOrdHashMap Text Value}
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Union the keys, ordered, in two maps, erroring on duplicates.
safeUnion :: Object -> Object -> Either String Object
safeUnion (Object_ x) (Object_ y) =
  fmap
    Object_
    (traverse
       id
       (OMap.unionWithKey
          (\k _a _b -> Left ("Duplicate key: " ++ T.unpack k))
          (fmap Right x)
          (fmap Right y)))

-- | Empty object.
empty :: Object
empty = Object_ mempty

-- | Insert before the element at index i. Think of it in terms of
-- 'splitAt', which is (take k, drop k). Deletes existing key, if any.
insert :: (Int, Text) -> Value -> Object -> Object
insert (idx, key) val =
  Object_ .
  OMap.fromList .
  uncurry (<>) .
  second ((key, val) :) .
  splitAt idx .
  OMap.toList .
  OMap.delete key .
  unObject_

-- | Lookup a key.
lookup :: Text -> Object -> Maybe Value
lookup key (Object_ omap) = OMap.lookup key omap

-- | Delete a key.
delete :: Text -> Object -> Object
delete key (Object_ omap) = Object_ (OMap.delete key omap)

-- | ToList a key.
toList :: Object -> [(Text,Value)]
toList (Object_ omap) = OMap.toList omap

-- | FromList a key.
fromList :: [(Text,Value)] -> Object
fromList = Object_ . OMap.fromList

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
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Value pairs to Value
object :: [(Text, Value)] -> Value
object = Object . fromList

-- | Value list to Value
array :: [Value] -> Value
array = Array . V.fromList

-- | Convert Aeson Value to Ordered Value
toOrdered :: (J.ToJSON a) => a -> Value
toOrdered v = case J.toJSON v of
  J.Object obj    -> Object $ fromList $ map (second toOrdered) $ Map.toList obj
  J.Array arr     -> Array $ V.fromList $ map toOrdered $ V.toList arr
  J.String text   -> String text
  J.Number number -> Number number
  J.Bool boolean  -> Bool boolean
  J.Null          -> Null

-- | Convert Ordered Value to Aeson Value
fromOrdered :: Value -> J.Value
fromOrdered v = case v of
  Object obj    -> J.Object $ Map.fromList $ map (second fromOrdered) $
                   Data.Aeson.Ordered.toList obj
  Array arr     -> J.Array $ V.fromList $ map fromOrdered $ V.toList arr
  String text   -> J.String text
  Number number -> J.Number number
  Bool boolean  -> J.Bool boolean
  Null          -> J.Null

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
        then skipSpace >> loop (v:acc) (len+1)
        else return (V.reverse (V.fromListN len (v:acc)))
{-# INLINE arrayValues #-}

-- Copied from aeson package.
objectValues :: Parser (InsOrdHashMap Text Value)
objectValues = do
  skipSpace
  w <- A.peekWord8'
  if w == CLOSE_CURLY
    then A.anyWord8 >> return OMap.empty
    else loop OMap.empty
 where
  -- Why use acc pattern here, you may ask? because 'H.fromList' use 'unsafeInsert'
  -- and it's much faster because it's doing in place update to the 'HashMap'!
  loop acc = do
    k <- (jstring A.<?> "object key") <* skipSpace <* (A8.char ':' A.<?> "':'")
    v <- (value A.<?> "object value") <* skipSpace
    ch <- A.satisfy (\w -> w == COMMA || w == CLOSE_CURLY) A.<?> "',' or '}'"
    let acc' = OMap.insert k v acc
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
    DOUBLE_QUOTE  -> String <$> jstring
    OPEN_CURLY    -> A.anyWord8 *> (Object . Object_ <$> objectValues)
    OPEN_SQUARE   -> A.anyWord8 *> (Array <$> arrayValues)
    C_f           -> A8.string "false" $> Bool False
    C_t           -> A8.string "true" $> Bool True
    C_n           -> A8.string "null" $> Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> Number <$> A8.scientific
      | otherwise -> fail "not a valid json value"
{-# INLINE value #-}

-- Copied from aeson package.
-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpace :: Parser ()
skipSpace = A.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}
