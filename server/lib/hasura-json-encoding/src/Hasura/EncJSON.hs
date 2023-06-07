-- A module for representing encoded json
-- and efficient operations to construct them

module Hasura.EncJSON
  ( EncJSON,
    encJFromBuilder,
    encJToLBS,
    encJToBS,
    encJFromJValue,
    encJFromJEncoding,
    encJFromChar,
    encJFromText,
    encJFromNonEmptyText,
    encJFromBool,
    encJFromBS,
    encJFromLBS,
    encJFromList,
    encJFromAssocList,
    encJFromInsOrdHashMap,
    encJFromOrderedValue,
    encJFromBsWithoutSoh,
    encJFromLbsWithoutSoh,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.Extra qualified as BB
import Data.ByteString.Builder.Internal qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Encoding qualified as TE
import Data.Text.NonEmpty (NonEmptyText)
import Data.Text.NonEmpty qualified as NET
import Data.Vector qualified as V
import Data.Word (Word8)
import Database.PG.Query qualified as PG
import Hasura.Prelude

newtype EncJSON = EncJSON {unEncJSON :: BB.Builder}

-- | JSONB bytestrings start with a @SOH@ header @\x01@ and then
-- follow with a valid JSON string, therefore we should check for this
-- and remove if necessary before decoding as normal
instance PG.FromCol EncJSON where
  fromCol = \case
    Just bs -> Right $ encJFromBsWithoutSoh bs
    -- null values return a JSON null value
    Nothing -> Right $ encJFromJValue J.Null

-- | JSONB bytestrings start with a @SOH@ header @\x01@ and then
-- follow with a valid JSON string, therefore we should check for this
-- and remove if necessary before decoding as normal
encJFromBsWithoutSoh :: B.ByteString -> EncJSON
encJFromBsWithoutSoh = encJFromBS . removeSOH B.uncons

-- | JSONB bytestrings start with a @SOH@ header @\x01@ and then
-- follow with a valid JSON string, therefore we should check for this
-- and remove if necessary before decoding as normal
encJFromLbsWithoutSoh :: BL.ByteString -> EncJSON
encJFromLbsWithoutSoh = encJFromLBS . removeSOH BL.uncons

-- | JSONB bytestrings start with a @SOH@ header @\x01@ and then
-- follow with a valid JSON string, therefore we should check for this
-- and remove if necessary before decoding as normal
removeSOH :: (bs -> Maybe (Word8, bs)) -> bs -> bs
removeSOH uncons bs =
  case uncons bs of
    Just (bsHead, bsTail) ->
      if bsHead == 1
        then bsTail
        else bs
    Nothing -> bs

-- NB: this is somewhat wasteful, because the design of the `FromJSON` type
-- class forces that the incoming `ByteString` value is first parsed to an
-- `aeson` `Value`. But we then immediately re-serialize it here into an
-- `EncJSON`.
instance J.FromJSON EncJSON where
  parseJSON = pure . encJFromJValue

instance J.ToJSON EncJSON where
  toJSON j =
    let parsed = J.decode' $ encJToLBS j
     in case parsed of
          Nothing -> error "EncJSON contained syntactically invalid JSON!"
          Just x -> x

-- No other instances for `EncJSON`. In particular, because:
--
-- - Having a `Semigroup` or `Monoid` instance allows constructing semantically
--   illegal values of type `EncJSON`. To drive this point home: the derived
--   `Semigroup` and `Monoid` instances always produce illegal JSON. It is
--   merely through an abuse of these APIs that legal JSON can be created.
--
-- - `IsString` would be a footgun because it's not clear what its expected
--   behavior is: does it construct serialized JSON from a serialized `String`,
--   or does it serialize a given `String` into a JSON-encoded string value?
--
-- - `Eq` would also be a footgun: does it compare two serialized values, or
--   does it compare values semantically?
--
-- - `Show`: unused.

encJToLBS :: EncJSON -> BL.ByteString
{-# INLINE encJToLBS #-}
encJToLBS = BB.toLazyByteStringWith outputAllocationStrategy mempty . unEncJSON
  where
    -- this is a modification of 'untrimmedStrategy' tuned for typical request sizes.
    -- There's no point to trimming; that's just going to create more garbage
    -- that will be collected immediately.
    outputAllocationStrategy =
      BB.customStrategy nextBuffer bufSize0 (\_ _ -> False)
      where
        -- NOTE: on cloud we see uncompressed response body sizes like:
        --   P50:   150 bytes
        --   P75:  1200
        --   P90: 17000
        --   P99: 95000
        bufSize0 = 200 -- bytes
        bufGrowthFactor = 5

        {-# INLINE nextBuffer #-}
        nextBuffer Nothing = BB.newBuffer bufSize0
        -- FYI: minSize == bufSize0, except e.g. where `ensureFree` is used (and maybe other situations)
        nextBuffer (Just (prevBuf, minSize)) =
          -- nextBufSize grows exponentially, up to defaultChunkSize; but always at least minSize
          let nextBufSize = max minSize (min BB.defaultChunkSize (BB.bufferSize prevBuf * bufGrowthFactor))
           in BB.newBuffer nextBufSize

encJToBS :: EncJSON -> B.ByteString
encJToBS = BL.toStrict . encJToLBS
{-# INLINE encJToBS #-}

encJFromBuilder :: BB.Builder -> EncJSON
encJFromBuilder = EncJSON
{-# INLINE encJFromBuilder #-}

encJFromBS :: B.ByteString -> EncJSON
encJFromBS = EncJSON . BB.byteString
{-# INLINE encJFromBS #-}

encJFromLBS :: BL.ByteString -> EncJSON
encJFromLBS = EncJSON . BB.lazyByteString
{-# INLINE encJFromLBS #-}

encJFromJValue :: (J.ToJSON a) => a -> EncJSON
encJFromJValue = encJFromBuilder . J.fromEncoding . J.toEncoding
{-# INLINE encJFromJValue #-}

encJFromJEncoding :: J.Encoding -> EncJSON
encJFromJEncoding = encJFromBuilder . J.fromEncoding
{-# INLINE encJFromJEncoding #-}

encJFromChar :: Char -> EncJSON
encJFromChar = EncJSON . BB.charUtf8
{-# INLINE encJFromChar #-}

encJFromText :: Text -> EncJSON
encJFromText = encJFromBuilder . TE.encodeUtf8Builder
{-# INLINE encJFromText #-}

encJFromNonEmptyText :: NonEmptyText -> EncJSON
encJFromNonEmptyText = encJFromBuilder . TE.encodeUtf8Builder . NET.unNonEmptyText
{-# INLINE encJFromNonEmptyText #-}

encJFromBool :: Bool -> EncJSON
encJFromBool = \case
  False -> encJFromText "false"
  True -> encJFromText "true"
{-# INLINE encJFromBool #-}

encJFromList :: [EncJSON] -> EncJSON
encJFromList =
  encJFromBuilder . \case
    [] -> "[]"
    x : xs -> "[" <> unEncJSON x <> foldr go "]" xs
      where
        go v b = "," <> unEncJSON v <> b

-- from association list
encJFromAssocList :: [(Text, EncJSON)] -> EncJSON
encJFromAssocList =
  encJFromBuilder . \case
    [] -> "{}"
    x : xs -> "{" <> builder' x <> foldr go "}" xs
      where
        go v b = "," <> builder' v <> b
        -- builds "key":value from (key,value)
        builder' (t, v) = J.fromEncoding (J.text t) <> ":" <> unEncJSON v

encJFromInsOrdHashMap :: InsOrdHashMap Text EncJSON -> EncJSON
encJFromInsOrdHashMap = encJFromAssocList . InsOrdHashMap.toList

-- | Encode a 'JO.Value' as 'EncJSON'.
encJFromOrderedValue :: JO.Value -> EncJSON
encJFromOrderedValue = \case
  JO.Object obj ->
    encJFromAssocList $ (map . second) encJFromOrderedValue $ JO.toList obj
  JO.Array vec ->
    encJFromList $ map encJFromOrderedValue $ V.toList vec
  JO.String s -> encJFromJValue s
  JO.Number sci -> encJFromJValue sci
  JO.Bool b -> encJFromJValue b
  JO.Null -> encJFromJValue J.Null
