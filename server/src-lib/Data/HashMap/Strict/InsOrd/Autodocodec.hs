module Data.HashMap.Strict.InsOrd.Autodocodec
  ( sortedElemsCodec,
    sortedElemsCodecWith,
  )
where

import Autodocodec (HasCodec (codec), JSONCodec, dimapCodec, listCodec)
import Data.HashMap.Strict.InsOrd (elems)
import Hasura.Prelude

-- | Codec for ordered hash maps where the key for each element can be inferred
-- from the element value. This codec serializes the hash map as an array sorted
-- by key.
sortedElemsCodec :: (HasCodec a, Hashable k, Ord k) => (a -> k) -> JSONCodec (InsOrdHashMap k a)
sortedElemsCodec = sortedElemsCodecWith codec

-- | Codec for ordered hash maps where the key for each element can be inferred
-- from the element value. This codec serializes the hash map as an array sorted
-- by key.
--
-- This version is useful if there is no 'HasCodec' instance for the type of the
-- hash map values. You supply a codec as an argument instead.
sortedElemsCodecWith :: (Hashable k, Ord k) => JSONCodec a -> (a -> k) -> JSONCodec (InsOrdHashMap k a)
sortedElemsCodecWith valueCodec keyForElem = dimapCodec dec enc $ listCodec valueCodec
  where
    dec = oMapFromL keyForElem
    enc = sortOn keyForElem . elems
