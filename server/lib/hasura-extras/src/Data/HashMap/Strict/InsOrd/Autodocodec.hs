{-# OPTIONS_GHC -Wno-orphans #-}

module Data.HashMap.Strict.InsOrd.Autodocodec
  ( insertionOrderedElemsCodec,
    insertionOrderedElemsCodecWith,
    sortedElemsCodec,
    sortedElemsCodecWith,
  )
where

import Autodocodec (Codec (..), HasCodec (codec), JSONCodec, ValueCodec, bimapCodec, listCodec)
import Data.HashMap.Strict.InsOrd (elems)
import Data.List.Extended (duplicates)
import Data.Text qualified as T
import Data.Text.Extended qualified as T
import Hasura.Prelude

-- | Codec for ordered hash maps where the key for each element can be inferred
-- from the element value. This codec serializes the hash map as an array sorted
-- by key.
sortedElemsCodec :: (HasCodec a, Hashable k, Ord k, T.ToTxt k) => (a -> k) -> JSONCodec (InsOrdHashMap k a)
sortedElemsCodec = sortedElemsCodecWith codec

-- | Codec for ordered hash maps where the key for each element can be inferred
-- from the element value. This codec serializes the hash map as an array sorted
-- by key.
--
-- This version is useful if there is no 'HasCodec' instance for the type of the
-- hash map values. You supply a codec as an argument instead.
sortedElemsCodecWith :: (Hashable k, Ord k, T.ToTxt k) => JSONCodec a -> (a -> k) -> JSONCodec (InsOrdHashMap k a)
sortedElemsCodecWith elemCodec keyForElem = bimapCodec dec enc $ listCodec elemCodec
  where
    dec = fromListWithDuplicateCheck elemCodec keyForElem
    enc = sortOn keyForElem . elems

-- | Codec for ordered hash maps that serializes to a list. Elements are ordered
-- according to insertion order in the map. A function to map from elements to
-- key is used on deserialization.
--
-- This version is useful if there is no 'HasCodec' instance for the type of the
-- hash map values. You supply a codec as an argument instead.
insertionOrderedElemsCodec :: (Hashable k, HasCodec a, T.ToTxt k) => (a -> k) -> JSONCodec (InsOrdHashMap k a)
insertionOrderedElemsCodec = insertionOrderedElemsCodecWith codec

-- | Codec for ordered hash maps that serializes to a list. Elements are ordered
-- according to insertion order in the map. A function to map from elements to
-- key is used on deserialization.
--
-- This version is useful if there is no 'HasCodec' instance for the type of the
-- hash map values. You supply a codec as an argument instead.
insertionOrderedElemsCodecWith :: (Hashable k, T.ToTxt k) => JSONCodec a -> (a -> k) -> JSONCodec (InsOrdHashMap k a)
insertionOrderedElemsCodecWith elemCodec keyForElem = bimapCodec dec enc $ listCodec elemCodec
  where
    dec = fromListWithDuplicateCheck elemCodec keyForElem
    enc = elems

fromListWithDuplicateCheck :: (Hashable k, T.ToTxt k) => JSONCodec a -> (a -> k) -> [a] -> Either String (InsOrdHashMap k a)
fromListWithDuplicateCheck elemCodec keyForElem xs =
  let dupKeys = duplicates $ map keyForElem xs
   in if null dupKeys
        then Right $ oMapFromL keyForElem xs
        else Left $ T.unpack $ errMsg <> T.commaSeparated dupKeys
  where
    errMsg = case codecName elemCodec of
      (Just t) -> "multiple " <> t <> " declarations exist: "
      Nothing -> "multiple declarations exist: "

codecName :: ValueCodec input output -> Maybe Text
codecName = \case
  NullCodec -> Nothing
  BoolCodec mname -> mname
  StringCodec mname -> mname
  NumberCodec mname _ -> mname
  ArrayOfCodec mname _ -> mname
  HashMapCodec _ -> Nothing
  MapCodec _ -> Nothing
  ValueCodec -> Nothing
  EqCodec _ _ -> Nothing
  BimapCodec _ _ c -> codecName c
  ObjectOfCodec mname _ -> mname
  EitherCodec {} -> Nothing
  CommentCodec _ c -> codecName c
  ReferenceCodec n _ -> Just n
