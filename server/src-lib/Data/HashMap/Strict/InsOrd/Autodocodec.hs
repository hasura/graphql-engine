module Data.HashMap.Strict.InsOrd.Autodocodec
  ( sortedElemsCodec,
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
sortedElemsCodecWith valueCodec keyForElem = bimapCodec dec enc $ listCodec valueCodec
  where
    dec xs =
      let dupKeys = duplicates $ map keyForElem xs
       in if null dupKeys
            then Right $ oMapFromL keyForElem xs
            else Left $ T.unpack $ errMsg <> T.commaSeparated dupKeys

    enc = sortOn keyForElem . elems

    errMsg = case codecName valueCodec of
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
