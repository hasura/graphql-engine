-- A module for representing encoded json
-- and efficient operations to construct them

module Hasura.EncJSON
  ( EncJSON(..)
  , encJToLBS
  , encJFromB
  , encJFromJ
  , encJFromC
  , encJFromT
  , encJFromBS
  , encJFromLBS
  , encJFromL
  , encJFromAL
  ) where

import           Hasura.Prelude

import qualified Data.Aeson           as J
import qualified Data.Binary.Builder  as BB
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding   as TE

-- encoded json
newtype EncJSON
  = EncJSON { unEncJSON :: BB.Builder }
  deriving (Semigroup, Monoid, IsString)

encJToLBS :: EncJSON -> BL.ByteString
encJToLBS = BB.toLazyByteString . unEncJSON

encJFromB :: BB.Builder -> EncJSON
encJFromB = EncJSON
{-# INLINE encJFromB #-}

encJFromBS :: B.ByteString -> EncJSON
encJFromBS = EncJSON . BB.fromByteString
{-# INLINE encJFromBS #-}

encJFromLBS :: BL.ByteString -> EncJSON
encJFromLBS = EncJSON . BB.fromLazyByteString
{-# INLINE encJFromLBS #-}

encJFromJ :: J.ToJSON a => a -> EncJSON
encJFromJ = encJFromLBS . J.encode
{-# INLINE encJFromJ #-}

encJFromC :: Char -> EncJSON
encJFromC = EncJSON . BB.putCharUtf8
{-# INLINE encJFromC #-}

encJFromT :: Text -> EncJSON
encJFromT = encJFromBS . TE.encodeUtf8
{-# INLINE encJFromT #-}

encJFromL :: [EncJSON] -> EncJSON
encJFromL = \case
  []   -> "[]"
  x:xs -> encJFromC '['
          <> x
          <> foldr go (encJFromC ']') xs
    where go v b  = encJFromC ',' <> v <> b

-- from association list
encJFromAL :: [(Text, EncJSON)] -> EncJSON
encJFromAL = \case
  []   -> "{}"
  x:xs -> encJFromC '{'
          <> builder' x
          <> foldr go (encJFromC '}') xs
  where
    go v b  = encJFromC ',' <> builder' v <> b
    -- builds "key":value from (key,value)
    builder' (t, v) =
      encJFromC '"' <> encJFromT t <> encJFromT "\":" <> v
