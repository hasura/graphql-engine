-- A module for representing encoded json
-- and efficient operations to construct them

module Hasura.EncJSON
  ( EncJSON
  , encJFromBuilder
  , encJToLBS
  , encJToBS
  , encJFromJValue
  , encJFromChar
  , encJFromText
  , encJFromBS
  , encJFromLBS
  , encJFromList
  , encJFromAssocList
  ) where

import qualified Data.ByteString.Char8   as S8
import           Hasura.Prelude

import qualified Data.Aeson              as J
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Encoding      as TE
import qualified Database.PG.Query       as Q

-- encoded json
-- TODO: can be improved with gadts capturing bytestring, lazybytestring
-- and builder
newtype EncJSON
  = EncJSON { unEncJSON :: BB.Builder }
  deriving (Semigroup, Monoid, IsString)

instance Show EncJSON where show = S8.unpack . encJToBS

instance Eq EncJSON where a == b = show a == show b

instance Q.FromCol EncJSON where
  fromCol = fmap encJFromBS . Q.fromCol

encJToLBS :: EncJSON -> BL.ByteString
encJToLBS = BB.toLazyByteString . unEncJSON
{-# INLINE encJToLBS #-}

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

encJFromJValue :: J.ToJSON a => a -> EncJSON
encJFromJValue = encJFromBuilder . J.fromEncoding . J.toEncoding
{-# INLINE encJFromJValue #-}

encJFromChar :: Char -> EncJSON
encJFromChar = EncJSON . BB.charUtf8
{-# INLINE encJFromChar #-}

encJFromText :: Text -> EncJSON
encJFromText = encJFromBS . TE.encodeUtf8
{-# INLINE encJFromText #-}

encJFromList :: [EncJSON] -> EncJSON
encJFromList = \case
  []   -> "[]"
  x:xs -> encJFromChar '['
          <> x
          <> foldr go (encJFromChar ']') xs
    where go v b  = encJFromChar ',' <> v <> b

-- from association list
encJFromAssocList :: [(Text, EncJSON)] -> EncJSON
encJFromAssocList = \case
  []   -> "{}"
  x:xs -> encJFromChar '{'
          <> builder' x
          <> foldr go (encJFromChar '}') xs
  where
    go v b  = encJFromChar ',' <> builder' v <> b
    -- builds "key":value from (key,value)
    builder' (t, v) =
      encJFromChar '"' <> encJFromText t <> encJFromText "\":" <> v
