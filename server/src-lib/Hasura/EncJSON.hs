-- A module for representing encoded json
-- and efficient operations to construct them

module Hasura.EncJSON
  ( EncJSON,
    encJFromBuilder,
    encJToLBS,
    encJToBS,
    encJFromJValue,
    encJFromChar,
    encJFromText,
    encJFromBS,
    encJFromLBS,
    encJFromList,
    encJFromAssocList,
    encJFromInsOrdHashMap,
    encJFromOrderedValue,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Ordered qualified as JO
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Database.PG.Query qualified as Q
import Hasura.Prelude

-- encoded json
-- TODO (from master): can be improved with gadts capturing bytestring, lazybytestring
-- and builder
newtype EncJSON = EncJSON {unEncJSON :: BB.Builder}

(<.>) :: EncJSON -> EncJSON -> EncJSON
EncJSON a <.> EncJSON b = EncJSON (a <> b)

instance Show EncJSON where
  showsPrec d x =
    showParen (d > 10) $
      showString "encJFromBS " . showsPrec 11 (encJToLBS x)

instance Eq EncJSON where
  (==) = (==) `on` encJToLBS

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
encJFromText = encJFromBuilder . TE.encodeUtf8Builder
{-# INLINE encJFromText #-}

encJFromList :: [EncJSON] -> EncJSON
encJFromList = \case
  [] -> encJFromBuilder "[]"
  x : xs ->
    encJFromChar '['
      <.> x
      <.> foldr go (encJFromChar ']') xs
    where
      go v b = encJFromChar ',' <.> v <.> b

-- from association list
encJFromAssocList :: [(Text, EncJSON)] -> EncJSON
encJFromAssocList = \case
  [] -> encJFromBuilder "{}"
  x : xs ->
    encJFromChar '{'
      <.> builder' x
      <.> foldr go (encJFromChar '}') xs
  where
    go v b = encJFromChar ',' <.> builder' v <.> b
    -- builds "key":value from (key,value)
    builder' (t, v) =
      encJFromBuilder (J.fromEncoding (J.text t)) <.> encJFromBuilder ":" <.> v

encJFromInsOrdHashMap :: InsOrdHashMap Text EncJSON -> EncJSON
encJFromInsOrdHashMap = encJFromAssocList . OMap.toList

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
