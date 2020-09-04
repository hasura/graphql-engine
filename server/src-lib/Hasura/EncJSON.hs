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

import           Hasura.Prelude

import qualified Data.Aeson              as J
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Encoding      as TE
import qualified Database.PG.Query       as Q
import qualified Database.MySQL.Simple.Result as My
import qualified Database.MySQL.Base.Types as My

-- encoded json
-- TODO (from master): can be improved with gadts capturing bytestring, lazybytestring
-- and builder
newtype EncJSON
  = EncJSON { unEncJSON :: BB.Builder }
  deriving (Semigroup, Monoid, IsString)

instance Show EncJSON where
  showsPrec d x = showParen (d > 10) $
    showString "encJFromBS " . showsPrec 11 (encJToLBS x)

instance Eq EncJSON where
  (==) = (==) `on` encJToLBS

instance Q.FromCol EncJSON where
  fromCol = fmap encJFromBS . Q.fromCol

instance My.Result EncJSON where
  -- TODO this is dirty.

  -- We implement the decoding of incoming mysql data (I think these are
  -- initially received as bytestrings) into the various haskell types. That's
  -- what the Result type class is for.

  -- What you're supposed to do is use another instance, such as the one for
  -- ByteString, and do an appropriate interpretation of that result into the
  -- desired type.  But that's not possible, because the decoder for ByteString
  -- has some kind of built-in subtype checking mechanism that doesn't know
  -- about JSON.  So this dirty fix overwrites the type of an incoming column as
  -- String column types, parses them as bytestring, and then save them as
  -- EncJSON.

  -- Doing it the clean way would involve modifying mysql-simple (add Json to
  -- `okText`).

  -- Note that, at the moment of writing (4 September 2020), mysql also had to
  -- be modified to recognize the Json column type.  Although there was code to
  -- do this, the CPP code didn't seem to work correctly, so that this code was
  -- never included.
  convert f x = encJFromBS bs
    where bs = My.convert (f { My.fieldType = My.String }) x

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
