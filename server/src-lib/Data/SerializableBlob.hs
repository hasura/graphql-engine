module Data.SerializableBlob
  ( SerializableBlob,
    fromText,
    fromBS,
    fromLBS,
    toLBS,
  )
where

import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Prelude

-- | A JSON-serializable type for either text or raw binary data, encoded with base-64.
data SerializableBlob
  = SerializableText T.Text
  | SerializableBytes B.ByteString
  deriving (Show)

instance J.ToJSON SerializableBlob where
  toJSON (SerializableText t) =
    J.toJSON t
  toJSON (SerializableBytes bs) =
    -- try and turn into utf-8
    case TE.decodeUtf8' bs of
      -- if it's valid utf-8, treat it like the Text case
      Right t -> J.toJSON t
      -- otherwise base-64 encode it instead
      Left _ -> J.toJSON ["Base64", TE.decodeUtf8 (B64.encode bs)]

instance IsString SerializableBlob where
  fromString = fromText . T.pack

fromText :: T.Text -> SerializableBlob
fromText = SerializableText

fromBS :: B.ByteString -> SerializableBlob
fromBS = SerializableBytes

fromLBS :: BL.ByteString -> SerializableBlob
fromLBS =
  fromBS . BL.toStrict

toBS :: SerializableBlob -> B.ByteString
toBS (SerializableBytes bs) = bs
toBS (SerializableText t) = TE.encodeUtf8 t

toLBS :: SerializableBlob -> BL.ByteString
toLBS = BL.fromStrict . toBS
