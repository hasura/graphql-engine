module Data.TByteString
  ( TByteString,
    fromText,
    fromBS,
    fromLBS,
  )
where

import Data.Aeson qualified as J
import Data.Bool (bool)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.String
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Prelude

-- | A JSON-serializable type for either text or raw binary data, encoded with base-64.
newtype TByteString
  = TByteString (Bool, T.Text)
  deriving (Show, Eq)

instance J.ToJSON TByteString where
  toJSON (TByteString (isBase64, t)) =
    bool (J.toJSON t) (J.toJSON ["Base64", t]) isBase64

instance IsString TByteString where
  fromString = fromText . T.pack

fromText :: T.Text -> TByteString
fromText t = TByteString (False, t)

fromBS :: B.ByteString -> TByteString
fromBS bs =
  TByteString $
    -- if the bs in not utf-8 encoded, encode it to Base64
    case TE.decodeUtf8' bs of
      Left _ -> (True, TE.decodeUtf8 $ B64.encode bs)
      Right t -> (False, t)

fromLBS :: BL.ByteString -> TByteString
fromLBS =
  fromBS . BL.toStrict
