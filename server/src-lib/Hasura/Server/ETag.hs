-- | ETag (entitity tag) response header.
module Hasura.Server.ETag
  ( ETag
  , unETag
  , eTagHeader
  , generateETag
  , shouldSendResponse
  )
where

import           Hasura.Prelude

import           Data.Aeson
import           Hasura.Server.Utils  (getRequestHeader)

import qualified Crypto.Hash          as CH
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Network.HTTP.Types   as N


eTagHeader :: T.Text
eTagHeader = "ETag"

newtype ETag = ETag {unETag :: T.Text}
  deriving (Show, Eq, ToJSON)

generateETag :: BL.ByteString -> ETag
generateETag bytes = ETag $ T.pack $ quoteString <> show eTagHash <> quoteString
  where
    -- Blake2b is fasther than SHA-256
    eTagHash :: CH.Digest CH.Blake2b_256 = CH.hash $ BL.toStrict bytes
    quoteString = ['"']

shouldSendResponse :: N.RequestHeaders -> ETag -> Bool
shouldSendResponse reqHeaders eTag =
  case getRequestHeader ifNoneMatchHeader reqHeaders of
    -- Response is sent to client only if 'If-None-Match' header value
    -- is not matched with current ETag value
    Just reqETagBytes -> reqETagBytes /= TE.encodeUtf8 (unETag eTag)
    Nothing           -> True
  where
    ifNoneMatchHeader = "If-None-Match"
