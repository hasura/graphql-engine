module Hasura.Authentication.Header
  ( filterHeaders,
    getRequestHeader,
    mkSetCookieHeaders,
    redactSensitiveHeader,
  )
where

import Control.Lens
import Data.ByteString (ByteString)
import Data.HashSet qualified as HashSet
import Hasura.Authentication.Headers
import Hasura.Prelude
import Network.HTTP.Types qualified as HTTP
import Network.Wreq qualified as Wreq

getRequestHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe ByteString
getRequestHeader hdrName hdrs = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == hdrName) hdrs

filterHeaders :: HashSet HTTP.HeaderName -> [HTTP.Header] -> [HTTP.Header]
filterHeaders list = filter (\(n, _) -> not $ n `HashSet.member` list)

redactSensitiveHeader :: HTTP.Header -> HTTP.Header
redactSensitiveHeader (headerName, value) = (headerName, if headerName `elem` sensitiveHeaders then "<REDACTED>" else value)

mkSetCookieHeaders :: Wreq.Response a -> HTTP.ResponseHeaders
mkSetCookieHeaders resp =
  map (headerName,) $ resp ^.. Wreq.responseHeader headerName
  where
    headerName = "Set-Cookie"
