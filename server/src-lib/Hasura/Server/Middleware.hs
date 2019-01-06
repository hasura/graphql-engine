module Hasura.Server.Middleware where

import           Data.Maybe            (fromMaybe)
import           Network.Wai

import           Hasura.Prelude
import           Hasura.Server.Logging (getRequestHeader)

import qualified Data.ByteString       as B
import qualified Data.CaseInsensitive  as CI
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Network.HTTP.Types    as H

data CorsPolicy
  = CorsPolicy
  { cpDomain  :: !T.Text
  , cpMethods :: ![T.Text]
  , cpMaxAge  :: !Int
  } deriving (Show, Eq)

mkDefaultCorsPolicy :: T.Text -> CorsPolicy
mkDefaultCorsPolicy domain =
  CorsPolicy
  { cpDomain = domain
  , cpMethods = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
  , cpMaxAge = 1728000
  }

corsMiddleware :: CorsPolicy -> Middleware
corsMiddleware policy app req sendResp =
  maybe (app req sendResp) handleCors $ getRequestHeader "Origin" req

  where
    handleCors origin
      | cpDomain policy /= "*" && origin /= TE.encodeUtf8 (cpDomain policy) = app req sendResp
      | otherwise =
        case requestMethod req of
          "OPTIONS" -> sendResp $ respondPreFlight origin
          _         -> app req $ sendResp . injectCorsHeaders origin

    respondPreFlight :: B.ByteString -> Response
    respondPreFlight origin =
      setHeaders (mkPreFlightHeaders requestedHeaders)
        $ injectCorsHeaders origin emptyResponse

    emptyResponse = responseLBS H.status204 [] ""
    requestedHeaders =
      fromMaybe "" $ getRequestHeader "Access-Control-Request-Headers" req

    injectCorsHeaders :: B.ByteString -> Response -> Response
    injectCorsHeaders origin = setHeaders (mkCorsHeaders origin)

    mkPreFlightHeaders allowReqHdrs =
      [ ("Access-Control-Max-Age", "1728000")
      , ("Access-Control-Allow-Headers", allowReqHdrs)
      , ("Content-Length", "0")
      , ("Content-Type", "text/plain charset=UTF-8")
      ]

    mkCorsHeaders origin =
      [ ("Access-Control-Allow-Origin", origin)
      , ("Access-Control-Allow-Credentials", "true")
      , ("Access-Control-Allow-Methods",
         B.intercalate "," $ TE.encodeUtf8 <$> cpMethods policy)
      ]

    setHeaders hdrs = mapResponseHeaders (\h -> mkRespHdrs hdrs ++ h)
    mkRespHdrs = map (\(k,v) -> (CI.mk k, v))
