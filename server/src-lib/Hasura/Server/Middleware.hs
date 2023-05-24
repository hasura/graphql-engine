module Hasura.Server.Middleware
  ( corsMiddleware,
  )
where

import Control.Applicative
import Data.ByteString qualified as B
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Hasura.Prelude
import Hasura.Server.Cors
import Hasura.Server.Utils
import Network.HTTP.Types qualified as HTTP
import Network.Wai

corsMiddleware :: IO CorsPolicy -> Middleware
corsMiddleware getPolicy app req sendResp = do
  let origin = getRequestHeader "Origin" $ requestHeaders req
  policy <- getPolicy
  maybe (app req sendResp) (handleCors policy) origin
  where
    handleCors policy origin = case cpConfig policy of
      CCDisabled _ -> app req sendResp
      CCAllowAll -> sendCors origin policy
      CCAllowedOrigins ds
        -- if the origin is in our cors domains, send cors headers
        | bsToTxt origin `elem` dmFqdns ds -> sendCors origin policy
        -- if current origin is part of wildcard domain list, send cors
        | inWildcardList ds (bsToTxt origin) -> sendCors origin policy
        -- otherwise don't send cors headers
        | otherwise -> app req sendResp

    sendCors :: B.ByteString -> CorsPolicy -> IO ResponseReceived
    sendCors origin policy =
      case requestMethod req of
        "OPTIONS" -> sendResp $ respondPreFlight origin policy
        _ -> app req $ sendResp . injectCorsHeaders origin policy

    respondPreFlight :: B.ByteString -> CorsPolicy -> Response
    respondPreFlight origin policy =
      setHeaders (mkPreFlightHeaders requestedHeaders)
        $ injectCorsHeaders origin policy emptyResponse

    emptyResponse = responseLBS HTTP.status204 [] ""
    requestedHeaders =
      fromMaybe ""
        $ getRequestHeader "Access-Control-Request-Headers"
        $ requestHeaders req

    injectCorsHeaders :: B.ByteString -> CorsPolicy -> Response -> Response
    injectCorsHeaders origin policy = setHeaders (mkCorsHeaders origin policy)

    mkPreFlightHeaders allowReqHdrs =
      [ ("Access-Control-Max-Age", "1728000"),
        ("Access-Control-Allow-Headers", allowReqHdrs),
        ("Content-Length", "0"),
        ("Content-Type", "text/plain charset=UTF-8")
      ]

    mkCorsHeaders origin policy =
      [ ("Access-Control-Allow-Origin", origin),
        ("Access-Control-Allow-Credentials", "true"),
        ( "Access-Control-Allow-Methods",
          B.intercalate "," $ TE.encodeUtf8 <$> cpMethods policy
        ),
        -- console requires this header to access the cache headers as HGE and console
        -- are hosted on different domains in production
        ( "Access-Control-Expose-Headers",
          B.intercalate "," $ TE.encodeUtf8 <$> cacheExposedHeaders
        )
      ]

    cacheExposedHeaders = ["X-Hasura-Query-Cache-Key", "X-Hasura-Query-Family-Cache-Key", "Warning"]
    setHeaders hdrs = mapResponseHeaders (\h -> mkRespHdrs hdrs ++ h)
    mkRespHdrs = map (\(k, v) -> (CI.mk k, v))
