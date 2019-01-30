module Hasura.Server.Middleware where

import           Data.Maybe            (fromMaybe)
import           Network.Wai

import           Control.Applicative
import           Hasura.Prelude
import           Hasura.Server.Cors
import           Hasura.Server.Logging (getRequestHeader)
import           Hasura.Server.Utils

import qualified Data.ByteString       as B
import qualified Data.CaseInsensitive  as CI
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Network.HTTP.Types    as H
import qualified Network.URI           as URI


dropUriScheme :: URI.URI -> Text
dropUriScheme uri =
  let dom  = URI.uriRegName <$> URI.uriAuthority uri
      port = URI.uriPort <$> URI.uriAuthority uri
  in maybe (T.pack $ show uri) T.pack $ liftA2 (++) dom port

corsMiddleware :: CorsPolicy -> Middleware
corsMiddleware policy app req sendResp =
  maybe (app req sendResp) handleCors $ getRequestHeader "Origin" req

  where
    handleCors origin = do
      let mUri = URI.parseURI (T.unpack $ bsToTxt origin)
      case mUri of
        Nothing -> app req sendResp
        Just uri -> do
          let origin' = dropUriScheme uri
          case cpDomains policy of
            CDStar -> sendCors origin
            CDDomains ds
              -- if the origin is in our cors domains, send cors headers
              | origin' `elem` dFqdn ds   -> sendCors origin
              -- if current origin is part of wildcard domain list, send cors
              | inWildcardList ds origin' -> sendCors origin
              -- otherwise don't send cors headers
              | otherwise                 -> app req sendResp

    sendCors :: B.ByteString -> IO ResponseReceived
    sendCors origin =
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

    inWildcardList :: Domains -> Text -> Bool
    inWildcardList (Domains _ wildcards) origin =
      let listed = map (T.drop 2) wildcards
          orig = T.intercalate "." . tail . T.splitOn "." $ origin
      in orig `elem` listed || origin `elem` listed
