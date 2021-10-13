module Hasura.HTTP
  ( wreqOptions,
    HttpException (..),
    hdrsToText,
    addDefaultHeaders,
    HttpResponse (..),
    addHttpResponseHeaders,
    getHTTPExceptionStatus,
    serializeHTTPExceptionMessage,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson qualified as J
import Data.CaseInsensitive (original)
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Conversions (UTF8 (..), convertText)
import Data.Text.Encoding qualified as TE
import Hasura.Prelude
import Hasura.Server.Utils (redactSensitiveHeader)
import Hasura.Server.Version (currentVersion)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wreq qualified as Wreq

hdrsToText :: [HTTP.Header] -> [(Text, Text)]
hdrsToText hdrs =
  [ (bsToTxt $ original hdrName, bsToTxt hdrVal)
    | (hdrName, hdrVal) <- hdrs
  ]

wreqOptions :: HTTP.Manager -> [HTTP.Header] -> Wreq.Options
wreqOptions manager hdrs =
  Wreq.defaults
    & Wreq.headers .~ addDefaultHeaders hdrs
    & Wreq.checkResponse ?~ (\_ _ -> return ())
    & Wreq.manager .~ Right manager

-- Adds defaults headers overwriting any existing ones
addDefaultHeaders :: [HTTP.Header] -> [HTTP.Header]
addDefaultHeaders hdrs = defaultHeaders <> rmDefaultHeaders hdrs
  where
    rmDefaultHeaders = filter (not . isDefaultHeader)

isDefaultHeader :: HTTP.Header -> Bool
isDefaultHeader (hdrName, _) = hdrName `elem` map fst defaultHeaders

defaultHeaders :: [HTTP.Header]
defaultHeaders = [contentType, userAgent]
  where
    contentType = ("Content-Type", "application/json")
    userAgent =
      ( "User-Agent",
        "hasura-graphql-engine/" <> unUTF8 (convertText currentVersion)
      )

newtype HttpException = HttpException
  {unHttpException :: HTTP.HttpException}
  deriving (Show)

getHTTPExceptionStatus :: HttpException -> Maybe Int
getHTTPExceptionStatus = \case
  (HttpException (HTTP.HttpExceptionRequest _ httpExceptionContent)) ->
    case httpExceptionContent of
      HTTP.StatusCodeException response _ -> Just $ HTTP.statusCode $ HTTP.responseStatus response
      HTTP.ProxyConnectException _ _ status -> Just $ HTTP.statusCode status
      _ -> Nothing
  (HttpException (HTTP.InvalidUrlException _ _)) -> Nothing

serializeHTTPExceptionMessage :: HttpException -> Text
serializeHTTPExceptionMessage (HttpException (HTTP.HttpExceptionRequest _ httpExceptionContent)) =
  case httpExceptionContent of
    HTTP.StatusCodeException _ _ -> "unexpected"
    HTTP.TooManyRedirects _ -> "Too many redirects"
    HTTP.OverlongHeaders -> "Overlong headers"
    HTTP.ResponseTimeout -> "Response timeout"
    HTTP.ConnectionTimeout -> "Connection timeout"
    HTTP.ConnectionFailure _ -> "Connection failure"
    HTTP.InvalidStatusLine _ -> "Invalid HTTP Status Line"
    HTTP.InternalException _ -> "Internal Exception"
    HTTP.ProxyConnectException _ _ _ -> "Proxy connection exception"
    HTTP.NoResponseDataReceived -> "No response data received"
    HTTP.TlsNotSupported -> "TLS not supported"
    HTTP.InvalidDestinationHost _ -> "Invalid destination host"
    HTTP.InvalidHeader _ -> "Invalid Header"
    HTTP.InvalidRequestHeader _ -> "Invalid Request Header"
    HTTP.WrongRequestBodyStreamSize _ _ -> "Wrong request body stream size"
    HTTP.ResponseBodyTooShort _ _ -> "Response body too short"
    HTTP.InvalidChunkHeaders -> "Invalid chunk headers"
    HTTP.IncompleteHeaders -> "Incomplete headers"
    _ -> "unexpected"
serializeHTTPExceptionMessage (HttpException (HTTP.InvalidUrlException url reason)) = T.pack $ "URL: " <> url <> " is invalid because " <> reason

encodeHTTPRequestJSON :: HTTP.Request -> J.Value
encodeHTTPRequestJSON request =
  J.Object $
    M.fromList
      [ ("host", J.toJSON $ TE.decodeUtf8 $ HTTP.host request),
        ("port", J.toJSON $ HTTP.port request),
        ("secure", J.toJSON $ HTTP.secure request),
        ("requestHeaders", J.toJSON $ M.fromList $ hdrsToText $ map redactSensitiveHeader $ HTTP.requestHeaders request),
        ("path", J.toJSON $ TE.decodeUtf8 $ HTTP.path request),
        ("queryString", J.toJSON $ TE.decodeUtf8 $ HTTP.queryString request),
        ("method", J.toJSON $ TE.decodeUtf8 $ HTTP.method request),
        ("responseTimeout", J.String $ tshow $ HTTP.responseTimeout request)
      ]

instance J.ToJSON HttpException where
  toJSON httpException =
    case httpException of
      (HttpException (HTTP.InvalidUrlException _ e)) ->
        J.object
          [ "type" J..= ("invalid_url" :: Text),
            "message" J..= e
          ]
      (HttpException (HTTP.HttpExceptionRequest req _)) ->
        let statusMaybe = getHTTPExceptionStatus httpException
            exceptionContent = serializeHTTPExceptionMessage httpException
            reqJSON = encodeHTTPRequestJSON req
         in J.object $
              [ "type" J..= ("http_exception" :: Text),
                "message" J..= exceptionContent,
                "request" J..= reqJSON
              ]
                <> maybe mempty (\status -> ["status" J..= status]) statusMaybe

data HttpResponse a = HttpResponse
  { _hrBody :: !a,
    _hrHeaders :: !HTTP.ResponseHeaders
  }
  deriving (Functor, Foldable, Traversable)

addHttpResponseHeaders :: HTTP.ResponseHeaders -> HttpResponse a -> HttpResponse a
addHttpResponseHeaders newHeaders (HttpResponse b h) = HttpResponse b (newHeaders <> h)
