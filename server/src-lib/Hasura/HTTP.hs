module Hasura.HTTP
  ( wreqOptions,
    HttpException (..),
    hdrsToText,
    textToHdrs,
    addDefaultHeaders,
    defaultHeaders,
    HttpResponse (..),
    addHttpResponseHeaders,
    getHTTPExceptionStatus,
    serializeHTTPExceptionMessage,
    ShowHeadersAndEnvVarInfo (..),
    serializeHTTPExceptionWithErrorMessage,
    serializeHTTPExceptionMessageForDebugging,
    encodeHTTPRequestJSON,
    ShowErrorInfo (..),
    getHttpExceptionJson,
    serializeServantClientErrorMessage,
    serializeServantClientErrorMessageForDebugging,
  )
where

import Control.Exception (Exception (..), fromException)
import Control.Lens hiding ((.=))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as KM
import Data.CaseInsensitive (mk, original)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Text.Conversions (UTF8 (..), convertText)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Hasura.Prelude
import Hasura.Server.Utils (redactSensitiveHeader)
import Hasura.Server.Version (currentVersion)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Restricted qualified as Restricted
import Network.HTTP.Media qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wreq qualified as Wreq
import Servant.Client qualified as Servant

hdrsToText :: [HTTP.Header] -> [(Text, Text)]
hdrsToText hdrs =
  [ (bsToTxt $ original hdrName, bsToTxt hdrVal)
    | (hdrName, hdrVal) <- hdrs
  ]

textToHdrs :: [(Text, Text)] -> [HTTP.Header]
textToHdrs hdrs =
  [ (mk (txtToBs hdrName), TE.encodeUtf8 hdrVal)
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
    HTTP.InternalException err -> case fromException err of
      Just (Restricted.ConnectionRestricted _ _) -> "Blocked connection to private IP address"
      Nothing -> "Internal Exception"
    HTTP.ProxyConnectException {} -> "Proxy connection exception"
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

newtype ShowHeadersAndEnvVarInfo = ShowHeadersAndEnvVarInfo {unShowHeadersAndEnvVarInfo :: Bool}
  deriving (Show, Eq)

serializeHTTPExceptionWithErrorMessage :: ShowHeadersAndEnvVarInfo -> HTTP.HttpException -> Text
serializeHTTPExceptionWithErrorMessage (ShowHeadersAndEnvVarInfo isShowHeaderAndEnvVarInfo) = \case
  HTTP.HttpExceptionRequest _ err -> case err of
    HTTP.StatusCodeException response _ -> "Response status code indicated failure" <> (tshow . HTTP.statusCode $ HTTP.responseStatus response)
    HTTP.TooManyRedirects redirects -> "Too many redirects: " <> tshow (length redirects) <> " redirects"
    HTTP.OverlongHeaders -> "Overlong headers"
    HTTP.ResponseTimeout -> "Response timeout"
    HTTP.ConnectionTimeout -> "Connection timeout"
    HTTP.ConnectionFailure exn -> "Connection failure: " <> serializeExceptionForDebugging exn
    HTTP.InvalidStatusLine statusLine -> "Invalid HTTP status line: " <> fromUtf8 statusLine
    HTTP.InvalidHeader header ->
      if isShowHeaderAndEnvVarInfo
        then "Invalid header: " <> fromUtf8 header
        else "Invalid Header"
    HTTP.InvalidRequestHeader requestHeader ->
      if isShowHeaderAndEnvVarInfo
        then "Invalid request header: " <> fromUtf8 requestHeader
        else "Invalid request header"
    HTTP.InternalException exn -> case fromException exn of
      Just (Restricted.ConnectionRestricted _ _) -> "Blocked connection to private IP address: " <> serializeExceptionForDebugging exn
      Nothing -> "Internal error: " <> serializeExceptionForDebugging exn
    HTTP.ProxyConnectException proxyHost port status -> "Proxy connection to " <> fromUtf8 proxyHost <> ":" <> tshow port <> " returned response with status code that indicated failure: " <> tshow (HTTP.statusCode status)
    HTTP.NoResponseDataReceived -> "No response data received"
    HTTP.TlsNotSupported -> "TLS not supported"
    HTTP.WrongRequestBodyStreamSize expected actual -> "Wrong request body stream size. expected: " <> tshow expected <> ", actual: " <> tshow actual
    HTTP.ResponseBodyTooShort expected actual -> "Response body too short. expected: " <> tshow expected <> ", actual: " <> tshow actual
    HTTP.InvalidChunkHeaders -> "Invalid chunk headers"
    HTTP.IncompleteHeaders -> "Incomplete headers"
    HTTP.InvalidDestinationHost host -> "Invalid destination host: " <> fromUtf8 host
    HTTP.HttpZlibException exn -> "HTTP zlib error: " <> serializeExceptionForDebugging exn
    HTTP.InvalidProxyEnvironmentVariable name value ->
      if isShowHeaderAndEnvVarInfo
        then "Invalid proxy environment variable: " <> name <> "=" <> value
        else "Invalid proxy environment variable: " <> name
    HTTP.ConnectionClosed -> "Connection closed"
    HTTP.InvalidProxySettings err' -> "Invalid proxy settings: " <> err'
  HTTP.InvalidUrlException url' reason -> "Invalid url: " <> T.pack url' <> "; reason: " <> T.pack reason
  where
    fromUtf8 = TE.decodeUtf8With TE.lenientDecode

serializeHTTPExceptionMessageForDebugging :: HTTP.HttpException -> Text
serializeHTTPExceptionMessageForDebugging = serializeHTTPExceptionWithErrorMessage (ShowHeadersAndEnvVarInfo True)

encodeHTTPRequestJSON :: HTTP.Request -> J.Value
encodeHTTPRequestJSON request =
  J.Object
    $ KM.fromList
      [ ("host", J.toJSON $ TE.decodeUtf8 $ HTTP.host request),
        ("port", J.toJSON $ HTTP.port request),
        ("secure", J.toJSON $ HTTP.secure request),
        ("requestHeaders", J.toJSON $ HashMap.fromList $ hdrsToText $ map redactSensitiveHeader $ HTTP.requestHeaders request),
        ("path", J.toJSON $ TE.decodeUtf8 $ HTTP.path request),
        ("queryString", J.toJSON $ TE.decodeUtf8 $ HTTP.queryString request),
        ("method", J.toJSON $ TE.decodeUtf8 $ HTTP.method request),
        ("responseTimeout", J.String $ tshow $ HTTP.responseTimeout request)
      ]

newtype ShowErrorInfo = ShowErrorInfo {unShowErrorInfo :: Bool}
  deriving (Show, Eq)

-- this function excepts a boolean value (`ShowErrorInfo`) when True, exposes the errors associated with the HTTP
-- Exceptions using `serializeHTTPExceptionWithErrorMessage` function.
-- This function is used in event triggers, scheduled triggers and cron triggers where `ShowErrorInfo` is True
getHttpExceptionJson :: ShowErrorInfo -> HttpException -> J.Value
getHttpExceptionJson (ShowErrorInfo isShowHTTPErrorInfo) httpException =
  case httpException of
    (HttpException (HTTP.InvalidUrlException _ e)) ->
      J.object
        [ "type" J..= ("invalid_url" :: Text),
          "message" J..= e
        ]
    (HttpException (HTTP.HttpExceptionRequest req _)) -> do
      let statusMaybe = getHTTPExceptionStatus httpException
          exceptionContent =
            if isShowHTTPErrorInfo
              then serializeHTTPExceptionWithErrorMessage (ShowHeadersAndEnvVarInfo False) (unHttpException httpException)
              else serializeHTTPExceptionMessage httpException
          reqJSON = encodeHTTPRequestJSON req
      J.object
        $ [ "type" J..= ("http_exception" :: Text),
            "message" J..= exceptionContent,
            "request" J..= reqJSON
          ]
        <> maybe mempty (\status -> ["status" J..= status]) statusMaybe

-- it will not show HTTP Exception error message info
instance J.ToJSON HttpException where
  toJSON httpException = getHttpExceptionJson (ShowErrorInfo False) httpException

data HttpResponse a = HttpResponse
  { _hrBody :: !a,
    _hrHeaders :: !HTTP.ResponseHeaders
  }
  deriving (Functor, Foldable, Traversable)

addHttpResponseHeaders :: HTTP.ResponseHeaders -> HttpResponse a -> HttpResponse a
addHttpResponseHeaders newHeaders (HttpResponse b h) = HttpResponse b (newHeaders <> h)

serializeServantClientErrorMessage :: Servant.ClientError -> Text
serializeServantClientErrorMessage = \case
  Servant.FailureResponse _ response -> "response status code indicated failure: " <> (tshow . HTTP.statusCode $ Servant.responseStatusCode response)
  Servant.DecodeFailure decodeErrorText _ -> "unable to decode the response, " <> decodeErrorText
  Servant.UnsupportedContentType mediaType _ -> "unsupported content type in response: " <> TE.decodeUtf8With TE.lenientDecode (HTTP.renderHeader mediaType)
  Servant.InvalidContentTypeHeader _ -> "invalid content type in response"
  Servant.ConnectionError _ -> "connection error"

serializeServantClientErrorMessageForDebugging :: Servant.ClientError -> Text
serializeServantClientErrorMessageForDebugging = \case
  Servant.ConnectionError exn -> case fromException exn of
    Just httpException -> serializeHTTPExceptionMessageForDebugging httpException
    Nothing -> "error in the connection: " <> serializeExceptionForDebugging exn
  other -> serializeServantClientErrorMessage other

serializeExceptionForDebugging :: (Exception e) => e -> Text
serializeExceptionForDebugging = T.pack . displayException
