{-# OPTIONS_GHC -Wno-orphans #-}

-- | Our HTTP client library, with better ergonomics for logging and so on (see
-- 'Request').
--
-- NOTE: Do not create requests with IO based RequestBody
-- constructors. They cannot be transformed or logged.
--
-- NOTE: This module is meant to be imported qualified, e.g.
--
-- >  import qualified Network.HTTP.Client.Transformable as HTTP
--
-- ...or
--
-- >  import qualified Network.HTTP.Client.Transformable as Transformable
module Network.HTTP.Client.Transformable
  ( Client.Request,
    mkRequestThrow,
    mkRequestEither,
    url,
    Network.HTTP.Client.Transformable.method,
    headers,
    host,
    body,
    _RequestBodyLBS,
    port,
    path,
    queryParams,
    secure,
    timeout,
    getReqSize,
    getQueryStr,
    Client.Response (..),
    Client.ResponseTimeout,
    Client.HttpException (..),
    Internal.HttpExceptionContent (..),
    Client.Manager,
    Client.httpLbs,
    Client.responseTimeoutDefault,
    Client.responseTimeoutMicro,
    Client.newManager,
    module Types,
    module TLSClient,
    Client.RequestBody (..),
  )
where

-------------------------------------------------------------------------------

import Control.Exception.Safe (impureThrow)
import Control.Lens (Lens', Prism', lens, preview, prism', set, strict, to, view, (^.), (^?))
import Control.Monad.Catch (MonadThrow, fromException)
import Data.Aeson qualified as J
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Strict.Lens qualified as Strict (utf8)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Internal qualified as Internal
import Network.HTTP.Client.TLS as TLSClient
import Network.HTTP.Conduit qualified as NHS
import Network.HTTP.Simple qualified as NHS
import Network.HTTP.Types as Types
import Network.URI qualified as URI
import Prelude

-------------------------------------------------------------------------------

-- NOTE: This function makes internal usage of `Strict.utf8`/`TE.decodeUtf8`,
-- which throws an impure exception when the supplied `ByteString` cannot be
-- decoded into valid UTF8 text!
instance J.ToJSON Client.Request where
  toJSON req =
    J.object
      [ "url" J..= (req ^. url),
        "method" J..= (req ^. method . Strict.utf8),
        "headers" J..= (req ^. headers . renderHeaders),
        -- NOTE: We cannot decode IO based body types.
        "body" J..= (req ^? body . _RequestBodyLBS . strict . Strict.utf8),
        "query_string" J..= (req ^. to Client.queryString . Strict.utf8),
        "response_timeout" J..= (req ^. timeout . renderResponseTimeout)
      ]
    where
      renderHeaders = to $ fmap \(keyBytes, valBytes) ->
        let keyTxt = TE.decodeUtf8 . CI.original $ keyBytes
            valTxt = TE.decodeUtf8 valBytes
         in (keyTxt, valTxt)

      renderResponseTimeout = to $ \case
        Internal.ResponseTimeoutMicro i -> show i
        Internal.ResponseTimeoutNone -> "None"
        Internal.ResponseTimeoutDefault -> "default"

-- | Convert a URL into a Request value.
--
-- NOTE: This function will throw an error in 'MonadThrow' if the URL is
-- invalid.
mkRequestThrow :: (MonadThrow m) => Text -> m Client.Request
mkRequestThrow = Client.parseRequest . T.unpack

-- | 'mkRequestThrow' with the 'MonadThrow' instance specialized to 'Either'.
--
-- NOTE: While this function makes use of 'impureThrow', it should be
-- impossible to trigger in practice.
--
-- 'mkRequestThrow' calls 'Client.parseRequest', which only ever throws
-- 'Client.HttpException' errors (which should be "caught" by the
-- 'fromException' cast).
mkRequestEither :: Text -> Either Client.HttpException Client.Request
mkRequestEither urlTxt =
  mkRequestThrow urlTxt & first
    \someExc -> case fromException @Client.HttpException someExc of
      Just httpExc -> httpExc
      Nothing -> impureThrow someExc

-- | Url is 'materialized view' into `Request` consisting of
-- concatenation of `host`, `port`, `queryParams`, and `path` in the
-- underlying request object, as well as a literal url field that
-- stores the textual representation that was supplied from metadata.
--
-- The reason why we store the textual URL in addition to the parsed
-- URL in the request is that the parsed URL loses syntactic information
-- such as "does http://foo.com end in a slash?" which is important
-- when a template user has expectations about the $url variable
-- matching the string that was configured in the action.
--
-- We use the literal field to `view` the value but we must
-- carefully set the subcomponents by hand during `set` operations. Be
-- careful modifying this lens and verify against the unit tests..
url :: Lens' Client.Request Text
url = lens getUrl setUrl
  where
    getUrl :: Client.Request -> Text
    getUrl req = T.pack $ URI.uriToString id (Client.getUri req) mempty

    setUrl :: Client.Request -> Text -> Client.Request
    setUrl req url' = fromMaybe req $ do
      uri <- URI.parseURI (T.unpack url')
      URI.URIAuth {..} <- URI.uriAuthority uri
      let host' = C8.pack $ uriUserInfo <> uriRegName
          ssl = URI.uriScheme uri == "https:"
          port' = case uriPort of
            ':' : newPort -> read @Int newPort
            _ -> if ssl then 443 else 80
          queryString = Types.queryTextToQuery $ Types.parseQueryText $ C8.pack $ URI.uriQuery uri
          path' = C8.pack $ URI.uriPath uri
      pure $
        req
          & set host host'
          & set secure ssl
          & set port port'
          & set queryParams queryString
          & set path path'

body :: Lens' Client.Request NHS.RequestBody
body = lens getBody setBody
  where
    getBody :: Client.Request -> NHS.RequestBody
    getBody = NHS.requestBody

    setBody :: Client.Request -> NHS.RequestBody -> Client.Request
    setBody req newBody = req {NHS.requestBody = newBody}

-- NOTE: We cannot decode IO based body types.
_RequestBodyLBS :: Prism' NHS.RequestBody BL.ByteString
_RequestBodyLBS = prism' Client.RequestBodyLBS $ \case
  Client.RequestBodyLBS lbs -> pure lbs
  Client.RequestBodyBS bs -> pure (BL.fromStrict bs)
  Client.RequestBodyBuilder _ bldr -> pure (Builder.toLazyByteString bldr)
  _ -> Nothing

headers :: Lens' Client.Request [Types.Header]
headers = lens getHeaders setHeaders
  where
    getHeaders :: Client.Request -> [Types.Header]
    getHeaders = Client.requestHeaders

    setHeaders :: Client.Request -> [Types.Header] -> Client.Request
    setHeaders req headers' = NHS.setRequestHeaders headers' req

host :: Lens' Client.Request B.ByteString
host = lens getHost setHost
  where
    getHost :: Client.Request -> B.ByteString
    getHost = Client.host

    setHost :: Client.Request -> B.ByteString -> Client.Request
    setHost req host' = NHS.setRequestHost host' req

secure :: Lens' Client.Request Bool
secure = lens getSecure setSecure
  where
    getSecure :: Client.Request -> Bool
    getSecure = Client.secure

    setSecure :: Client.Request -> Bool -> Client.Request
    setSecure req ssl = NHS.setRequestSecure ssl req

method :: Lens' Client.Request B.ByteString
method = lens getMethod setMethod
  where
    getMethod :: Client.Request -> B.ByteString
    getMethod = Client.method

    setMethod :: Client.Request -> B.ByteString -> Client.Request
    setMethod req method' = NHS.setRequestMethod method' req

path :: Lens' Client.Request B.ByteString
path = lens getPath setPath
  where
    getPath :: Client.Request -> B.ByteString
    getPath = Client.path

    setPath :: Client.Request -> B.ByteString -> Client.Request
    setPath req p = req {Client.path = p}

port :: Lens' Client.Request Int
port = lens getPort setPort
  where
    getPort :: Client.Request -> Int
    getPort = Client.port

    setPort :: Client.Request -> Int -> Client.Request
    setPort req i = NHS.setRequestPort i req

getQueryStr :: Client.Request -> ByteString
getQueryStr = Types.renderQuery True . view queryParams

queryParams :: Lens' Client.Request NHS.Query
queryParams = lens getQueryParams setQueryParams
  where
    getQueryParams :: Client.Request -> NHS.Query
    getQueryParams = NHS.getRequestQueryString

    setQueryParams :: Client.Request -> NHS.Query -> Client.Request
    setQueryParams req params = NHS.setQueryString params req

timeout :: Lens' Client.Request Client.ResponseTimeout
timeout = lens getTimeout setTimeout
  where
    getTimeout :: Client.Request -> Client.ResponseTimeout
    getTimeout = Client.responseTimeout

    setTimeout :: Client.Request -> Client.ResponseTimeout -> Client.Request
    setTimeout req timeout' = req {Client.responseTimeout = timeout'}

getReqSize :: Client.Request -> Int64
getReqSize req = maybe 0 BL.length $ preview (body . _RequestBodyLBS) req
