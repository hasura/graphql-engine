module Network.HTTP.Client.Transformable
  ( Request,
    mkRequestThrow,
    mkRequestEither,
    url,
    Network.HTTP.Client.Transformable.method,
    headers,
    host,
    body,
    port,
    path,
    queryParams,
    timeout,
    getReqSize,
    getQueryStr,
    performRequest,
    Client.Response (..),
    Client.ResponseTimeout,
    Client.HttpException (..),
    Internal.HttpExceptionContent (..),
    Client.Manager,
    Client.responseTimeoutDefault,
    Client.responseTimeoutMicro,
    Client.newManager,
    module Types,
    module TLSClient,
  )
where

import Control.Exception (throw)
import Control.Lens (Lens', lens, set, view)
import Control.Monad.Catch (MonadThrow, fromException)
import Data.Aeson qualified as J
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.Internal qualified as Internal
import Network.HTTP.Client.TLS as TLSClient
import Network.HTTP.Conduit qualified as NHS
import Network.HTTP.Simple qualified as NHS
import Network.HTTP.Types as Types
import Network.URI qualified as URI
import Prelude

-- | @Network.HTTP.Client@.'Client.Request' stores the request body in a sum
-- type which has a case containing IO along with some other unwieldy cases.
-- This makes it difficult to log our requests before and after transformation.
--
-- In our codebase we only ever use the Lazy ByteString case. So by
-- lifting the request body out of Network.HTTP.Client.Request, we
-- make it much easier to log our Requests.
--
-- When executing the request we simply insert the value at `rdBody`
-- into the Request.
--
-- When working with Transformable Requests you should always import
-- this module qualified and use the `mkRequest*` functions for
-- constructing requests. Modification of Request should be done using
-- the provided lens API.
--
-- NOTE: This module is meant to be imported qualified, e.g.
--
-- >  import qualified Network.HTTP.Client.Transformable as HTTP
--
-- ...or
--
-- >  import qualified Network.HTTP.Client.Transformable as Transformable
data Request = Request
  { rdRequest :: Client.Request,
    rdBody :: Maybe BL.ByteString
  }
  deriving (Show)

instance J.ToJSON Request where
  toJSON req@Request {..} =
    J.object
      [ "url" J..= view url req,
        "method" J..= TE.decodeUtf8 (view method req),
        "headers" J..= fmap (bimap (TE.decodeUtf8 . CI.original) TE.decodeUtf8) (view headers req),
        "body" J..= fmap (TE.decodeUtf8 . BL.toStrict) rdBody,
        "query_string" J..= TE.decodeUtf8 (Client.queryString rdRequest),
        "response_timeout" J..= serializeRT (view timeout req)
      ]
    where
      serializeRT = \case
        Internal.ResponseTimeoutMicro i -> show i
        Internal.ResponseTimeoutNone -> "None"
        Internal.ResponseTimeoutDefault -> "default"

-- | Convert a URL into a Request value. This function can throw
-- if the URL is invalid.
mkRequestThrow :: MonadThrow m => T.Text -> m Request
mkRequestThrow url' = do
  req <- Client.parseRequest $ T.unpack url'
  pure $ Request req Nothing

-- | A concrete version of `mkRequestThrow` in the Either monad.
mkRequestEither :: T.Text -> Either Client.HttpException Request
mkRequestEither url' =
  case mkRequestThrow url' of
    Right res -> Right res
    Left e -> case fromException @Client.HttpException e of
      Just httpExcept -> Left httpExcept
      Nothing -> throw e

-- | Url is 'materialized view' into `Request` consisting of
-- concatenation of `host`, `port`, `queryParams`, and `path`.
--
-- We use `Network.HTTP.Client.getUri` to `view` the value but we must
-- carefully set the subcomponents by hand during `set` operations. Be
-- careful modifying this lens and verify against the unit tests..
url :: Lens' Request T.Text
url = lens getUrl setUrl
  where
    getUrl :: Request -> T.Text
    getUrl Request {rdRequest} = T.pack $ URI.uriToString id (Client.getUri rdRequest) mempty

    setUrl :: Request -> T.Text -> Request
    setUrl req url' = fromMaybe req $ do
      uri <- URI.parseURI (T.unpack url')
      URI.URIAuth {..} <- URI.uriAuthority uri
      let host' = C8.pack $ uriUserInfo <> uriRegName
          port' = case uriPort of
            ':' : newPort -> read @Int newPort
            _ -> 80
          queryString = Types.queryTextToQuery $ Types.parseQueryText $ C8.pack $ URI.uriQuery uri
          path' = C8.pack $ URI.uriPath uri

      pure $
        req & set host host'
          & set port port'
          & set queryParams queryString
          & set path path'

body :: Lens' Request (Maybe BL.ByteString)
body = lens rdBody setBody
  where
    setBody :: Request -> Maybe BL.ByteString -> Request
    setBody req body' = req {rdBody = body'}

headers :: Lens' Request [Types.Header]
headers = lens getHeaders setHeaders
  where
    getHeaders :: Request -> [Types.Header]
    getHeaders Request {rdRequest} = Client.requestHeaders rdRequest

    setHeaders :: Request -> [Types.Header] -> Request
    setHeaders req@Request {rdRequest} headers' =
      req {rdRequest = NHS.setRequestHeaders headers' rdRequest}

host :: Lens' Request B.ByteString
host = lens getHost setHost
  where
    getHost :: Request -> B.ByteString
    getHost Request {rdRequest} = Client.host rdRequest

    setHost :: Request -> B.ByteString -> Request
    setHost req@Request {rdRequest} host' =
      req {rdRequest = NHS.setRequestHost host' rdRequest}

method :: Lens' Request B.ByteString
method = lens getMethod setMethod
  where
    getMethod :: Request -> B.ByteString
    getMethod Request {rdRequest} = Client.method rdRequest

    setMethod :: Request -> B.ByteString -> Request
    setMethod req@Request {rdRequest} method' = req {rdRequest = NHS.setRequestMethod method' rdRequest}

path :: Lens' Request B.ByteString
path = lens getPath setPath
  where
    getPath :: Request -> B.ByteString
    getPath Request {rdRequest} = Client.path rdRequest

    setPath :: Request -> B.ByteString -> Request
    setPath req@Request {rdRequest} p =
      req {rdRequest = rdRequest {Client.path = p}}

port :: Lens' Request Int
port = lens getPort setPort
  where
    getPort :: Request -> Int
    getPort Request {rdRequest} = Client.port rdRequest

    setPort :: Request -> Int -> Request
    setPort req@Request {rdRequest} i =
      req {rdRequest = NHS.setRequestPort i rdRequest}

getQueryStr :: Request -> ByteString
getQueryStr = Types.renderQuery True . view queryParams

queryParams :: Lens' Request NHS.Query
queryParams = lens getQueryParams setQueryParams
  where
    getQueryParams :: Request -> NHS.Query
    getQueryParams Request {rdRequest} = NHS.getRequestQueryString rdRequest

    setQueryParams :: Request -> NHS.Query -> Request
    setQueryParams req@Request {rdRequest} params = req {rdRequest = NHS.setQueryString params rdRequest}

timeout :: Lens' Request Client.ResponseTimeout
timeout = lens getTimeout setTimeout
  where
    getTimeout :: Request -> Client.ResponseTimeout
    getTimeout Request {rdRequest} = Client.responseTimeout rdRequest

    setTimeout :: Request -> Client.ResponseTimeout -> Request
    setTimeout req@Request {rdRequest} timeout' =
      let updatedReq = rdRequest {Client.responseTimeout = timeout'}
       in req {rdRequest = updatedReq}

getReqSize :: Request -> Int64
getReqSize Request {rdBody} = maybe 0 BL.length rdBody

toRequest :: Request -> Client.Request
toRequest Request {..} = case rdBody of
  Nothing -> rdRequest
  Just body' -> NHS.setRequestBody (Client.RequestBodyLBS body') rdRequest

performRequest :: Request -> Client.Manager -> IO (Client.Response BL.ByteString)
performRequest req manager = Client.httpLbs (toRequest req) manager
