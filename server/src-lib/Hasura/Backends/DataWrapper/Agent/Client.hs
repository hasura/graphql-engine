{-# LANGUAGE DeriveAnyClass #-}

--
module Hasura.Backends.DataWrapper.Agent.Client
  ( ConnSourceConfig (..),
    Hasura.Backends.DataWrapper.Agent.Client.client,
  )
where

import Control.Exception (try)
import Control.Monad.Free
-- import           Hasura.Tracing                         (MonadTrace, tracedHttpRequest)

-- import qualified Network.HTTP.Client.Transformable      as Transformable

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Casing qualified as J
import Data.Text (unpack)
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Base.Error
import Hasura.Incremental.Internal.Dependency (Cacheable (..))
import Hasura.Prelude
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Servant.Client
import Servant.Client.Core.RunClient (ClientF (..))
import Servant.Client.Generic
import Servant.Client.Internal.HttpClient (clientResponseToResponse)

--------------------------------------------------------------------------------
-- Client

newtype ConnSourceConfig = ConnSourceConfig
  { dcscEndpoint :: Text
  }
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving anyclass (Hashable, NFData, Cacheable)

instance ToJSON ConnSourceConfig where
  toJSON =
    J.genericToJSON $
      J.defaultOptions
        { J.fieldLabelModifier = J.snakeCase . drop 4
        }

instance FromJSON ConnSourceConfig where
  parseJSON =
    J.genericParseJSON $
      J.defaultOptions
        { J.fieldLabelModifier = J.snakeCase . drop 4
        }

-- | Create a record of client functions (see 'Routes') from a 'ConnSourceConfig'
-- configuration object. This function takes care to add trace headers, and to
-- propagate useful errors back to the client for debugging purposes.
client ::
  forall m.
  (MonadIO m {- MonadTrace m, -}, MonadError QErr m) =>
  Manager ->
  ConnSourceConfig ->
  IO (API.Routes (AsClientT m))
client mgr config = do
  baseUrl <- parseBaseUrl (unpack (dcscEndpoint config))
  let interpret :: ClientF a -> m a
      interpret (RunRequest req k) = do
        let req' = defaultMakeClientRequest baseUrl req
        -- TODO: Had to remove tracing here because the API changed when
        -- request transformations were added:
        -- tracedHttpRequest (Transformable.fromRequest req) \req' -> do
        do
          responseOrException <- liftIO . try @HTTP.HttpException $ HTTP.httpLbs req' mgr
          case responseOrException of
            Left ex ->
              -- TODO: log the error details
              throw500 $
                "Error in dynamic backend: "
                  <> case ex of
                    HTTP.InvalidUrlException {} -> "invalid URL"
                    HTTP.HttpExceptionRequest _ detail ->
                      case detail of
                        HTTP.StatusCodeException {} -> "status code"
                        HTTP.TooManyRedirects {} -> "too many redirects"
                        HTTP.OverlongHeaders {} -> "overlong headers"
                        HTTP.ResponseTimeout {} -> "response timeout"
                        HTTP.ConnectionTimeout {} -> "connection timeout"
                        HTTP.ConnectionFailure {} -> "connection failure"
                        HTTP.InvalidStatusLine {} -> "invalid status line"
                        HTTP.InvalidHeader {} -> "invalid header"
                        HTTP.InvalidRequestHeader {} -> "invalid request header"
                        HTTP.InternalException {} -> "internal error"
                        HTTP.ProxyConnectException {} -> "proxy connection error"
                        HTTP.NoResponseDataReceived {} -> "no response data received"
                        HTTP.TlsNotSupported {} -> "TLS not supported"
                        HTTP.WrongRequestBodyStreamSize {} -> "wrong request body stream size"
                        HTTP.ResponseBodyTooShort {} -> "response body too short"
                        HTTP.InvalidChunkHeaders {} -> "invalid chunk headers"
                        HTTP.IncompleteHeaders {} -> "incomplete headers"
                        HTTP.InvalidDestinationHost {} -> "invalid destination host"
                        HTTP.HttpZlibException {} -> "HTTP zlib error"
                        HTTP.InvalidProxyEnvironmentVariable {} -> "invalid proxy environment variable"
                        HTTP.ConnectionClosed {} -> "connection closed"
                        HTTP.InvalidProxySettings {} -> "invalid proxy settings"
            Right response ->
              pure $ k (clientResponseToResponse id response)
      interpret (Throw err) = do
        -- TODO: log the error details
        let status res =
              let s = (responseStatusCode res)
               in tshow (statusCode s) <> " " <> bsToTxt (statusMessage s)
        throw500 $
          "Error in dynamic backend: "
            <> case err of
              FailureResponse _ res -> "the server returned status " <> status res
              DecodeFailure _ res -> "unable to decode the response; status " <> status res
              UnsupportedContentType _ res -> "unsupported content type in response; status " <> status res
              InvalidContentTypeHeader res -> "invalid content type in response; status " <> status res
              ConnectionError {} -> "connection error"
  pure $ genericClientHoist (foldFree interpret)
