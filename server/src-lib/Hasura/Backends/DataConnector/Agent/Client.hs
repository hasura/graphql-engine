{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.DataConnector.Agent.Client
  ( AgentLicenseKey (..),
    AgentClientContext (..),
    AgentClientT,
    runAgentClientT,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (try)
import Control.Lens ((%=), (&~), (.=))
import Data.ByteString (ByteString)
import Hasura.Backends.DataConnector.Logging (logAgentRequest, logClientError)
import Hasura.Base.Error
import Hasura.HTTP qualified
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.Tracing (MonadTrace, traceHTTPRequest)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.HTTP.Types.Status (Status)
import Servant.Client
import Servant.Client.Core (Request, RunClient (..))
import Servant.Client.Internal.HttpClient (clientResponseToResponse, mkFailureResponse)

-------------------------------------------------------------------------------rs

-- | Auth Key provided to the GDC Agent in 'Request' headers.
newtype AgentLicenseKey = AgentLicenseKey {unAgentLicenseKey :: ByteString}

data AgentClientContext = AgentClientContext
  { _accLogger :: Logger Hasura,
    _accBaseUrl :: BaseUrl,
    _accHttpManager :: HTTP.Manager,
    _accResponseTimeout :: Maybe Int,
    _accAgentLicenseKey :: Maybe AgentLicenseKey
  }

newtype AgentClientT m a = AgentClientT (ReaderT AgentClientContext m a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadTrace, MonadIO)

runAgentClientT :: AgentClientT m a -> AgentClientContext -> m a
runAgentClientT (AgentClientT action) ctx = runReaderT action ctx

askClientContext :: Monad m => AgentClientT m AgentClientContext
askClientContext = AgentClientT ask

instance (MonadIO m, MonadTrace m, MonadError QErr m) => RunClient (AgentClientT m) where
  runRequestAcceptStatus = runRequestAcceptStatus'
  throwClientError = throwClientError'

runRequestAcceptStatus' :: (MonadIO m, MonadTrace m, MonadError QErr m) => Maybe [Status] -> Request -> (AgentClientT m) Response
runRequestAcceptStatus' acceptStatus req = do
  AgentClientContext {..} <- askClientContext
  let transformableReq = defaultMakeClientRequest _accBaseUrl req

  -- Set the response timeout explicitly if it is provided
  let transformableReq' =
        transformableReq &~ do
          for_ _accResponseTimeout \x -> HTTP.timeout .= HTTP.responseTimeoutMicro x
          HTTP.headers
            %= \headers -> maybe headers (\(AgentLicenseKey key) -> ("X-Hasura-License", key) : headers) _accAgentLicenseKey

  (tracedReq, responseOrException) <- traceHTTPRequest transformableReq' \tracedReq ->
    fmap (tracedReq,) . liftIO . try @HTTP.HttpException $ HTTP.httpLbs tracedReq _accHttpManager
  logAgentRequest _accLogger tracedReq responseOrException
  case responseOrException of
    -- throwConnectionError is used here in order to avoid a metadata inconsistency error
    Left ex -> throwConnectionError $ "Error in Data Connector backend: " <> Hasura.HTTP.serializeHTTPExceptionMessage (Hasura.HTTP.HttpException ex)
    Right response -> do
      let status = HTTP.responseStatus response
          servantResponse = clientResponseToResponse id response
          goodStatus = case acceptStatus of
            Nothing -> HTTP.statusIsSuccessful status
            Just good -> status `elem` good
      if goodStatus
        then pure $ servantResponse
        else throwClientError $ mkFailureResponse _accBaseUrl req servantResponse

throwClientError' :: (MonadIO m, MonadTrace m, MonadError QErr m) => ClientError -> (AgentClientT m) a
throwClientError' err = do
  AgentClientContext {..} <- askClientContext
  logClientError _accLogger err
  case err of
    FailureResponse _ r | responseStatusCode r == HTTP.status401 -> throw401 "EE License Key Required."
    _ -> throw500 $ "Error in Data Connector backend: " <> Hasura.HTTP.serializeServantClientErrorMessage err
