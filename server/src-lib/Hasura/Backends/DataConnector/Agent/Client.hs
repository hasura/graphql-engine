{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.DataConnector.Agent.Client
  ( AgentClientContext (..),
    AgentClientT,
    runAgentClientT,
  )
where

import Control.Exception (try)
import Hasura.Backends.DataConnector.Logging (logAgentRequest, logClientError)
import Hasura.Base.Error
import Hasura.HTTP qualified
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.Tracing (MonadTrace, tracedHttpRequest)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Transformable qualified as TransformableHTTP
import Network.HTTP.Types.Status (Status)
import Servant.Client
import Servant.Client.Core (Request, RunClient (..))
import Servant.Client.Internal.HttpClient (clientResponseToResponse, mkFailureResponse)

data AgentClientContext = AgentClientContext
  { _accLogger :: Logger Hasura,
    _accBaseUrl :: BaseUrl,
    _accHttpManager :: Manager
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
  let req' = defaultMakeClientRequest _accBaseUrl req

  transformableReq <-
    TransformableHTTP.tryFromClientRequest req'
      `onLeft` (\err -> throw500 $ "Error in Data Connector backend: Could not create request. " <> err)

  (tracedReq, responseOrException) <- tracedHttpRequest transformableReq (\tracedReq -> fmap (tracedReq,) . liftIO . try @HTTP.HttpException $ TransformableHTTP.performRequest tracedReq _accHttpManager)
  logAgentRequest _accLogger tracedReq responseOrException
  case responseOrException of
    Left ex ->
      throw500 $ "Error in Data Connector backend: " <> Hasura.HTTP.serializeHTTPExceptionMessage (Hasura.HTTP.HttpException ex)
    Right response -> do
      let status = TransformableHTTP.responseStatus response
          servantResponse = clientResponseToResponse id response
          goodStatus = case acceptStatus of
            Nothing -> TransformableHTTP.statusIsSuccessful status
            Just good -> status `elem` good
      if goodStatus
        then pure $ servantResponse
        else throwClientError $ mkFailureResponse _accBaseUrl req servantResponse

throwClientError' :: (MonadIO m, MonadTrace m, MonadError QErr m) => ClientError -> (AgentClientT m) a
throwClientError' err = do
  AgentClientContext {..} <- askClientContext
  logClientError _accLogger err
  throw500 $ "Error in Data Connector backend: " <> Hasura.HTTP.serializeServantClientErrorMessage err
