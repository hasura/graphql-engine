{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.DataConnector.Agent.Client
  ( AgentLicenseKey (..),
    AgentClientContext (..),
    AgentClientT,
    runAgentClientT,
    capabilities,
    schemaGet,
    schemaPost,
    query,
    explain,
    mutation,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (try)
import Control.Lens ((%=), (&~), (.=))
import Data.ByteString (ByteString)
import Data.Text.Extended (toTxt)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Backends.DataConnector.Logging (logAgentRequest, logClientError)
import Hasura.Base.Error
import Hasura.HTTP qualified
import Hasura.Logging (Hasura, Logger)
import Hasura.Prelude
import Hasura.RQL.Types.Common qualified as RQL
import Hasura.Tracing (MonadTrace, MonadTraceContext, b3TraceContextPropagator, traceHTTPRequest)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.HTTP.Types.Status (Status)
import Servant.Client
import Servant.Client.Core (Request, RunClient (..))
import Servant.Client.Generic (genericClient)
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
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadTraceContext, MonadTrace, MonadIO)

runAgentClientT :: AgentClientT m a -> AgentClientContext -> m a
runAgentClientT (AgentClientT action) ctx = runReaderT action ctx

askClientContext :: (Monad m) => AgentClientT m AgentClientContext
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

  (tracedReq, responseOrException) <- traceHTTPRequest b3TraceContextPropagator transformableReq' \tracedReq ->
    fmap (tracedReq,) . liftIO . try @HTTP.HttpException $ HTTP.httpLbs tracedReq _accHttpManager
  logAgentRequest _accLogger tracedReq responseOrException
  case responseOrException of
    Left ex -> throwError $ err500 ConnectionNotEstablished ("Error communicating with data connector agent: " <> Hasura.HTTP.serializeHTTPExceptionMessage (Hasura.HTTP.HttpException ex))
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

-------------------------------------------------------------------------------

capabilities :: (MonadIO m, MonadTrace m, MonadError QErr m) => AgentClientT m API.CapabilitiesResponse
capabilities = do
  capabilitiesGuard =<< (genericClient @API.Routes // API._capabilities)
  where
    errorAction e = throw400WithDetail (mapErrorType $ API._crType e) (API._crMessage e) (API._crDetails e)
    defaultAction = throw400 DataConnectorError "Unexpected data connector capabilities response - Unexpected Type"
    capabilitiesGuard = API.capabilitiesCase defaultAction pure errorAction

schemaGet :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> API.Config -> AgentClientT m API.SchemaResponse
schemaGet sourceName config = do
  schemaGuard =<< (genericClient // API._schemaGet) (toTxt sourceName) config
  where
    errorAction e = throw400WithDetail (mapErrorType $ API._crType e) (API._crMessage e) (API._crDetails e)
    defaultAction = throw400 DataConnectorError "Unexpected data connector schema response - Unexpected Type"
    schemaGuard = API.schemaCase defaultAction pure errorAction

schemaPost :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> API.Config -> API.SchemaRequest -> AgentClientT m API.SchemaResponse
schemaPost sourceName config schemaRequest = do
  schemaGuard =<< (genericClient // API._schemaPost) (toTxt sourceName) config schemaRequest
  where
    errorAction e = throw400WithDetail (mapErrorType $ API._crType e) (API._crMessage e) (API._crDetails e)
    defaultAction = throw400 DataConnectorError "Unexpected data connector schema response - Unexpected Type"
    schemaGuard = API.schemaCase defaultAction pure errorAction

query :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> API.Config -> API.QueryRequest -> AgentClientT m API.QueryResponse
query sourceName config queryRequest = do
  queryGuard =<< (genericClient // API._query) (toTxt sourceName) config queryRequest
  where
    errorAction e = throw400WithDetail (mapErrorType $ API._crType e) (API._crMessage e) (API._crDetails e)
    defaultAction = throw400 DataConnectorError "Unexpected data connector query response - Unexpected Type"
    queryGuard = API.queryCase defaultAction pure errorAction

explain :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> API.Config -> API.QueryRequest -> AgentClientT m API.ExplainResponse
explain sourceName config queryRequest = do
  (genericClient // API._explain) (toTxt sourceName) config queryRequest

mutation :: (MonadIO m, MonadTrace m, MonadError QErr m) => RQL.SourceName -> API.Config -> API.MutationRequest -> AgentClientT m API.MutationResponse
mutation sourceName config mutationRequest = do
  mutationGuard =<< (genericClient // API._mutation) (toTxt sourceName) config mutationRequest
  where
    errorAction e = throw400WithDetail (mapErrorType $ API._crType e) (API._crMessage e) (API._crDetails e)
    defaultAction = throw400 DataConnectorError "Unexpected data connector mutation response - Unexpected Type"
    mutationGuard = API.mutationCase defaultAction pure errorAction

mapErrorType :: API.ErrorResponseType -> Code
mapErrorType = \case
  API.UncaughtError -> DataConnectorError
  API.MutationConstraintViolation -> ConstraintViolation
  API.MutationPermissionCheckFailure -> PermissionError
