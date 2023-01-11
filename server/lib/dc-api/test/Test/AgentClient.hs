{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.AgentClient
  ( AgentIOClient (..),
    mkAgentIOClient,
    AgentClientConfig (..),
    mkAgentClientConfig,
    HasAgentClient,
    introduceAgentClient,
    getAgentClientConfig,
    AgentClientT,
    runAgentClientT,
    getCapabilitiesGuarded,
    guardCapabilitiesResponse,
    getHealth,
    getSchemaGuarded,
    guardSchemaResponse,
    queryGuarded,
    queryExpectError,
    explain,
    getMetrics,
  )
where

import Command (AgentOptions (..), SensitiveOutputHandling (..))
import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Class (get, modify')
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.CaseInsensitive qualified as CI
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API qualified as API
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types (HeaderName, Status (..), statusIsSuccessful)
import Servant.API (NamedRoutes)
import Servant.API.ContentTypes (NoContent)
import Servant.API.UVerb (Union)
import Servant.Client (BaseUrl, Client, Response, defaultMakeClientRequest, hoistClient, mkClientEnv, runClientM, (//))
import Servant.Client.Core (Request, RunClient (..))
import Servant.Client.Generic (genericClient)
import Servant.Client.Internal.HttpClient (clientResponseToResponse, mkFailureResponse)
import Test.HttpFile qualified as HttpFile
import Test.Sandwich (HasBaseContext, HasLabel, Label (..), LabelValue, NodeOptions (..), SpecFree, defaultNodeOptions, getContext, type (:>))
import Test.Sandwich.Contexts (getCurrentFolder)
import Test.Sandwich.Expectations (expectationFailure)
import Test.Sandwich.Misc (ExampleT)
import Test.Sandwich.Nodes (introduce')

newtype AgentIOClient = AgentIOClient (forall m. MonadIO m => Client m (NamedRoutes API.Routes))

configHeader :: HeaderName
configHeader = CI.mk "X-Hasura-DataConnector-Config"

mkHttpClientManager :: MonadIO m => SensitiveOutputHandling -> m HttpClient.Manager
mkHttpClientManager sensitiveOutputHandling =
  let settings = HttpClient.defaultManagerSettings {HttpClient.managerModifyRequest = pure . addHeaderRedaction sensitiveOutputHandling}
   in liftIO $ HttpClient.newManager settings

addHeaderRedaction :: SensitiveOutputHandling -> HttpClient.Request -> HttpClient.Request
addHeaderRedaction sensitiveOutputHandling request =
  case sensitiveOutputHandling of
    AllowSensitiveOutput -> request
    DisallowSensitiveOutput -> request {HttpClient.redactHeaders = HttpClient.redactHeaders request <> Set.singleton configHeader}

mkAgentIOClient :: MonadIO m => SensitiveOutputHandling -> AgentOptions -> m AgentIOClient
mkAgentIOClient sensitiveOutputHandling AgentOptions {..} = do
  manager <- mkHttpClientManager sensitiveOutputHandling
  let clientEnv = mkClientEnv manager _aoAgentBaseUrl
  pure $ AgentIOClient $ hoistClient (Proxy @(NamedRoutes API.Routes)) (\m -> liftIO (runClientM m clientEnv >>= either throwIO pure)) API.apiClient

data AgentClientConfig = AgentClientConfig
  { _accBaseUrl :: BaseUrl,
    _accHttpManager :: HttpClient.Manager,
    _accSensitiveOutputHandling :: SensitiveOutputHandling
  }

mkAgentClientConfig :: MonadIO m => SensitiveOutputHandling -> AgentOptions -> m AgentClientConfig
mkAgentClientConfig sensitiveOutputHandling AgentOptions {..} = do
  manager <- mkHttpClientManager sensitiveOutputHandling
  pure $ AgentClientConfig _aoAgentBaseUrl manager sensitiveOutputHandling

introduceAgentClient :: forall context m. (MonadIO m, MonadBaseControl IO m) => AgentClientConfig -> SpecFree (LabelValue "agent-client" AgentClientConfig :> context) m () -> SpecFree context m ()
introduceAgentClient agentConfig = introduce' nodeOptions "Introduce agent client" agentClientLabel (pure agentConfig) (const $ pure ())
  where
    nodeOptions =
      defaultNodeOptions
        { nodeOptionsVisibilityThreshold = 150,
          nodeOptionsCreateFolder = False,
          nodeOptionsRecordTime = False
        }

agentClientLabel :: Label "agent-client" AgentClientConfig
agentClientLabel = Label

type HasAgentClient context = HasLabel context "agent-client" AgentClientConfig

getAgentClientConfig :: (Monad m, HasCallStack, HasAgentClient context, MonadReader context m) => m AgentClientConfig
getAgentClientConfig = getContext agentClientLabel

data AgentClientState = AgentClientState
  { _acsConfig :: AgentClientConfig,
    _acsRequestCounter :: Int
  }

newtype AgentClientT m a = AgentClientT (StateT AgentClientState m a)
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader r)

runAgentClientT :: (Monad m, HasAgentClient context) => AgentClientT (ExampleT context m) a -> ExampleT context m a
runAgentClientT (AgentClientT action) = do
  config <- getAgentClientConfig
  evalStateT action (AgentClientState config 0)

getClientState :: Monad m => AgentClientT m AgentClientState
getClientState = AgentClientT get

incrementRequestCounter :: Monad m => AgentClientT m ()
incrementRequestCounter = AgentClientT $ modify' \state -> state {_acsRequestCounter = _acsRequestCounter state + 1}

instance (MonadIO m, MonadThrow m, HasBaseContext context, MonadReader context m) => RunClient (AgentClientT m) where
  runRequestAcceptStatus = runRequestAcceptStatus'
  throwClientError clientError = throwM clientError

runRequestAcceptStatus' :: (MonadIO m, MonadThrow m, HasBaseContext context, MonadReader context m) => Maybe [Status] -> Request -> (AgentClientT m) Response
runRequestAcceptStatus' acceptStatus request = do
  AgentClientState {_acsConfig = AgentClientConfig {..}, ..} <- getClientState

  let clientRequest = addHeaderRedaction _accSensitiveOutputHandling $ defaultMakeClientRequest _accBaseUrl request

  testFolder <- getCurrentFolder
  for_ testFolder $ HttpFile.writeRequest _accBaseUrl clientRequest _acsRequestCounter
  incrementRequestCounter

  clientResponse <- liftIO $ HttpClient.httpLbs clientRequest _accHttpManager
  for_ testFolder $ HttpFile.writeResponse clientResponse _acsRequestCounter

  let status = HttpClient.responseStatus clientResponse
  let response = clientResponseToResponse id clientResponse
  let goodStatus = case acceptStatus of
        Nothing -> statusIsSuccessful status
        Just good -> status `elem` good
  if goodStatus
    then pure $ response
    else throwClientError $ mkFailureResponse _accBaseUrl request response

getCapabilitiesGuarded :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m API.CapabilitiesResponse
getCapabilitiesGuarded = guardCapabilitiesResponse =<< (genericClient // API._capabilities)

guardCapabilitiesResponse :: MonadThrow m => Union API.CapabilitiesResponses -> m API.CapabilitiesResponse
guardCapabilitiesResponse = API.capabilitiesCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected CapabilitiesResponse"
    successAction c = pure c
    errorAction e = expectationFailure $ "Expected CapabilitiesResponse, got " <> show e

getHealth :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => Maybe API.SourceName -> Maybe API.Config -> AgentClientT m NoContent
getHealth sourceName config = (genericClient // API._health) sourceName config

getSchemaGuarded :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.SourceName -> API.Config -> AgentClientT m API.SchemaResponse
getSchemaGuarded sourceName config = guardSchemaResponse =<< (genericClient // API._schema) sourceName config

guardSchemaResponse :: MonadThrow m => Union API.SchemaResponses -> m API.SchemaResponse
guardSchemaResponse = API.schemaCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected SchemaResponse"
    successAction c = pure c
    errorAction e = expectationFailure $ "Expected SchemaResponse, got " <> show e

queryGuarded :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.SourceName -> API.Config -> API.QueryRequest -> AgentClientT m API.QueryResponse
queryGuarded sourceName config queryRequest = guardQueryResponse =<< (genericClient // API._query) sourceName config queryRequest

guardQueryResponse :: MonadThrow m => Union API.QueryResponses -> m API.QueryResponse
guardQueryResponse = API.queryCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected QueryResponse"
    successAction q = pure q
    errorAction e = expectationFailure $ "Expected QueryResponse, got " <> show e

queryExpectError :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.SourceName -> API.Config -> API.QueryRequest -> AgentClientT m API.ErrorResponse
queryExpectError sourceName config queryRequest = guardQueryErrorResponse =<< (genericClient // API._query) sourceName config queryRequest

guardQueryErrorResponse :: MonadThrow m => Union API.QueryResponses -> m API.ErrorResponse
guardQueryErrorResponse = API.queryCase defaultAction successAction errorAction
  where
    defaultAction = expectationFailure "Expected ErrorResponse"
    successAction q = expectationFailure $ "Expected ErrorResponse, got " <> show q
    errorAction e = pure e

explain :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => API.SourceName -> API.Config -> API.QueryRequest -> AgentClientT m API.ExplainResponse
explain sourceName config queryRequest = (genericClient // API._explain) sourceName config queryRequest

getMetrics :: (HasBaseContext context, MonadReader context m, MonadThrow m, MonadIO m) => AgentClientT m Text
getMetrics = genericClient // API._metrics
