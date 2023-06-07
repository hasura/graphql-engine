module Harness.DataConnectorAgent
  ( createClone,
    createClone',
    deleteClone,
    deleteClone',
    createManagedClone,
  )
where

import Control.Exception.Lifted (finally, throwIO)
import Control.Monad.Managed (MonadManaged, managed)
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Network.HTTP.Client qualified as Http
import Servant (NamedRoutes, Proxy (..))
import Servant.Client (Client, ClientM, client, mkClientEnv, parseBaseUrl, runClientM, (//))

runDataConnectorAgentRequests :: String -> (Client ClientM (NamedRoutes API.Routes) -> ClientM a) -> IO a
runDataConnectorAgentRequests agentUri runWithClient = do
  clientEnv <- mkClientEnv <$> Http.newManager Http.defaultManagerSettings <*> parseBaseUrl agentUri
  results <- flip runClientM clientEnv $ do
    runWithClient $ client (Proxy @(NamedRoutes API.Routes))
  results `onLeft` throwIO

createClone :: String -> TestEnvironment -> API.DatasetTemplateName -> IO API.DatasetCreateCloneResponse
createClone agentUri TestEnvironment {..} templateName =
  createClone' agentUri (API.DatasetCloneName $ tshow uniqueTestId) templateName

createClone' :: String -> API.DatasetCloneName -> API.DatasetTemplateName -> IO API.DatasetCreateCloneResponse
createClone' agentUri cloneName templateName = runDataConnectorAgentRequests agentUri $ \dcClient -> do
  (dcClient // API._datasets // API._createClone) cloneName (API.DatasetCreateCloneRequest templateName)

deleteClone :: String -> TestEnvironment -> IO API.DatasetDeleteCloneResponse
deleteClone agentUri TestEnvironment {..} =
  deleteClone' agentUri (API.DatasetCloneName $ tshow uniqueTestId)

deleteClone' :: String -> API.DatasetCloneName -> IO API.DatasetDeleteCloneResponse
deleteClone' agentUri cloneName = runDataConnectorAgentRequests agentUri $ \dcClient -> do
  (dcClient // API._datasets // API._deleteClone) cloneName

withClone :: String -> TestEnvironment -> API.DatasetTemplateName -> (API.DatasetCreateCloneResponse -> IO a) -> IO a
withClone agentUri TestEnvironment {..} templateName useClone = runDataConnectorAgentRequests agentUri $ \dcClient -> do
  let cloneName = API.DatasetCloneName (tshow uniqueTestId)
  response <- (dcClient // API._datasets // API._createClone) cloneName (API.DatasetCreateCloneRequest templateName)
  finally
    (liftIO $ useClone response)
    ((dcClient // API._datasets // API._deleteClone) cloneName)

createManagedClone :: (MonadManaged m) => String -> TestEnvironment -> API.DatasetTemplateName -> m API.DatasetCreateCloneResponse
createManagedClone agentUri testEnvironment templateName = managed (withClone agentUri testEnvironment templateName)
