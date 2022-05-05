module Main (main) where

import Command (AgentCapabilities (..), Command (..), TestOptions (..), parseCommandLine)
import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Data.Aeson.Text (encodeToLazyText)
import Data.Proxy (Proxy (..))
import Data.Text.Lazy.IO qualified as Text
import Hasura.Backends.DataConnector.API (Routes (..), apiClient, openApiSchemaJson)
import Hasura.Backends.DataConnector.API qualified as API
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (NamedRoutes)
import Servant.Client (Client, ClientError, hoistClient, mkClientEnv, runClientM, (//))
import Test.ConfigSchemaSpec qualified
import Test.Hspec (Spec)
import Test.Hspec.Core.Runner (runSpec)
import Test.Hspec.Core.Util (filterPredicate)
import Test.Hspec.Runner (Config (..), defaultConfig, evaluateSummary)
import Test.QuerySpec qualified
import Test.SchemaSpec qualified
import Prelude

tests :: Client IO (NamedRoutes Routes) -> API.Config -> API.Capabilities -> Spec
tests api agentConfig capabilities = do
  Test.ConfigSchemaSpec.spec api agentConfig
  Test.SchemaSpec.spec api agentConfig capabilities
  Test.QuerySpec.spec api agentConfig capabilities

main :: IO ()
main = do
  command <- parseCommandLine
  case command of
    Test testOptions@TestOptions {..} -> do
      api <- mkIOApiClient testOptions
      agentCapabilities <- getAgentCapabilities api _toAgentConfig _toAgentCapabilities
      runSpec (tests api _toAgentConfig agentCapabilities) (applyTestConfig defaultConfig testOptions) >>= evaluateSummary
    ExportOpenAPISpec ->
      Text.putStrLn $ encodeToLazyText openApiSchemaJson

  pure ()

mkIOApiClient :: TestOptions -> IO (Client IO (NamedRoutes Routes))
mkIOApiClient TestOptions {..} = do
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager _toAgentBaseUrl
  pure $ hoistClient (Proxy @(NamedRoutes Routes)) (flip runClientM clientEnv >=> throwClientError) apiClient

throwClientError :: Either ClientError a -> IO a
throwClientError = either throwIO pure

getAgentCapabilities :: Client IO (NamedRoutes Routes) -> API.Config -> AgentCapabilities -> IO API.Capabilities
getAgentCapabilities api agentConfig = \case
  AutoDetect -> fmap API.srCapabilities $ api // _schema $ agentConfig
  Explicit capabilities -> pure capabilities

applyTestConfig :: Config -> TestOptions -> Config
applyTestConfig config TestOptions {..} =
  config
    { configConcurrentJobs = _toParallelDegree,
      configFilterPredicate = filterPredicate <$> _toMatch,
      configSkipPredicate = filterPredicate <$> _toSkip
    }
