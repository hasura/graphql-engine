module Main (main) where

import Command (AgentCapabilities (..), Command (..), TestOptions (..), parseCommandLine)
import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Data.Aeson.Text (encodeToLazyText)
import Data.Proxy (Proxy (..))
import Data.Text.Lazy.IO qualified as Text
import Hasura.Backends.DataWrapper.API (Routes (..), apiClient, openApiSchema)
import Hasura.Backends.DataWrapper.API qualified as API
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (NamedRoutes)
import Servant.Client (Client, ClientError, hoistClient, mkClientEnv, runClientM, (//))
import Test.Hspec (Spec)
import Test.Hspec.Core.Runner (runSpec)
import Test.Hspec.Core.Util (filterPredicate)
import Test.Hspec.Runner (Config (..), defaultConfig, evaluateSummary)
import Test.QuerySpec qualified
import Test.SchemaSpec qualified
import Prelude

tests :: Client IO (NamedRoutes Routes) -> API.Capabilities -> Spec
tests api capabilities = do
  Test.SchemaSpec.spec api capabilities
  Test.QuerySpec.spec api capabilities

main :: IO ()
main = do
  command <- parseCommandLine
  case command of
    Test testOptions -> do
      api <- mkIOApiClient testOptions
      agentCapabilities <- getAgentCapabilities api (_toAgentCapabilities testOptions)
      runSpec (tests api agentCapabilities) (applyTestConfig defaultConfig testOptions) >>= evaluateSummary
    ExportOpenAPISpec ->
      Text.putStrLn $ encodeToLazyText openApiSchema

  pure ()

mkIOApiClient :: TestOptions -> IO (Client IO (NamedRoutes Routes))
mkIOApiClient TestOptions {..} = do
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager _toAgentBaseUrl
  pure $ hoistClient (Proxy @(NamedRoutes Routes)) (flip runClientM clientEnv >=> throwClientError) apiClient

throwClientError :: Either ClientError a -> IO a
throwClientError = either throwIO pure

getAgentCapabilities :: Client IO (NamedRoutes Routes) -> AgentCapabilities -> IO API.Capabilities
getAgentCapabilities api = \case
  AutoDetect -> fmap API.srCapabilities $ api // _schema
  Explicit capabilities -> pure capabilities

applyTestConfig :: Config -> TestOptions -> Config
applyTestConfig config TestOptions {..} =
  config
    { configConcurrentJobs = _toParallelDegree,
      configFilterPredicate = filterPredicate <$> _toMatch,
      configSkipPredicate = filterPredicate <$> _toSkip
    }
